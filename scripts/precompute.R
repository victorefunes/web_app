# =============================================================================
# precompute.R  --  corn terminal-crop profit, by field and county, across ncrops
#
# Streams ~6GB of Monte Carlo draws through DuckDB and reduces them to two
# small tables. The draws never enter R memory.
#
#   Rscript precompute.R
#
# INPUT (long; one row per tile x draw):
#   tile_field_ID, fips, ncrops, draw      <- `draw` IS PROFIT ($/ac)
#
# OUTPUT:
#   tile_profit.rds     one row per (tile, ncrops)   -- field level
#   county_profit.rds   one row per (fips, ncrops)   -- county level
# =============================================================================

library(DBI)
library(duckdb)
library(data.table)

setwd("C:/Users/vf006/Box/premiums/NPV_sims/risk_model/app/web_app")

# ---------------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------------
INPUT_GLOB <- "D:/Crop data/simulations/corn_sim.csv"     # all corn terminal-crop draw files
OUT_TILE   <- "tile_profit.rds"
OUT_COUNTY <- "county_profit.rds"

MEMORY_LIMIT <- "8GB"
THREADS      <- 4

# Field geometry -- REQUIRED for field points on the maps.
# Either:
#   a) a CSV with columns: tile_field_ID, lon, lat
#   b) a spatial file (.shp/.gpkg/.geojson) of tile polygons or points; the
#      centroid of each feature is used, and it must carry a tile_field_ID
#      column (set FIELD_ID_COL if it's named something else).
# Set to NULL to skip field points entirely.
FIELD_GEOM    <- "D:/Crop data/simulations/field_locations.csv"   # or "tiles.gpkg" / "tiles.shp"
FIELD_ID_COL  <- "tile_field_ID"     # the ID column inside FIELD_GEOM

# ---------------------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = "scratch.duckdb"))
on.exit({ dbDisconnect(con, shutdown = TRUE); unlink("scratch.duckdb") })
dbExecute(con, sprintf("SET memory_limit='%s'", MEMORY_LIMIT))
dbExecute(con, sprintf("SET threads=%d", THREADS))

dbExecute(con, sprintf("
  CREATE OR REPLACE VIEW draws AS
  SELECT * FROM read_csv_auto('%s', union_by_name=true)", INPUT_GLOB))

n <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM draws")$n
message("Draws: ", format(n, big.mark = ","))

# ---------------------------------------------------------------------------
# FIELD-LEVEL PROFIT METRICS
#
# `draw` is profit, so everything is a direct summary of the draws -- no
# thresholds, no reference distribution, no interpolation.
#
#   mean_profit   E[profit]              -- the headline number
#   p_loss        P(profit < 0)          -- exact; field fails to cover costs
#   sd_profit     dispersion across draws (risk)
#   p10/p50/p90   quantiles of the profit distribution
#   cv            coefficient of variation (risk per dollar of profit)
# ---------------------------------------------------------------------------
message("Field-level metrics...")
print(system.time(dbExecute(con, "
  CREATE OR REPLACE TABLE tile_profit AS
  SELECT
    tile_field_ID,
    fips,
    ncrops,
    AVG(draw)                                  AS mean_profit,
    MEDIAN(draw)                               AS median_profit,
    STDDEV(draw)                               AS sd_profit,
    QUANTILE_CONT(draw, 0.10)                  AS p10_profit,
    QUANTILE_CONT(draw, 0.90)                  AS p90_profit,
    AVG(CASE WHEN draw < 0 THEN 1.0 ELSE 0.0 END) AS p_loss,
    COUNT(*)                                   AS n_draws
  FROM draws
  GROUP BY tile_field_ID, fips, ncrops
")))

tile_profit <- as.data.table(dbGetQuery(con, "SELECT * FROM tile_profit"))
setnames(tile_profit, tolower(names(tile_profit)))
tile_profit[, cv := fifelse(mean_profit > 0, sd_profit / mean_profit, NA_real_)]

message("Fields x scenarios: ", format(nrow(tile_profit), big.mark = ","))

# ---------------------------------------------------------------------------
# Attach field coordinates (enables field points on the maps)
# ---------------------------------------------------------------------------
if (!is.null(FIELD_GEOM)) {
  if (!file.exists(FIELD_GEOM)) stop("FIELD_GEOM not found: ", FIELD_GEOM)

  ext <- tolower(tools::file_ext(FIELD_GEOM))

  coords <- if (ext == "csv") {
    cd <- fread(FIELD_GEOM)
    setnames(cd, tolower(names(cd)))
    setnames(cd, tolower(FIELD_ID_COL), "tile_field_id", skip_absent = TRUE)
    stopifnot(all(c("tile_field_id", "lon", "lat") %in% names(cd)))
    cd[, .(tile_field_id, lon, lat)]

  } else {
    # Spatial file: take centroids, reproject to WGS84 for leaflet
    library(sf)
    g <- st_read(FIELD_GEOM, quiet = TRUE)
    names(g) <- tolower(names(g))
    idc <- tolower(FIELD_ID_COL)
    if (!idc %in% names(g))
      stop("Column '", idc, "' not found in ", FIELD_GEOM,
           ". Found: ", paste(names(g), collapse = ", "))

    # suppressWarnings: centroids of lon/lat polygons trigger a planar warning;
    # for placing a marker that's fine.
    ctr <- suppressWarnings(st_centroid(st_geometry(g)))
    ctr <- st_transform(st_sf(geometry = ctr), 4326)
    xy  <- st_coordinates(ctr)

    data.table(tile_field_id = as.character(g[[idc]]),
               lon = xy[, 1], lat = xy[, 2])
  }

  coords <- unique(coords, by = "tile_field_id")
  tile_profit[, tile_field_id := as.character(tile_field_id)]
  tile_profit <- merge(tile_profit, coords, by = "tile_field_id", all.x = TRUE)

  n_matched <- sum(!is.na(tile_profit$lon))
  message(sprintf("Coordinates matched for %s of %s field-scenarios (%.1f%%).",
                  format(n_matched, big.mark = ","),
                  format(nrow(tile_profit), big.mark = ","),
                  100 * n_matched / nrow(tile_profit)))
  if (n_matched == 0)
    warning("No coordinates matched -- check that IDs in FIELD_GEOM match tile_field_ID exactly.")

  # Also save a compact one-row-per-field lookup for the app's point layer,
  # so it doesn't have to carry coordinates on every scenario row.
  field_coords <- unique(tile_profit[!is.na(lon), .(tile_field_id, fips, lon, lat)],
                         by = "tile_field_id")
  saveRDS(field_coords, "field_coords.rds")
  message("Wrote field_coords.rds: ", format(nrow(field_coords), big.mark = ","), " fields")
}

saveRDS(tile_profit, OUT_TILE)

# ---------------------------------------------------------------------------
# COUNTY-LEVEL AGGREGATION
#
# Mean of each field metric across fields in the county, plus the ACROSS-FIELD
# SD -- which is the interesting part: it's within-county heterogeneity, i.e.
# how much fields in the same county differ from each other. A county whose
# mean profit is $70 with fields spanning -$50 to $200 is a very different
# place from one where every field sits near $70.
# ---------------------------------------------------------------------------
message("County aggregation...")

metric_cols <- c("mean_profit", "median_profit", "sd_profit",
                 "p10_profit", "p90_profit", "p_loss")

county_profit <- tile_profit[, c(
  lapply(.SD, mean, na.rm = TRUE),
  setNames(lapply(.SD, sd, na.rm = TRUE), paste0(metric_cols, "_between")),
  list(n_fields = .N)
), by = .(fips, ncrops), .SDcols = metric_cols]

setorder(county_profit, fips, ncrops)
saveRDS(county_profit, OUT_COUNTY)

# ---------------------------------------------------------------------------
# DEPLOYMENT SIZE CHECK
#
# These .rds files get committed to git for Posit Connect Cloud. GitHub rejects
# files >100MB, and Connect's git-backed publishing does NOT support Git LFS --
# so anything oversized has to be trimmed here, not worked around later.
#
# Trims applied:
#   - drop columns the app never reads
#   - xz compression (slower to write, much smaller)
#   - round floats: these are Monte Carlo estimates with ~1-5pp of simulation
#     error, so storing 15 significant digits is noise. Rounding costs nothing
#     real and compresses far better.
# ---------------------------------------------------------------------------
app_cols <- c("tile_field_id", "fips", "ncrops", "mean_profit", "median_profit",
              "sd_profit", "p10_profit", "p90_profit", "p_loss", "n_draws",
              if (all(c("lon", "lat") %in% names(tile_profit))) c("lon", "lat"))

tile_app <- tile_profit[, intersect(app_cols, names(tile_profit)), with = FALSE]

num_cols <- names(tile_app)[vapply(tile_app, is.numeric, logical(1))]
num_cols <- setdiff(num_cols, c("fips", "ncrops", "n_draws", "lon", "lat"))
tile_app[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]
if ("lon" %in% names(tile_app))
  tile_app[, `:=`(lon = round(lon, 5), lat = round(lat, 5))]   # ~1m precision

saveRDS(tile_app, OUT_TILE, compress = "xz")

sizes <- sapply(c(OUT_COUNTY, OUT_TILE, "counties_sf.rds"),
                function(f) if (file.exists(f)) file.size(f) / 1024^2 else NA)

message("\n--- Deployment file sizes (MB) ---")
print(round(sizes, 2))

if (any(sizes > 90, na.rm = TRUE)) {
  warning("A file is near GitHub's 100MB limit. Options:\n",
          "  - drop the Fields tab and ship only county_profit.rds\n",
          "  - keep tile data for a subset of counties\n",
          "  - store tiles as Parquet and read with arrow")
}

message("Counties x scenarios: ", nrow(county_profit))
message("ncrops levels: ", paste(sort(unique(county_profit$ncrops)), collapse = ", "))

# ---------------------------------------------------------------------------
# COUNTY GEOMETRY -> RDS
#
# Fetch the boundaries ONCE here rather than in the app. Calling tigris at app
# startup hits the Census API on every launch: slow, fragile, and impossible
# under shinylive (webR has no system GDAL and would be blocked by CORS anyway).
# Baking the geometry into a small .rds makes the app self-contained.
#
# st_simplify shrinks the file a lot with no visible difference at county zoom
# levels -- worth it if you're shipping this to a browser.
# ---------------------------------------------------------------------------
library(sf)
options(tigris_use_cache = TRUE)

state_fips <- unique(substr(sprintf("%05d", county_profit$fips), 1, 2))
message("Fetching county geometry for state FIPS: ", paste(state_fips, collapse = ", "))

counties_sf <- tigris::counties(state = state_fips, cb = TRUE, year = 2022) |>
  st_as_sf() |>
  transform(fips = as.integer(GEOID)) |>
  st_transform(4326) |>
  subset(select = c(fips, NAME, geometry))
names(counties_sf)[names(counties_sf) == "NAME"] <- "county_name"

# Simplify: ~10m tolerance is invisible at county scale but cuts size sharply
counties_sf <- st_simplify(counties_sf, dTolerance = 100, preserveTopology = TRUE)

saveRDS(counties_sf, "counties_sf.rds")
message("Wrote counties_sf.rds (", round(file.size("counties_sf.rds") / 1024), " KB)")

# ---------------------------------------------------------------------------
# SUMMARY: the headline rotation effect
# ---------------------------------------------------------------------------
message("\n--- Mean profit by number of crops in rotation ---")
print(tile_profit[, .(
  fields      = .N,
  mean_profit = round(mean(mean_profit), 1),
  median      = round(median(mean_profit), 1),
  p_loss      = round(mean(p_loss), 3),
  sd_within   = round(mean(sd_profit), 1)
), by = ncrops][order(ncrops)])

# ---------------------------------------------------------------------------
# Monte Carlo precision note
# At n draws per field, P(profit<0) has binomial SE = sqrt(p(1-p)/n). With
# n=100 and p~0.3 that's ~4.6pp at the FIELD level -- visible as speckle on a
# field-level map. County means over many fields shrink it by ~sqrt(n_fields).
# ---------------------------------------------------------------------------
nd <- mean(tile_profit$n_draws)
pl <- mean(tile_profit$p_loss)
message(sprintf("\nP(loss) = %.3f | field-level SE at n=%.0f draws: %.3f",
                pl, nd, sqrt(pl * (1 - pl) / nd)))
message(sprintf("avg fields/county = %.0f -> county-mean SE ~%.4f",
                mean(county_profit$n_fields),
                sqrt(pl * (1 - pl) / nd) / sqrt(mean(county_profit$n_fields))))
