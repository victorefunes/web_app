# =============================================================================
# app.R  --  Corn Terminal-Crop Profit Explorer
#
# Displays simulated profit ($/ac) for corn as the terminal crop, at field and
# county level, compared across the number of crops in the rotation.
#
#   Rscript precompute.R      # once
#   shiny::runApp("app.R")
#
# TABS
#   Map        county choropleth; two panels compare two ncrops scenarios
#   Rotation   how profit responds to rotation length (the headline result)
#   Fields     field-level detail within a chosen county
# =============================================================================

library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)
library(leaflet)

setwd("C:/Users/vf006/Box/premiums/NPV_sims/risk_model/app/web_app")
rsconnect::writeManifest()

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# 1. Load precomputed metrics
# ---------------------------------------------------------------------------
for (f in c("county_profit.rds", "tile_profit.rds"))
  if (!file.exists(f)) stop("'", f, "' not found. Run precompute.R first.")

county <- as.data.table(readRDS("county_profit.rds"))
tiles  <- as.data.table(readRDS("tile_profit.rds"))

ALL_NCROPS <- sort(unique(county$ncrops))

# Field coordinates (written by precompute.R when FIELD_GEOM is set).
HAS_COORDS <- all(c("lon", "lat") %in% names(tiles)) &&
              any(!is.na(tiles$lon))

# Rendering ~270k points: plain addCircleMarkers builds one DOM/SVG element per
# point and will lock the browser. leafgl draws them on the GPU in one pass and
# handles hundreds of thousands comfortably. If leafgl isn't installed we fall
# back to canvas-rendered circle markers and cap the point count.
HAS_LEAFGL <- requireNamespace("leafgl", quietly = TRUE)
if (HAS_LEAFGL) library(leafgl)

MAX_POINTS_FALLBACK <- 20000   # only used when leafgl is unavailable

if (HAS_COORDS && !HAS_LEAFGL)
  message("NOTE: install.packages('leafgl') for fast rendering of all fields. ",
          "Falling back to a ", format(MAX_POINTS_FALLBACK, big.mark = ","),
          "-point sample.")

# ---------------------------------------------------------------------------
# 2. Metric registry -- profit only
#    good_high: TRUE  -> more is better (green/viridis)
#               FALSE -> more is worse  (red/magma reversed)
# ---------------------------------------------------------------------------
METRICS <- list(
  "Mean profit"          = list(key = "mean_profit",   good_high = TRUE,  fmt = "usd"),
  "Median profit"        = list(key = "median_profit", good_high = TRUE,  fmt = "usd"),
  "P(profit < 0)"        = list(key = "p_loss",        good_high = FALSE, fmt = "pct"),
  "Profit volatility (SD)" = list(key = "sd_profit",   good_high = FALSE, fmt = "usd"),
  "10th pct profit"      = list(key = "p10_profit",    good_high = TRUE,  fmt = "usd"),
  "90th pct profit"      = list(key = "p90_profit",    good_high = TRUE,  fmt = "usd")
)
METRICS <- METRICS[vapply(METRICS, function(m) m$key %in% names(county), logical(1))]

fmt_val <- function(v, type) {
  ifelse(is.na(v), "\u2013",
    switch(type, pct = sprintf("%.1f%%", 100 * v), usd = sprintf("$%.0f", v),
           sprintf("%.2f", v)))
}

# ---------------------------------------------------------------------------
# 3. Geometry -- pre-saved by precompute.R.
#
# Deliberately NOT calling tigris here: hitting the Census API on every app
# launch is slow and fragile, and impossible under shinylive/webR. The .rds is
# small and makes the app self-contained and portable to any host.
# ---------------------------------------------------------------------------
if (!file.exists("counties_sf.rds"))
  stop("counties_sf.rds not found. Run precompute.R first.")

counties_sf <- readRDS("counties_sf.rds")

county <- merge(county, as.data.table(st_drop_geometry(counties_sf)),
                by = "fips", all.x = TRUE)
COUNTY_CHOICES <- setNames(counties_sf$fips, counties_sf$county_name)
COUNTY_CHOICES <- COUNTY_CHOICES[COUNTY_CHOICES %in% unique(tiles$fips)]
COUNTY_CHOICES <- COUNTY_CHOICES[order(names(COUNTY_CHOICES))]

# ---------------------------------------------------------------------------
# 4. UI
# ---------------------------------------------------------------------------
ui <- page_navbar(
  title = "Corn Profit Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c7a4b",
                   base_font = font_google("Public Sans")),
  header = tags$style(HTML(
    ".map-box{height:calc(100vh - 300px);min-height:360px;}
     .leaflet-container{background:#f8f9fa;}
     .leaflet-tooltip{white-space:normal !important;max-width:none !important;}")),

  # ---- Tab 1: county maps, two scenarios side by side --------------------
  nav_panel(
    "Map",
    div(class = "px-3 pt-2",
        layout_columns(
          col_widths = c(8, 4),
          p(class = "text-muted small mb-1",
            "Corn as terminal crop. County choropleth = mean across fields. Both panels share a color scale."),
          if (HAS_COORDS)
            checkboxInput("show_fields", "Show individual fields", value = FALSE)
          else
            p(class = "text-muted small mb-1 fst-italic",
              "No field coordinates loaded \u2014 set FIELD_GEOM in precompute.R."))),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(uiOutput("l_hdr")),
        div(class = "px-3 pt-2", layout_columns(
          col_widths = c(7, 5),
          selectInput("l_metric", "Metric", names(METRICS)),
          selectInput("l_ncrops", "Crops in rotation", ALL_NCROPS,
                      selected = min(ALL_NCROPS)))),
        div(class = "map-box", leafletOutput("l_map", height = "100%")),
        card_footer(uiOutput("l_stats"))),
      card(
        card_header(uiOutput("r_hdr")),
        div(class = "px-3 pt-2", layout_columns(
          col_widths = c(7, 5),
          selectInput("r_metric", "Metric", names(METRICS)),
          selectInput("r_ncrops", "Crops in rotation", ALL_NCROPS,
                      selected = max(ALL_NCROPS)))),
        div(class = "map-box", leafletOutput("r_map", height = "100%")),
        card_footer(uiOutput("r_stats"))))
  ),

  # ---- Tab 2: the rotation effect ----------------------------------------
  nav_panel(
    "Rotation effect",
    div(class = "px-3 pt-2",
        p(class = "text-muted small mb-2",
          "How corn profit responds to rotation length. Left: distribution of field-level profit at each rotation length. Right: change relative to continuous corn (ncrops = 1), by county.")),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Profit distribution by rotation length"),
           plotOutput("dist_plot", height = "440px")),
      card(card_header("Change vs. continuous corn"),
           plotOutput("delta_plot", height = "440px")))
  ),

  # ---- Tab 3: fields within a county -------------------------------------
  nav_panel(
    "Fields",
    div(class = "px-3 pt-2",
        layout_columns(
          col_widths = c(3, 3, 6),
          selectInput("f_county", "County", COUNTY_CHOICES),
          selectInput("f_metric", "Metric", names(METRICS)),
          div())),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(if (HAS_COORDS) "Field map" else "Field distribution"),
           if (HAS_COORDS) div(class = "map-box", leafletOutput("f_map", height = "100%"))
           else plotOutput("f_hist", height = "420px")),
      card(card_header("Fields by rotation length"),
           plotOutput("f_box", height = "420px"))),
    div(class = "px-3 pb-3", uiOutput("f_note"))
  ),

  nav_spacer(),
  nav_item(downloadButton("dl", "Export", class = "btn-sm btn-outline-secondary"))
)

# ---------------------------------------------------------------------------
# 5. SERVER
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Map tab ----------------------------------------------------------
  side <- function(s) reactive({
    m <- input[[paste0(s, "_metric")]]; nc <- as.integer(input[[paste0(s, "_ncrops")]])
    req(m, nc)
    spec <- METRICS[[m]]
    d <- copy(county[ncrops == nc])[, value := get(spec$key)]
    list(sf = left_join(counties_sf, d, by = "fips"),
         metric = m, key = spec$key, fmt = spec$fmt,
         good_high = spec$good_high, ncrops = nc)
  })
  L <- side("l"); R <- side("r")

  # Shared color domain across panels. When fields are shown, the domain must
  # cover the FIELD value range too -- fields are far more dispersed than
  # county means, so a scale built only on county means would clip most points
  # to the extreme colors and destroy the contrast.
  shared <- reactive({
    a <- L(); b <- R()
    if (!identical(a$key, b$key)) return(NULL)

    v <- c(a$sf$value, b$sf$value)

    if (isTRUE(input$show_fields) && HAS_COORDS) {
      # Robust limits: field distributions have long tails (a handful of fields
      # with extreme profit would stretch the scale and flatten everything
      # else). Clip to the 1st/99th percentile of field values.
      fv <- tiles[ncrops %in% c(a$ncrops, b$ncrops)][[a$key]]
      v <- c(v, quantile(fv, c(0.01, 0.99), na.rm = TRUE))
    }
    range(v, na.rm = TRUE)
  })

  hover <- function(d) lapply(seq_len(nrow(d$sf)), function(i) {
    r <- d$sf[i, ]
    if (is.na(r$n_fields))
      return(htmltools::HTML(sprintf("<b>%s County</b><br><span style='color:#999'>No data</span>",
                                     r$county_name)))
    rows <- vapply(names(METRICS), function(mn) {
      sp <- METRICS[[mn]]
      sprintf("<tr style='%s'><td style='padding:1px 8px 1px 0'>%s%s</td>
               <td style='text-align:right'><b>%s</b></td></tr>",
              if (identical(sp$key, d$key)) "background:#eef6f1" else "",
              if (identical(sp$key, d$key)) "&#9656; " else "", mn,
              fmt_val(r[[sp$key]], sp$fmt))
    }, character(1))
    htmltools::HTML(sprintf(
      "<div style='font-family:system-ui;font-size:12px;min-width:220px'>
         <div style='font-weight:600;font-size:13px'>%s County</div>
         <div style='color:#666;font-size:11px;margin-bottom:4px'>%d-crop rotation &middot; %s fields</div>
         <table style='width:100%%;border-collapse:collapse'>%s</table></div>",
      r$county_name, d$ncrops, format(r$n_fields, big.mark = ","),
      paste(rows, collapse = "")))
  })

  # Field points for a given scenario, colored on the SAME scale as the
  # choropleth beneath them -- so a field visibly darker than its county is
  # genuinely worse than its county average, not just differently scaled.
  field_pts <- function(nc, key) {
    if (!HAS_COORDS) return(NULL)
    d <- tiles[ncrops == nc & !is.na(lon) & !is.na(lat),
               c("tile_field_id", "fips", "lon", "lat", key), with = FALSE]
    setnames(d, key, "value")
    d[!is.na(value)]
  }

  draw_map <- function(d_r, side_id) {
    d <- d_r()
    dom <- shared() %||% range(d$sf$value, na.rm = TRUE)
    pal <- colorNumeric(if (d$good_high) "viridis" else "magma",
                        domain = dom, na.color = "#e0e0e0", reverse = !d$good_high)

    m <- leaflet(d$sf, options = leafletOptions(attributionControl = FALSE,
                                                preferCanvas = TRUE)) |>
      addPolygons(
        fillColor = ~pal(value),
        # When fields are shown, fade the choropleth to a light basemap so the
        # points read clearly on top of it rather than fighting for attention.
        fillOpacity = if (isTRUE(input$show_fields)) 0.25 else 0.9,
        weight = 0.6, color = "white", smoothFactor = 0.3,
        highlightOptions = highlightOptions(weight = 2, color = "#333",
                                            bringToFront = FALSE),
        label = hover(d),
        labelOptions = labelOptions(direction = "auto", sticky = TRUE,
          style = list(padding = "8px 10px", "border-radius" = "4px",
                       "box-shadow" = "0 2px 8px rgba(0,0,0,.15)")))

    if (isTRUE(input$show_fields) && HAS_COORDS) {
      pts <- field_pts(d$ncrops, d$key)

      if (!is.null(pts) && nrow(pts) > 0) {
        if (HAS_LEAFGL) {
          # GPU-rendered: handles the full ~270k fields in one pass.
          pts_sf <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
          m <- m |>
            addGlPoints(data = pts_sf, fillColor = pal(pts$value),
                        radius = 4, fillOpacity = 0.85,
                        popup = ~sprintf("%s<br>%s: %s", tile_field_id,
                                         d$metric, fmt_val(value, d$fmt)))
        } else {
          # Fallback: canvas markers on a random sample, so the browser survives.
          if (nrow(pts) > MAX_POINTS_FALLBACK)
            pts <- pts[sample(.N, MAX_POINTS_FALLBACK)]
          m <- m |>
            addCircleMarkers(data = pts, lng = ~lon, lat = ~lat,
              radius = 2.5, stroke = FALSE, fillOpacity = 0.8,
              fillColor = ~pal(value),
              label = ~sprintf("%s | %s", tile_field_id, fmt_val(value, d$fmt)))
        }
      }
    }

    m |> addLegend(pal = pal, values = dom, position = "bottomright",
                   title = d$metric, opacity = .9,
                   labFormat = if (d$fmt == "pct")
                     labelFormat(suffix = "%", transform = function(x) 100 * x)
                   else labelFormat(prefix = "$"))
  }
  output$l_map <- renderLeaflet(draw_map(L, "l"))
  output$r_map <- renderLeaflet(draw_map(R, "r"))

  output$l_hdr <- renderUI(HTML(sprintf("%s &middot; %d-crop", L()$metric, L()$ncrops)))
  output$r_hdr <- renderUI(HTML(sprintf("%s &middot; %d-crop", R()$metric, R()$ncrops)))

  stat_box <- function(d_r) renderUI({
    d <- d_r(); v <- d$sf$value
    HTML(sprintf("Mean <b>%s</b> &nbsp;|&nbsp; %s to %s &nbsp;|&nbsp; %d counties",
                 fmt_val(mean(v, na.rm = TRUE), d$fmt),
                 fmt_val(min(v, na.rm = TRUE), d$fmt),
                 fmt_val(max(v, na.rm = TRUE), d$fmt), sum(!is.na(v))))
  })
  output$l_stats <- stat_box(L); output$r_stats <- stat_box(R)

  # ---- Rotation-effect tab ----------------------------------------------
  # Distribution of FIELD-level mean profit at each rotation length.
  output$dist_plot <- renderPlot({
    ggplot(tiles, aes(x = mean_profit, fill = factor(ncrops))) +
      geom_density(alpha = 0.45, colour = NA) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey30") +
      scale_fill_viridis_d(name = "Crops in\nrotation", option = "D", end = 0.85) +
      labs(x = "Field mean profit ($/ac)", y = "Density",
           subtitle = "Dashed line = breakeven. Mass left of it is fields losing money.") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  # County-level change relative to continuous corn (ncrops = 1).
  # This is the comparison the rotation literature cares about, and the one
  # a two-panel map only shows indirectly.
  output$delta_plot <- renderPlot({
    base <- county[ncrops == min(ALL_NCROPS), .(fips, base = mean_profit)]
    d <- merge(county[ncrops > min(ALL_NCROPS)], base, by = "fips")
    d[, delta := mean_profit - base]

    ggplot(d, aes(x = factor(ncrops), y = delta)) +
      geom_hline(yintercept = 0, colour = "grey40") +
      geom_boxplot(aes(fill = factor(ncrops)), width = .55, outlier.size = .6,
                   outlier.alpha = .35, colour = "grey25") +
      scale_fill_viridis_d(option = "D", end = .85, guide = "none") +
      labs(x = "Crops in rotation", y = sprintf("Profit change vs. %d-crop ($/ac)",
                                                min(ALL_NCROPS)),
           subtitle = "Each point is a county. Above zero = rotation improves corn profit.") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })

  # ---- Fields tab -------------------------------------------------------
  f_tiles <- reactive({
    req(input$f_county)
    tiles[fips == as.integer(input$f_county)]
  })

  output$f_hist <- renderPlot({
    d <- f_tiles(); spec <- METRICS[[input$f_metric]]
    ggplot(d, aes(x = .data[[spec$key]], fill = factor(ncrops))) +
      geom_histogram(bins = 40, alpha = .55, position = "identity", colour = NA) +
      scale_fill_viridis_d(name = "Crops in\nrotation", option = "D", end = .85) +
      labs(x = input$f_metric, y = "Fields",
           subtitle = sprintf("%s fields in this county",
                              format(uniqueN(d$tile_field_id), big.mark = ","))) +
      theme_minimal(base_size = 13)
  })

  output$f_box <- renderPlot({
    d <- f_tiles(); spec <- METRICS[[input$f_metric]]
    ggplot(d, aes(x = factor(ncrops), y = .data[[spec$key]], fill = factor(ncrops))) +
      geom_boxplot(width = .55, outlier.size = .5, outlier.alpha = .3, colour = "grey25") +
      { if (spec$key %in% c("mean_profit", "median_profit", "p10_profit", "p90_profit"))
          geom_hline(yintercept = 0, linetype = 2, colour = "grey40") } +
      scale_fill_viridis_d(option = "D", end = .85, guide = "none") +
      labs(x = "Crops in rotation", y = input$f_metric,
           subtitle = "Each box = distribution across fields in this county") +
      theme_minimal(base_size = 13)
  })

  if (HAS_COORDS) output$f_map <- renderLeaflet({
    d <- f_tiles(); spec <- METRICS[[input$f_metric]]
    d <- d[!is.na(lon) & !is.na(lat)]
    validate(need(nrow(d) > 0, "No field coordinates for this county."))

    pal <- colorNumeric(if (spec$good_high) "viridis" else "magma",
                        domain = d[[spec$key]], reverse = !spec$good_high)

    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles("CartoDB.Positron")

    # One county's fields is a much smaller set than the statewide layer, so
    # regular markers are fine here -- they also give hover labels, which the
    # GL layer doesn't.
    m |>
      addCircleMarkers(data = d, lng = ~lon, lat = ~lat,
        radius = 4, stroke = FALSE, fillOpacity = .85,
        fillColor = ~pal(get(spec$key)),
        label = ~sprintf("%s | %d-crop | %s", tile_field_id, ncrops,
                         fmt_val(get(spec$key), spec$fmt))) |>
      addLegend(pal = pal, values = d[[spec$key]], title = input$f_metric,
                position = "bottomright")
  })

  output$f_note <- renderUI({
    d <- f_tiles()
    nd <- mean(d$n_draws)
    p  <- mean(d$p_loss, na.rm = TRUE)
    se <- sqrt(p * (1 - p) / nd)
    HTML(sprintf(
      "<div class='text-muted small'>%s fields &middot; %.0f draws each.
       At this draw count a single field's P(profit&lt;0) carries a Monte Carlo
       standard error of about %.1f pp, so treat individual-field values as
       indicative; county means are far more stable.%s</div>",
      format(uniqueN(d$tile_field_id), big.mark = ","), nd, 100 * se,
      if (!HAS_COORDS)
        " Add a tile_field_ID/lon/lat file to precompute.R (FIELD_COORDS) to enable a field map."
      else ""))
  })

  # ---- Export -----------------------------------------------------------
  output$dl <- downloadHandler(
    filename = function() sprintf("corn_profit_%s.csv", Sys.Date()),
    content = function(f) fwrite(county, f))
}

shinyApp(ui, server)
