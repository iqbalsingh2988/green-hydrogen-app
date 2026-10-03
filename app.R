library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. STATIC DATA (Guaranteed non-empty)
# ------------------------------------------------------------------------------
states_data <- data.frame(
  ST_NM = c("Gujarat", "Rajasthan", "Maharashtra", "Uttarakhand", "Madhya Pradesh", "Odisha", "Karnataka"),
  lon = c(71.19, 74.21, 75.71, 79.01, 78.65, 84.80, 75.71),
  lat = c(22.25, 27.02, 19.75, 30.06, 22.97, 20.95, 15.31),
  Mean_FA_Ratio = c(0.328, 0.315, 0.305, 0.342, 0.285, 0.322, 0.292),
  Baseline_CO2_Mt = c(55.2, 42.1, 68.4, 18.5, 38.2, 49.0, 31.4),
  Projected_LCOH = c(2.04, 2.13, 2.21, 1.93, 2.36, 2.08, 2.31)
)

india_sf <- st_as_sf(states_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Fixed lines for spillover channels
spillover_lines <- data.frame(
  group = c(1, 2, 3),
  start_lon = c(79.01, 79.01, 71.19),
  start_lat = c(30.06, 30.06, 22.25),
  end_lon   = c(74.21, 78.65, 75.71),
  end_lat   = c(27.02, 22.97, 19.75)
)

# ------------------------------------------------------------------------------
# 2. USER INTERFACE
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "darkly", primary = "#00bc8c"),
  title = "India Green Hydrogen Ecosystem | Geospatial Digital Twin",
  
  sidebar = sidebar(
    title = "Policy & Scenario Drivers",
    width = 320,
    sliderInput("h2_expansion", "Green H2 Expansion (% Increase):", min = 0, max = 200, value = 50, step = 10),
    sliderInput("capex_red", "Electrolyzer CapEx Reduction (%):", min = 0, max = 70, value = 35, step = 5),
    checkboxInput("show_arrows", "Show Transboundary Spillover Vectors", value = TRUE),
    hr(),
    helpText("Links Fourier ARDL (-1.061 Elasticity) & Spatial Durbin Model (-0.7236 Transboundary Indirect Effect).")
  ),
  
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Geospatial Factor Allocation & Transboundary Spillover Channels"),
      leafletOutput("map", height = "550px")
    ),
    card(
      card_header("Selected State Inspector"),
      uiOutput("state_details"),
      hr(),
      card_header("Econometric Model Takeaways"),
      helpText("• Direct Local Effect (+0.7878): Captures localized industrial expansion."),
      helpText("• Transboundary Spillover (-0.7236): Represented by orange arrows exporting decarbonization across borders.")
    )
  )
)

# ------------------------------------------------------------------------------
# 3. SERVER LOGIC (Zero-Length Proofed)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  pal <- colorNumeric(palette = "viridis", domain = c(0.28, 0.35))
  
  # Render base Leaflet map safely without external provider dependencies
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 78.9629, lat = 22.5937, zoom = 5)
  })
  
  # Observe inputs and update map
  observe({
    # Hard safety check for startup inputs
    if (is.null(input$h2_expansion) || is.null(input$capex_red)) return()
    
    h2_val <- as.numeric(input$h2_expansion)
    capex_val <- as.numeric(input$capex_red)
    show_arrows_val <- isTRUE(input$show_arrows)
    
    elasticity_co2 <- -1.061
    
    sf_calc <- india_sf %>%
      mutate(
        Adjusted_CO2 = Baseline_CO2_Mt * (1 - (h2_val * 0.01 * abs(elasticity_co2) / 100)),
        Adjusted_LCOH = Projected_LCOH * (1 - (capex_val / 100) * 0.5)
      )
    
    popups <- sprintf(
      "<div style='font-family: Arial; width: 200px;'>
        <h4 style='color: #00bc8c; margin-bottom: 5px;'>%s</h4>
        <hr style='margin: 5px 0;'>
        <b>Mean FA Ratio:</b> %.3f<br>
        <b>Baseline CO₂:</b> %.1f Mt<br>
        <b>Adjusted CO₂:</b> <span style='color: #e74c3c;'>%.1f Mt</span><br>
        <b>Est. LCOH:</b> <span style='color: #f39c12;'>$%.2f / kg</span>
      </div>",
      sf_calc$ST_NM, sf_calc$Mean_FA_Ratio, sf_calc$Baseline_CO2_Mt, sf_calc$Adjusted_CO2, sf_calc$Adjusted_LCOH
    )
    
    proxy <- leafletProxy("map")
    proxy %>% clearShapes() %>% clearMarkers()
    
    # Render Circle Markers
    proxy %>% addCircleMarkers(
      data = sf_calc,
      lng = ~lon, lat = ~lat,
      radius = ~sqrt(Baseline_CO2_Mt) * 3,
      fillColor = ~pal(Mean_FA_Ratio),
      fillOpacity = 0.85,
      weight = 2,
      color = "#ffffff",
      layerId = ~ST_NM,
      popup = popups
    )
    
    # Vectorized Spillover Rendering (NO FOR LOOPS)
    if (show_arrows_val) {
      proxy %>%
        addPolylines(
          lng = c(spillover_lines$start_lon[1], spillover_lines$end_lon[1]),
          lat = c(spillover_lines$start_lat[1], spillover_lines$end_lat[1]),
          color = "#f39c12", weight = 2.5, dashArray = "5, 10"
        ) %>%
        addPolylines(
          lng = c(spillover_lines$start_lon[2], spillover_lines$end_lon[2]),
          lat = c(spillover_lines$start_lat[2], spillover_lines$end_lat[2]),
          color = "#f39c12", weight = 2.5, dashArray = "5, 10"
        ) %>%
        addPolylines(
          lng = c(spillover_lines$start_lon[3], spillover_lines$end_lon[3]),
          lat = c(spillover_lines$start_lat[3], spillover_lines$end_lat[3]),
          color = "#f39c12", weight = 2.5, dashArray = "5, 10"
        )
    }
  })
  
  # State Inspector side-panel output
  output$state_details <- renderUI({
    click <- input$map_marker_click
    
    if (is.null(click) || is.null(click$id)) {
      return(div(
        class = "alert alert-info",
        "💡 Click on any node on the map to inspect Factor Allocation details, emissions abatement, and localized LCOH trajectory."
      ))
    }
    
    selected_state <- india_sf %>% filter(ST_NM == click$id)
    
    if (nrow(selected_state) == 0) return(NULL)
    
    h2_val <- if (!is.null(input$h2_expansion)) input$h2_expansion else 50
    capex_val <- if (!is.null(input$capex_red)) input$capex_red else 35
    
    curr_lcoh <- selected_state$Projected_LCOH * (1 - (capex_val / 100) * 0.5)
    co2_reduction <- h2_val * 1.061
    
    tagList(
      h3(selected_state$ST_NM, style = "color: #00bc8c;"),
      div(class = "well",
          p(strong("Mean FA Ratio: "), sprintf("%.3f", selected_state$Mean_FA_Ratio)),
          p(strong("Baseline CO₂: "), sprintf("%.2f Mt", selected_state$Baseline_CO2_Mt)),
          p(strong("Target LCOH: "), sprintf("$%.2f / kg", curr_lcoh)),
          p(strong("Modeled CO₂ Reduction: "), sprintf("-%.2f%%", co2_reduction))
      )
    )
  })
}

# ------------------------------------------------------------------------------
# 4. LAUNCH APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
