library(tidyverse)
library(shiny)
library(shinydashboard)
library(sf)
library(mapedit)
library(leaflet)

region = ukboundaries::leeds
map = leaflet(data = region) %>%
  addTiles() %>% 
  addPolylines()


if(!file.exists("intervention.geojson")) {
library(osmdata)
intervention = opq(bbox = "leeds") %>%
  add_osm_feature(key = "name", value = "Cycle Superhighway 1", value_exact = FALSE) %>%
  osmdata_sf()
intervention = intervention$osm_multilines
sf::write_sf(intervention, "intervention.geojson")
}

intervention = sf::read_sf("intervention.geojson")
intervention = sf::st_sf(
  data = intervention %>% sf::st_drop_geometry(),
  geometry = intervention$geometry
)


ui = dashboardPage(
  dashboardHeader(title = "ATIP"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      
    box(
      width = 4, 
      downloadButton("downloadData", "Download"),
      textInput(inputId = "name", label = "Intervention name", value = "Intervention x on road y in town z"),
      textInput(inputId = "promoter", label = "Promoter", value = "Transport for ..."),
      sliderInput(inputId = "trips_without", label = "Number of trips per day without the proposed intervention", min = 0, max = 1000, value = 100),
      sliderInput(inputId = "trips_with", label = "Number of trips per day with the proposed intervention", min = 0, max = 1000, value = 110),
      sliderInput(inputId = "uptake", label = "Uptake", min = 0, max = 100, value = 5)
      ),
    box(
      width = 8,
        # add map
        mainPanel(
          editModUI("map")
        )
      )
    )
  )
)

server = function(input, output) {
  # output$map = renderTmap(
  #   qtm(intervention) + tm_view(set.view = 12)
  #   # qtm()
  #   )
  edits <- callModule(
    editMod,
    leafmap = map,
    id = "map"
  )
  
  observeEvent(input$save, {
    
    geom <- edits()$finished
    
    if (!is.null(geom)) {
      assign('new_geom', geom, envir = .GlobalEnv)
      sf::write_sf(geom, 'new_geom.geojson', delete_layer = TRUE, delete_dsn = TRUE)
    }
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input", ".geojson", sep = "")
    },
    content = function(file) {
      geom <- edits()$finished
      
            sf::write_sf(geom, file, delete_layer = TRUE, delete_dsn = TRUE)

    }
  )
  
}

shinyApp(ui, server)

