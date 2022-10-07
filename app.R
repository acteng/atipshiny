library(tidyverse)
library(bs4Dash)
library(sf)
library(mapedit)
library(leaflet)


region = ukboundaries::leeds
map = leaflet(data = region) %>%
  addTiles() %>% 
  addPolylines()

ui = bs4DashPage(fullscreen = FALSE, dark = TRUE,
  dashboardHeader(title = "ATIP"),
  dashboardSidebar(collapsed = FALSE,
    width = "50%", # Changing width makes body fail
    minified = FALSE,
    textInput(inputId = "name", label = "Intervention name", value = "E.g. Chapeltown Active Travel Neighbourhood"),
    textInput(inputId = "promoter", label = "Promoter", value = "Transport for ..."),
    sliderInput(inputId = "trips_without", label = "Number of trips per day without the proposed intervention", min = 0, max = 1000, value = 100),
    sliderInput(inputId = "trips_with", label = "Number of trips per day with the proposed intervention", min = 0, max = 1000, value = 110),
    sliderInput(inputId = "uptake", label = "Uptake", min = 0, max = 100, value = 5),
    div(style="position:relative; left:calc(25%);",     downloadButton("downloadData", "Download"))
  ),
  bs4DashBody(
    fillPage(
      editModUI("map", width = "100%", height = 800)
    )
    )
  )

server = function(input, output) {
  edits <- callModule(
    editMod,
    leafmap = map,
    id = "map"
  )
  
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

