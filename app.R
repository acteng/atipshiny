library(tidyverse)
library(shiny)
library(shinydashboard)
library(sf)
library(tmap)
tmap_mode("view")
pdf(file = NULL)

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
      textInput(inputId = "name", label = "Intervention name", value = "Intervention x on road y in town z"),
      textInput(inputId = "promoter", label = "Promoter", value = "Transport for ..."),
      sliderInput(inputId = "trips_without", label = "Number of trips per day without the proposed intervention", min = 0, max = 1000, value = 100),
      sliderInput(inputId = "trips_with", label = "Number of trips per day with the proposed intervention", min = 0, max = 1000, value = 110),
      sliderInput(inputId = "uptake", label = "Uptake", min = 0, max = 100, value = 5)
      ),
    box(
      width = 8,
      # tmapOutput("map")
      # Sidebar with a ui for grabbing mapedit data
      sidebarLayout(
        sidebarPanel(
          actionButton('save', 'Save from Map')
        ),
        
        # add map
        mainPanel(
          editModUI("map")
        )
      )
      )
    )
  )
)

server = function(input, output) {
  output$map = renderTmap(
    qtm(intervention) + tm_view(set.view = 12)
    # qtm()
    )
}

shinyApp(ui, server)

