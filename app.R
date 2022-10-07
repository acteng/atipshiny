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
    textInput(inputId = "description", label = "Description", value = "Brief description of scheme"),
    dateInput(inputId = "completion_date", label = "Planned completion date"),
    shinyWidgets::currencyInput(inputId = "budget_capital", label = "Budget (capital)", format = "British", value = 10000),
    shinyWidgets::currencyInput(inputId = "budget_annual", label = "Budget (revenue)", format = "British", value = 1000),
    sliderInput(inputId = "impacts", label = "Network impacts", min = 1, max = 5, value = 3),
    sliderInput(inputId = "porosity", label = "Porosity", min = 1, max = 5, value = 3),
    sliderInput(inputId = "density", label = "Mesh density", min = 1, max = 5, value = 3),
    sliderInput(inputId = "permeability", label = "Permeability", min = 1, max = 5, value = 3),
    sliderInput(inputId = "motor", label = "Motor through-traffic", min = 1, max = 5, value = 3),
    sliderInput(inputId = "crossings", label = "Crossings", min = 1, max = 5, value = 3),
    sliderInput(inputId = "engagement", label = "Engagement practices", min = 1, max = 5, value = 3),
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
      geom = sf::st_sf(
        data.frame(
          name = input$name,
          description = input$description,
          completion_date = input$completion_date,
          budget_capital = input$budget_capital,
          budget_revenue = input$budget_revenue,
          impacts = input$impacts,
          porosity = input$porosity,
          permeability = input$permeability,
          motor = input$motor,
          crossings = input$crossings,
          engagement = input$engagement
          ),
        geometry = geom$geometry
      )
      sf::write_sf(geom, file, delete_layer = TRUE, delete_dsn = TRUE)
    }
  )
  
}

shinyApp(ui, server)

