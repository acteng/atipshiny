library(tidyverse)
library(bs4Dash)
library(sf)
library(mapedit)
library(leaflet)
library(DT)

dt_output = function(title, id) {
  fluidRow(column(
    12, 
    h3(title),
    hr(),
    DTOutput(id)
  ))
}
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}


region = ukboundaries::leeds
map = leaflet(data = region) %>%
  addTiles() %>% 
  addPolylines()

ui = bs4DashPage(fullscreen = FALSE, dark = FALSE,
  dashboardHeader(title = "ATIP"),
  dashboardSidebar(collapsed = FALSE, skin = "gray",
    width = "50%", # Changing width makes body fail
    minified = FALSE,
    dt_output('Add data', 'x2'),
    textInput(inputId = "name", label = "Intervention name", value = "E.g. Chapeltown Active Travel Neighbourhood"),
    textInput(inputId = "description", label = "Description", value = "Brief description of scheme"),
    dateInput(inputId = "completion_date", label = "Planned completion date"),
    shinyWidgets::currencyInput(inputId = "budget_capital", label = "Budget (capital)", format = "British", value = 10000),
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
      paste("interventions", ".geojson", sep = "")
    },
    content = function(file) {
      geom <- edits()$finished
      geom = sf::st_sf(
        data.frame(
          name = input$name,
          description = input$description,
          completion_date = input$completion_date,
          budget_capital = input$budget_capital
          ),
        geometry = geom$geometry
      )
      sf::write_sf(geom, file, delete_layer = TRUE, delete_dsn = TRUE)
    }
  )
  
  d2 = iris
  output$x2 = render_dt(d2, 'row', FALSE)
  
}

shinyApp(ui, server)

