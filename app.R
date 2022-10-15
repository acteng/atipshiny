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
  dashboardSidebar(collapsed = TRUE, skin = "gray",
    width = "40%", # Changing width makes body fail
    minified = FALSE,
    checkboxInput(inputId = "edit", label = "Map edits complete", value = FALSE),
    conditionalPanel(condition = "input.edit == true", dt_output('Add data', 'd_edit')),
    conditionalPanel(condition = "input.edit == true", div(style="position:relative; left:calc(25%);",     downloadButton("downloadData", "Download")))
  ),
  bs4DashBody(
    fillPage(
      editModUI("map", width = "100%", height = 800)
    )
    )
  )

server = function(input, output) {
  edits = callModule(
    editMod,
    leafmap = map,
    id = "map"
  )

  d_sf = sf::read_sf("intervention.geojson")
  d_df = sf::st_drop_geometry(d_sf)

  observeEvent(input$edit, {
    geom = edits()$finished
    nrows <<- length(geom$geometry)
    d_df <<- d_df[rep(1, max(1, nrows)), ]
    output$d_edit = render_dt(d_df, editable = 'all')
  })

  # edit a row
  observeEvent(input$d_edit_cell_edit, {
    d_df <<- editData(d_df, input$d_edit_cell_edit, 'd_edit')
    message(d_df$name, "2")
  })

  output$downloadData = downloadHandler(
    filename = function() {
      paste("interventions", ".geojson", sep = "")
    },
    content = function(file) {
      geom = edits()$finished
      if(is.null(geom)) {
        geom = d_sf
      }
      geom = sf::st_sf(
        d_df,
        geometry = geom$geometry
      )
      sf::write_sf(geom, file, delete_layer = TRUE, delete_dsn = TRUE)
    }
  )
}

shinyApp(ui, server)
