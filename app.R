library(tidyverse)
library(bs4Dash)
library(sf)
library(mapedit)
library(leaflet)
library(DT)
library(shinyjs)

tas = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/transport_regions_2022.geojson")
tool_types = c("Areas", "Routes (not yet implemented)", "Crossings (not yet implemented)")
# Tool types
task = c("Area tool: Draw the boundaries representing area interventions and click 'Map edits complete' in the togglable left hand sidebar when complete")
data_input_instructions = paste(collapse = " ",
  "Zoom into your region and draw the boundaries of area interventions",
  "with the 'Draw a polygon' tool below the line 'Draw a polyline' tool.",
  "Complete each boundary by clicking on the first boundary point.",
  "When your areas are complete click on 'Map edits complete' above",
  "and enter the name, description and other details of each intervention",
  "in the table that appears by double clicking on the table.",
  "Save the data by pressing Ctrl+Enter and download the data by clicking",
  "the Download button. See demo video here (TBC)."
)

map = leaflet() |>
  addTiles() |>
  addPolylines(data = tas)

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

ui = bs4DashPage(
  header = dashboardHeader(
    title = "ATIP",
    leftUi = dropdownMenu(task, type = "tasks")),
  fullscreen = FALSE,
  dark = FALSE,
  title = "ATIP",
  dashboardSidebar(
    collapsed = TRUE,
    skin = "gray",
    width = "40%",
    minified = FALSE,
    conditionalPanel(
      condition = "input.edit == false",
      selectInput(inputId = "tool", label = "Select intervention type", choices = tool_types),
    ),
    checkboxInput(inputId = "edit", label = "Map edits complete", value = FALSE),
    conditionalPanel(
      condition = "input.edit == false",
      textOutput("text")
      ),
    conditionalPanel(condition = "input.edit == true", dt_output('Add data', 'd_edit')),
    conditionalPanel(condition = "input.edit == true", div(style="position:relative; left:calc(25%);",     downloadButton("downloadData", "Download")))
  ),
  bs4DashBody(
 editModUI("map", width = "100%", height = 800)
    # fillPage(
    #   )
    )
  )

server = function(input, output) {
  
  edits = callModule(
    editMod,
    leafmap = map,
    id = "map"
  )

  output$text = renderText(data_input_instructions)
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
