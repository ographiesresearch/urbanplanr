ui <- bslib::page_sidebar(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  # ),
  sidebar = bslib::sidebar(
    width = "40%",
    bslib::card(
      shiny::selectizeInput( 
        "placemode",
        "How would you like to specify your study area?",
        list(
          "Upload a File" = "file",
          "Select Places" = "places",
          "Select Counties" = "counties",
          "Select States" = "states",
          "Draw on the Map" = "map"
        ),
        options = list(dropdownParent = 'body'),
        multiple = FALSE
      ),
      shiny::uiOutput("place_select"),
    ),
    shiny::uiOutput("place_settings"),
    shiny::verbatimTextOutput("drawn_features")
  ),
  mapgl::maplibreOutput("map")
)
