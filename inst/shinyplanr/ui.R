ui <- bslib::page_sidebar(
  sidebar = bslib::sidebar(
    width = "40%",
    open = "always",
    bslib::input_dark_mode(id="darklight"),
    shiny::h1(shiny::code("urbanplanr")),
    bslib::accordion(
      multiple = FALSE,
      id = "panels",
      bslib::accordion_panel(
        title = "Selection",
        shiny::selectizeInput( 
          "mode",
          "How would you like to specify your study area?",
          list(
            "Upload a File" = "file",
            "Select Places" = "places",
            "Select Counties" = "counties",
            "Select States" = "states"
          ),
          options = list(dropdownParent = 'body'),
          multiple = FALSE,
        ),
        shiny::uiOutput("place_selector"),
        shiny::actionButton(
          "button_confirm_places", 
          "Continue",
          width = "100%"
        )
      ),
      shiny::uiOutput("table_settings"),
      bslib::accordion_panel(
        title = "CRS",
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::selectizeInput(
              "choose_crs",
              "Choose a Coordinate Reference System",
              choices = NULL,
              selected = NULL,
              width = "100%",
              options = list(dropdownParent = 'body')
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::actionButton(
              "button_confirm_crs",
              "Continue",
              width = "100%"
            )
          )
        )
      ),
      bslib::accordion_panel(
        title = "Region",
        shiny::fluidRow(
          shiny::column(
            width = 6,
            numericInput(
              "region",
              "Buffer",
              value = 0,
              min = 1,
              width = "100%"
            )
          ),
          shiny::column(
            width = 6,
            selectizeInput(
              "region_units",
              "Units",
              list(
                "Miles" = "miles",
                "Kilometers" = "km",
                "Feet" = "feet",
                "Meters" = "meters"
              ),
              width = "100%"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::fluidRow(
              selectizeInput(
                "region_type",
                "Type",
                list(
                  "Square" = "square",
                  "Circle" ="circle",
                  "Bounding Box" = "bbox",
                  "Radial Buffer" = "buffer"
                )
              )
            )
          ),
          shiny::column(
            width = 6,
            shiny::actionButton(
              "button_update_region",
              "Update",
              width = "100%"
            )
          )
        ),
        shiny::actionButton(
          "button_confirm_region",
          "Continue",
          width = "100%"
        )
      )
      # ,
      # bslib::accordion_panel(
      #   title = "Table",
      #   shiny::fluidRow(
      #     shiny::column(
      #       width = 6,
      #       shiny::selectizeInput(
      #         "place_id_col",
      #         "ID Column",
      #         choices = names(places()),
      #         selected = if ("id" %in% names(places())) {"id"} else {NULL},
      #         width = "100%",
      #         options = list(dropdownParent = 'body')
      #       )
      #     ),
      #     shiny::column(
      #       width = 6,
      #       shiny::selectizeInput(
      #         "place_name_col",
      #         "Name Column",
      #         choices = names(places()),
      #         selected = if ("name" %in% names(places())) { "name" } else {NULL},
      #         width = "100%",
      #         options = list(dropdownParent = 'body')
      #       )
      #     )
      #   )
      # )
      # bslib::accordion_panel(
      #   title = "Output",
      #   shiny::fluidRow(
      #     shiny::column(
      #       width = 6,
      #       textInput( 
      #         "outname", 
      #         "File/DB Name",
      #         value = "results"
      #       )
      #     ),
      #     shiny::column(
      #       width = 6,
      #       shiny::selectizeInput(
      #         inputId = 'out_format',
      #         label = "Format",
      #         choices = c("Geopackage" = "gpkg", "PostGIS" = "postgis"),
      #         multiple = FALSE,
      #         options = list(dropdownParent = 'body')
      #       )
      #     )
      #   )
      # )
    )
  ),
  mapgl::maplibreOutput("map")
)
