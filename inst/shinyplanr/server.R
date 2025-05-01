server <- function(input, output, session) {
  
  counties <- COUNTIES |>
    dplyr::arrange(state_abbrev, county_geoid) |>
    dplyr::mutate(
      long = stringr::str_c(county_name, state_abbrev, sep=", ")
    )
  
  states <- STATES |>
    dplyr::arrange(state_abbrev)
  
  places_list <- MUNIS |>
    dplyr::arrange(state, name) |>
    dplyr::mutate(
      long = stringr::str_c(name, state, sep=", ")
    )
  
  init_bbox <- states |>
    dplyr::filter(state_abbrev %in% c("WA", "FL", "ME", "CA")) |>
    sf::st_union() |>
    sf::st_as_sf() |>
    sf::st_set_geometry("geometry") |>
    st_bbox_sf()
  
  places <- reactiveVal()
  places_boundary <- reactiveVal()
  
  shiny::observeEvent(
    input$placemode, {
      places(NULL)
      places_boundary(NULL)
      if (input$placemode == "counties") {
        shiny::updateSelectizeInput(
          session,
          "place_select",
          choices = counties$long,
          server = TRUE
        )
      } else if (input$placemode == "states") {
        shiny::updateSelectizeInput(
          session,
          "place_select",
          choices = states$state_abbrev,
          server = TRUE
        )
      } else if (input$placemode == "places") {
        shiny::updateSelectizeInput(
          session,
          "place_select",
          choices = places_list$long,
          server = TRUE
        )
      }
    }
  )
  
  observeEvent(input$placemode, ignoreNULL = FALSE, {
    if (input$placemode == "map") {
      mapgl::maplibre_proxy("map") |>
        mapgl::add_draw_control(
          controls = c(
            "line_string" = FALSE, 
            "combine_features" = FALSE, 
            "uncombine_features" = FALSE
          )
        )
    } else {
      mapgl::maplibre_proxy("map") |>
        mapgl::clear_controls()
    }
  })

  output$place_select <- renderUI({
    if (input$placemode %in% c("states", "counties", "places")) {
      list(
        shiny::selectizeInput(
          inputId = 'place_select',
          label = "Places",
          choices = NULL,
          multiple = TRUE,
          options = list(dropdownParent = 'body')
        ),
        shiny::actionButton(
          "button_get_places", 
          "Fetch Selections",
          width = "100%"
        )
      )
    } else if (input$placemode == "file") {
      fileInput("file", "Choose a File")
    } else if (input$placemode == "map") {
      shiny::actionButton(
        "button_get_drawn", 
        "Finish Drawing",
        width = "100%"
      )
    }
  })
  
  output$place_settings <- renderUI({
    if (!is.null(places())) {
      list(
        bslib::card(
          h3("Boundary Settings"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              numericInput( 
                "place_buffer", 
                "Buffer", 
                value = 0, 
                min = 1,
                width = "100%"
              )
            ),
            shiny::column(
              width = 6,
              selectizeInput( 
                "place_buffer_units", 
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
              checkboxInput("place_buffer_as_bbox", "Boundary as Bounding Box?", FALSE)
            ),
            shiny::column(
              width = 6,
              shiny::actionButton(
                "update_place_buffer", 
                "Update",
                width = "100%"
              )
            )
          )
        ),
        bslib::card(
          h3("Place Settings"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectizeInput(
                "place_id_col", 
                "ID Column", 
                choices = names(places()),
                selected = if ("id" %in% names(places())) {"id"} else {NULL},
                width = "100%",
                options = list(dropdownParent = 'body')
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectizeInput(
                "place_name_col",
                "Name Column",
                choices = names(places()),
                selected = if ("name" %in% names(places())) { "name" } else {NULL},
                width = "100%",
                options = list(dropdownParent = 'body')
              )
            )
          )
        )
      )
    }
  })
  
  shiny::observeEvent(input$update_place_buffer, {
    d <- places() |>
      sf::st_buffer(units::as_units(
        input$place_buffer,
        input$place_buffer_units
      )) |>
      sf::st_union() |>
      sf::st_as_sf() |>
      sf::st_set_geometry("geometry")
    
    if (input$place_buffer_as_bbox) {
      d <- d |>
        st_bbox_sf()
    }
    d |>
      places_boundary()
  })
  
  output$map <- mapgl::renderMaplibre({
    mapgl::maplibre(
      bounds = init_bbox
      )
  })
  
  observeEvent(input$file, {
    sf::st_read(input$file$datapath) |>
      places()
  })
  
  observeEvent(places(), ignoreNULL = FALSE, {
    mapgl::maplibre_proxy("map") |>
      mapgl::clear_layer("places")
    
    shiny::req(!is.null(places()))
    
    mapgl::maplibre_proxy("map") |>
      mapgl::add_line_layer(
        id = "places",
        source = places(),
        line_color = "blue"
      ) |>
      mapgl::fit_bounds(places(), animate = FALSE)
    }
  )
  
  observeEvent(places_boundary(), ignoreNULL = FALSE, {
    mapgl::maplibre_proxy("map") |>
      mapgl::clear_layer("placesBoundary")
    
    shiny::req(!is.null(places_boundary()))
    
    mapgl::maplibre_proxy("map") |>
      mapgl::add_line_layer(
        id = "placesBoundary",
        source = places_boundary(),
        line_color = "red"
      ) |>
      mapgl::fit_bounds(places_boundary(), animate = TRUE)
  })
  
  observeEvent(input$button_get_drawn, {
    withProgress(
      message = glue::glue("Fetching {input$placemode}!"),
      detail = "Should only be a few seconds...",
      features <- mapgl::maplibre_proxy("map") |>
        mapgl::get_drawn_features()
      )
    output$drawn_features <- renderPrint({
      print(features)
    })
  })
  
  observeEvent(input$button_get_places, {
    withProgress(
      message = glue::glue("Fetching {input$placemode}!"),
      detail = "Should only be a few seconds...",
      if (input$placemode == "counties") {
          select <- counties |> 
            dplyr::filter(long %in% input$place_select) |>
            dplyr::mutate(
              long = stringr::str_replace(long, ",", " COUNTY,")
            ) |>
            dplyr::pull(long) |>
            tigris_get_counties() |>
            places()
      } else if (input$placemode == "states") {
        select <- states |> 
          dplyr::filter(state_abbrev %in% input$place_select) |>
          dplyr::pull(state_abbrev) |>
          tigris_get_states() |>
          places()
      } else if (input$placemode == "places") {
        select <- places_list |> 
          dplyr::filter(long %in% input$place_select) |>
          dplyr::pull(long) |>
          munis_get_munis() |>
          places()
      }
  )
  })
}