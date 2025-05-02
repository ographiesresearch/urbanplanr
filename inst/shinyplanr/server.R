server <- function(input, output, session) {
  
  # Setup ====
  
  ## Process Internal Data ====
  
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
  
  ## Declare Reactive Values ====
  
  places <- reactiveVal()
  region <- reactiveVal()
  crs <- reactiveValues()
  crs$units <- "ft"
  
  ## Initialize Map ====
  
  output$map <- mapgl::renderMaplibre({
    mapgl::maplibre(
      bounds = init_bbox,
      style = mapgl::carto_style(
        if(input$darklight == "dark") "dark-matter" else "positron"
      )
    )
  })
  
  # Dynamic UI ====
  
  ## Place Selections ====

  output$place_selector <- renderUI({
    if (input$mode %in% c("states", "counties", "places")) {
      list(
        shiny::selectizeInput(
          inputId = 'selected_places',
          label = glue::glue("{stringr::str_to_sentence(input$mod)}"),
          choices = NULL,
          multiple = TRUE,
          options = list(dropdownParent = 'body')
        )
      )
    } else if (input$mode == "file") {
      fileInput("file", "Choose a File")
    } else if (input$mode == "map") {
      
    }
  })
  
  ## Populate Place Selections ====
  
  shiny::observeEvent(
    input$mode, {
      places(NULL)
      region(NULL)
      if (input$mode == "counties") {
        choices <- counties$long
      } else if (input$mode == "states") {
        choices <- states$state_abbrev
      } else if (input$mode == "places") {
        choices <- places_list$long
      } else {
        choices <- NULL
      }
      shiny::updateSelectizeInput(
        session,
        "selected_places",
        choices = choices,
        server = TRUE
      )
    }
  )
  
  shiny::observeEvent(crs$suggest, {
    print("hello2")
    shiny::updateSelectizeInput(
      session,
      "choose_crs",
      choices = crs$suggest$crs_name
    )
  })
  
  observeEvent(input$button_confirm_crs, {
    shiny::req(crs$suggest)
    print(crs$suggest)
    crs <- crs$suggest |>
      dplyr::filter(crs_name == input$choose_crs) |>
      dplyr::pull(crs_code) |>
      as.numeric()
    places() |>
      sf::st_transform(crs) |>
      places()
    bslib::accordion_panel_open(
      "panels",
      "Region"
    )
  })
  
  ## Build Table Settings ====
  
  # output$table_settings <- renderUI({
  #   if (!is.null(places())) {
  #     bslib::accordion_panel(
  #       title = "Table",
  #         shiny::fluidRow(
  #           shiny::column(
  #             width = 6,
  #             shiny::selectizeInput(
  #               "place_id_col",
  #               "ID Column",
  #               choices = names(places()),
  #               selected = if ("id" %in% names(places())) {"id"} else {NULL},
  #               width = "100%",
  #               options = list(dropdownParent = 'body')
  #             )
  #           ),
  #           shiny::column(
  #             width = 6,
  #             shiny::selectizeInput(
  #               "place_name_col",
  #               "Name Column",
  #               choices = names(places()),
  #               selected = if ("name" %in% names(places())) { "name" } else {NULL},
  #               width = "100%",
  #               options = list(dropdownParent = 'body')
  #             )
  #           )
  #         )
  #     )
  #   }
  # })
  
  # Data Processing ====
  
  ## Update Buffer Based on Regionalizer ====
  
  shiny::observeEvent(input$button_update_region, {
    d <- places() |>
      sf::st_buffer(units::as_units(
        input$region,
        input$region_units
      )) |>
      sf::st_union() |>
      sf::st_as_sf(crs = sf::st_crs(places())) |>
      sf::st_set_geometry("geometry")
    
    if (input$region_type == "bbox") {
      d <- d |>
        st_bbox_sf()
    } else if (input$region_type == "square") {
      d <- d |>
        st_square_it()
    } else if (input$region_type == "circle") {
      d <- d |>
        st_square_it(circle = TRUE)
    }
    d |>
      region()
  })
  
  # I/O ====
  
  ## input$file listener. ====
  
  observeEvent(input$file, {
    sf::st_read(input$file$datapath) |>
      places()
  })
  
  ## input$button_confirm_places() listener. ====
  
  observeEvent(input$button_confirm_places, {
    shiny::req(input$selected_places)
    withProgress(
      message = glue::glue("Fetching {input$select_mode}!"),
      detail = "Should only be a few seconds...", {
      if (input$mode == "counties") {
        select <- counties |> 
          dplyr::filter(long %in% input$selected_places) |>
          dplyr::mutate(
            long = stringr::str_replace(long, ",", " COUNTY,")
          ) |>
          dplyr::pull(long) |>
          tigris_get_counties()
      } else if (input$mode == "states") {
        select <- states |> 
          dplyr::filter(state_abbrev %in% input$selected_places) |>
          dplyr::pull(state_abbrev) |>
          tigris_get_states()
      } else if (input$mode == "places") {
        select <- places_list |> 
          dplyr::filter(long %in% input$selected_places) |>
          dplyr::pull(long) |>
          munis_get_munis()
      } else if (input$mode == "map") {
        # Drawn Features NOT WORKING
        select <- mapgl::maplibre_proxy("map") |>
          mapgl::get_drawn_features()
      }
      places(select)
    })
    bslib::accordion_panel_open(
      "panels",
      "CRS"
    )
  })
  
  # Map ==== 
  
  mapgl_add_remove_fill <- function(map, layername, layer, animate = FALSE) {
    map |>
      mapgl::clear_layer(layername)
    
    shiny::req(!is.null(layer))
    
    map |>
      mapgl::add_fill_layer(
        id = layername,
        source = layer,
        fill_color = "#990000",
        fill_opacity = 0.3,
        fill_outline_color = "white"
      ) |>
      mapgl::fit_bounds(layer, animate = TRUE)
  }
  
  ## places() listener. ====
  
  observeEvent(places(), ignoreNULL = FALSE, {
    mapgl::maplibre_proxy("map") |>
      mapgl_add_remove_fill("places", places())
    if (is.null(places())) {
      crs$suggest <- NULL
    } else {
      print("hello")
      crs$suggest <- crsuggest::suggest_crs(places(), gcs = 4269, limit = 5, units = crs$units)
    }
  })
  
  ## region() listener. ====
  
  observeEvent(region(), ignoreNULL = FALSE, {
    mapgl::maplibre_proxy("map") |>
      mapgl_add_remove_fill("region", region())
  })
  
  ## `darklight` listener. ====
  
  shiny::observeEvent(input$darklight, {
    mapgl::maplibre_proxy("map") |>
      mapgl::set_style(
        mapgl::carto_style(if(input$darklight == "dark") "dark-matter" else "positron")
      )
  })
  
  ## input$mode light/dark listener. ====
  
  observeEvent(input$mode, ignoreNULL = FALSE, {
    if (input$mode == "map") {
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
}