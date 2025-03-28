# dotenv::load_dot_env()

options(
  # Suppress `summarise()` has grouped output by 'x'...'z' message.
  dplyr.summarise.inform = FALSE,
  # Suppress read/write CSV progress bar.
  readr.show_progress = FALSE
)

run <- function(config) {
  if (class(config) == "character") {
    config <- jsonlite::read_json(config)
  }
  
  states <- c("MA", "RI", "CT")
  crs <- 2249
  counties <- NULL
  year <- 2023
  
  tidycensus::census_api_key()
  utils_std_output_format()
  
  # # Create database if it doesn't already exist.
  # # Also prompts user to overwrite or not.
  # if (config$format == "postgis") {
  #   db_create_if(config$project)
  # }
  
  # Get states----
  states_ <- tigris_get_states(
    states = states, 
    crs = crs,
    year = year
  )
  
  # Get Counties----
  counties_ <- tigris_get_counties(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
  )
  
  # Get Places----
  places <- tigris_get_places(
    states = states,
    crs = crs,
    year = year
  )
  
  # Get Census Tracts----
  census_tracts <- tigris_get_tracts(
    states = states, 
    crs = crs,
    year = year
  ) |>
    sf::st_join(
      places |>
        dplyr::select(placefp), 
      largest = TRUE
      )
  
  # Get Block Groups----
  block_groups <- tigris_get_block_groups(
    states = states, 
    crs = crs,
    year = year
  ) |>
    sf::st_join(
      places |>
        dplyr::select(placefp), 
      largest = TRUE
    )
  
  # Get Area Water----
  area_water <- tigris_get_area_water(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
  )
  
  # Get Linear Water----
  linear_water <- tigris_get_linear_water(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
    )
  
  # Get Roads----
  roads <- tigris_get_roads(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
  )
  
  # Get Rails----
  rails <- tigris_get_rails(
    crs = crs,
    year = year
  )
  
  f <- function(var, params, geos = c("tract", "block_group", "place")) {
    out <- list()
    for (geo in geos) {
      out[[geo]] <- do.call(
        glue::glue("acs_get_{var}"), 
        args=append(params, list(census_unit = geo))
      )
    }
    out
  }
  
  
  if ("age" %in% datasets) {
    age <- f(
      var = "age",
      params = list(
        states = states,
        year = year
      )
    )
  }
  
  if ("race" %in% datasets) {
    age <- f(
      var = "race",
      params = list(
        states = states,
        year = year
      )
    )
  }
  
  if ("housing" %in% datasets) {
    age <- f(
      var = "race",
      params = list(
        states = states,
        year = year
      )
    )
  }
  
  if ("occ" %in% datasets) {
    age <- f(
      var = "race",
      params = list(
        states = states,
        year = year
      )
    )
  }

  # message("Downloading places...")
  # place_geo <- place_decision(config$states, crs = config$crs)
  # 
  # if ("places" %in% names(config)) {
  #   place_geo <- place_geo |>
  #     select_places(places = config$places)
  # }
  # 
  # place_geo |>
  #   # TODO: Replace with specific column removal.
  #   # remove_coords() |>
  #   write_multi("places", config = config)
  # 
  # census_units |>
  #   # TODO: Replace with specific column removal.
  #   # remove_coords() |>
  #   write_multi("census_unit", config = config)
  # 
  # if ("lodes" %in% config$datasets) {
  #   message("Downloading and processing LEHD Origin-Destination Employment Statistics (LODES) data...")
  #   od <- get_lodes(
  #       states = config$states,
  #       year = config$year,
  #       census_unit = config$census_unit) |>
  #     prep_lodes(
  #       census_unit = config$census_unit
  #     )
  # 
  #   od_census_units <- od |>
  #     lodes_to_census_units(
  #       census_units_geo = census_units,
  #       census_unit = config$census_unit
  #       )
  # 
  #   census_units_measured <- od_census_units |>
  #     proximity_measures() |>
  #     dplyr::filter(unit_id %in% census_units$unit_id)
  # 
  #   if ("places" %in% names(config)) {
  #     census_units_measured <- census_units_measured |>
  #         dplyr::full_join(
  #           od_census_units |>
  #             selected_ods_poly(),
  #           by = "unit_id"
  #           ) |>
  #       dplyr::mutate(
  #         dplyr::across(
  #           dplyr::where(is.numeric), ~tidyr::replace_na(.x, 0)
  #         )
  #       )
  #   }
  # 
  #   census_units_measured |>
  #     write_multi("census_unit_lodes", config = config)
  # 
  #   ods_lines(od_census_units, crs = config$crs) |>
  #     write_multi("census_unit_lodes_lines", config = config)
  # 
  #   ods_lines_place_agg(od_census_units, crs = config$crs) |>
  #     write_multi("place_lodes_lines", config = config)
  # }
}

if(!interactive()){
  renv::init()
  run(get_config(commandArgs(trailingOnly = TRUE)))
}