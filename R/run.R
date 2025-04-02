basemap <- function(
    states, 
    counties,
    crs, 
    year,
    format,
    acs_datasets,
    census_api) {
  
  # config <- utils_init_env()
  
  tidycensus::census_api_key(census_api)
  format <- utils_std_output_format(format)
  
  # # Create database if it doesn't already exist.
  # # Also prompts user to overwrite or not.
  if (format == "postgis") {
    db_create_if(config$project)
  }
  
  # Get Tigris----
  
  ## States----
  states_ <- tigris_get_states(
    states = states, 
    crs = crs,
    year = year
  )
  
  ## Counties----
  counties_ <- tigris_get_counties(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
  )
  
  ## Places----
  places <- tigris_get_places(
    states = states,
    crs = crs,
    year = year
  )
  
  ## Census Tracts----
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
  
  ## Block Groups----
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
  
  ## Area Water----
  area_water <- tigris_get_area_water(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
  )
  
  ## Linear Water----
  linear_water <- tigris_get_linear_water(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
    )
  
  ## Roads----
  roads <- tigris_get_roads(
    states = states, 
    counties = counties, 
    crs = crs,
    year = year
  )
  
  ## Rails----
  rails <- tigris_get_rails(
    crs = crs,
    year = year
  )
  
  # Get ACS----
  acs <- list()
  for (d in acs_datasets) {
    acs[[d]]<- acs_get_multi(
      var = d,
      params = list(
        states = states,
        year = year
      )
    )
  }
  
  # Get LODES----
  
  od <- get_lodes(
    states = states,
    year = year,
    census_unit = config$census_unit) |>
    prep_lodes(
      census_unit = config$census_unit
    )
  
  od_census_units <- od |>
    lodes_to_census_units(
      census_units_geo = census_units,
      census_unit = config$census_unit
      )

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
  # if ("lodes" %in% config$datasets) {
  #   message("Downloading and processing LEHD Origin-Destination Employment Statistics (LODES) data...")

 
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

# if(!interactive()){
#   renv::init()
#   basemap()
# }