counties <- function() {
  states <- tigris::states() |>
    sf::st_drop_geometry() |>
    dplyr::select(
      geoid=GEOID,
      state_abbrev=STUSPS,
      state_name=NAME
    )
  
  counties <- tigris::counties() |>
    sf::st_drop_geometry() |>
    dplyr::select(
      county_name = NAME,
      state_geoid = STATEFP,
      county_id = COUNTYFP,
      county_geoid = GEOID
    ) |>
    dplyr::left_join(
      states,
      by = dplyr::join_by("state_geoid"=="geoid")
    )
}

UTM_ZONES <- file.path("data-raw", "utm_zones.geojson") |>
  sf::st_read() |>
  dplyr::select(zone_num) |>
  sf::st_make_valid()

COUNTIES <- counties()

usethis::use_data(COUNTIES, UTM_ZONES, internal=TRUE, overwrite = TRUE)
