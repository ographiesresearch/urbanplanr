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

COUNTIES <- counties()

usethis::use_data(COUNTIES, overwrite = TRUE)
