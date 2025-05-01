states <- function() {
  tigris::states() |>
    st_bbox_sf() |>
    dplyr::select(
      geoid=GEOID,
      state_abbrev=STUSPS,
      state_name=NAME
    )
}

counties <- function(states) {
  counties <- tigris::counties() |>
    st_bbox_sf() |>
    dplyr::select(
      county_name = NAME,
      state_geoid = STATEFP,
      county_id = COUNTYFP,
      county_geoid = GEOID
    ) |>
    dplyr::left_join(
      states |> sf::st_drop_geometry(),
      by = dplyr::join_by("state_geoid"=="geoid")
    ) |>
    dplyr::mutate(
      long = stringr::str_to_lower(stringr::str_c(county_name, state_abbrev, sep=","))
    )
}

UTM_ZONES <- file.path("data-raw", "utm_zones.geojson") |>
  sf::st_read() |>
  dplyr::select(zone_num) |>
  sf::st_make_valid()

# For some reason South Dakota is errorring out and it doesn't actually appear
# to be a `tigris` caching thing
MUNIS <- state.abb[! state.abb %in% c("SD")] |>
  munis_get_munis(4326) |>
  st_bbox_sf()

STATES <- states()

COUNTIES <- counties(STATES)

usethis::use_data(COUNTIES, STATES, UTM_ZONES, MUNIS, internal=TRUE, overwrite = TRUE)