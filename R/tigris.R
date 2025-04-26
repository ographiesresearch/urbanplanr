#' Get Census-Designated Places
#' @name tigris_get_*
#' 
#' @description
#' `tigris_get_places()` retrieves census-designated places.
#' 
#' `tigris_get_states()` retrieves states, commonwealths, and territories.
#' 
#' `tigris_get_counties()` retrieves counties and equivalents.
#' 
#' `tigris_get_zctas()` retrieves ZIP code tabulation areas.
#' 
#' `tigris_get_multistate()` is a helper function that downloads multiple states
#' worth of data, where `tigris` function prevents this.
#' 
#' `tigris_get_multistate_by_county()` is a helper function that downloads
#' multiple states worth of data, county-by-county, where `tigris` function 
#' prevents this.
#' 
#' `tigris_get_tracts()` retrieves census tracts.
#' 
#' `tigris_get_block_groups()` retrieves block_groups.
#' 
#' `tigris_get_roads()` retrieves roads.
#' 
#' `tigris_get_primary_roads()` retrieves primary roads.
#' 
#' `tigris_get_primary_secondary_roads()` retrieves primary/secondary roads.
#' 
#' `tigris_get_area_water()` retrieves areal water features.
#' 
#' `tigris_get_linear_water()` retrieves linear water features.
#' 
#' @param states Vector of two-character state codes.
#' @param starts_with Used only in `tigris_get_zcta()`. Beginning digits of
#' ZCTAs you want to return.
#' @param crs EPSG code or `crs` object. `4326` default.
#' @param counties Character vector of county names. If 
#' `NULL` (the default), all counties are returned.
#' @param .function `tigris` function to run.
#' @param ... Passed on to `tigris` download function (e.g., 
#' `tigris::tracts()`).
#'
#' @returns Simple Features dataframe.
#' @export
#'
tigris_get_states <- function(states = NULL, crs = 4326, ...) {
  df <- tigris::states(...)
  
  if (!is.null(states)) {
    df <- df |>
      dplyr::filter(.data$stusps %in% states)
  }
  df |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_zctas <- function(starts_with = NULL, crs = 4326, counties = NULL, ...) {
  tigris::zctas(starts_with, cb = FALSE, ...) |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_multistate <- function(.function, states, crs = 4326, ...) {
  states |>
    purrr::map(\(x) .function(state = x, ...)) |>
    purrr::list_rbind() |>
    sf::st_as_sf() |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_multistate_by_county <- function(.function, 
                                            states, 
                                            crs = 4326, 
                                            counties = NULL, 
                                            ...) {
  county_list <- COUNTIES |>
    dplyr::filter(.data$state_abbrev %in% states)
  if (!is.null(counties)) {
    if (any(counties %in% county_list$county_name)) {
      county_list <- county_list |>
        dplyr::filter(.data$county_name %in% counties)
    } else if (any(counties %in% county_list$county_id)) {
      county_list <- county_list |>
        dplyr::filter(.data$county_id %in% counties)
    } else if (any(counties %in% county_list$county_geoid)) {
      county_list <- county_list |>
        dplyr::filter(.data$county_geoid %in% counties)
    }
  }
  county_list |>
    purrr::pmap(
      \(s, c) .function(state = s, county = c, ...)
    ) |>
    purrr::list_rbind() |>
    sf::st_as_sf() |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_counties <- function(counties, crs = 4326, ...) {
  parsed <- utils_parse_place_state(counties)
  df <- tigris::counties(
    state = parsed$states
  )
  if (!is.null(parsed$places)) {
    df <- df |>
      dplyr::left_join(
        STATES |> 
          sf::st_drop_geometry() |> 
          dplyr::select(geoid, state_abbrev),
        by = dplyr::join_by("STATEFP"=="geoid")
      ) |>
      dplyr::filter(
        stringr::str_c(
          stringr::str_to_upper(.data$NAME), 
          stringr::str_to_upper(.data$state_abbrev), 
          sep = ",") %in% parsed$upper
        )
    
  }
  df |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_places <- function(states, crs = 4326, ...) {
  message("Downloading county geometries.")
  tigris_get_multistate(
    .function = tigris::places,
    states = states,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_tracts <- function(states, crs = 4326, ...) {
  tigris_get_multistate_by_county(
    .function = tigris::tracts,
    states = states,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_block_groups <- function(states, crs = 4326, ...) {
  tigris_get_multistate(
    .function = tigris::block_groups,
    states = states,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_roads <- function(states, crs = 4326, counties = NULL, ...) {
  tigris_get_multistate_by_county(.function = tigris::roads,
                                  states = states,
                                  crs = crs,
                                  counties = counties,
                                  ...)
}

#' @name tigris_get_*
#' @export
tigris_get_primary_roads <- function(crs = 4326, ...) {
  message("Downloading road geometries.")
  tigris::primary_roads(...) |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_primary_secondary_roads <- function(states, crs = 4326, ...) {
  message("Downloading road geometries.")
  tigris_get_multistate(.function = tigris::primary_secondary_roads,
                                  states = states,
                                  crs = crs,
                                  ...)
}

#' @name tigris_get_*
#' @export
tigris_get_rails <- function(crs = 4326, ...) {
  message("Downloading road geometries.")
  tigris::rails(...) |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_area_water <- function(states, crs = 4326, counties = NULL, ...) {
  message("Downloading road geometries.")
  tigris_get_multistate_by_county(
    .function = tigris::area_water,
    states = states,
    crs = crs,
    counties = counties,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_linear_water <- function(states, crs = 4326, counties = NULL, ...) {
  message("Downloading road geometries.")
  tigris_get_multistate_by_county(
    .function = tigris::linear_water,
    states = states,
    crs = crs,
    counties = counties,
    ...
  )
}