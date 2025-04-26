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
      dplyr::filter(.data$STUSPS %in% states)
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
tigris_get_multi <- function(.function, 
                              places, 
                              crs = 4326,
                              ...) {
  parsed <- utils_parse_place_state(places)
  is_county <- all(stringr::str_to_lower(parsed$upper) %in% COUNTIES$long)
  is_states <- is.null(parsed)
  county_args <- "county" %in% formalArgs(.function)
  if (county_args & is_county) {
    data <- parsed$parsed |>
      purrr::map(
        \(x) .function(state = x[2], county = x[1], ...)
      )
  } else if (is_states) {
      data <- parsed$states |>
        purrr::map(
          \(x) .function(state = x, ...)
        )
  } else {
    data <- parsed$states |>
      purrr::map(
        \(x) .function(state = x, ...)
      )
  }
  data |>
    purrr::list_rbind() |>
    sf::st_as_sf() |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_counties <- function(places, crs = 4326, ...) {
  parsed <- utils_parse_place_state(places)
  
  df <- tigris_get_multi(
    tigris::counties,
    places = places,
    crs = crs
  ) |>
    dplyr::left_join(
      STATES |> 
        sf::st_drop_geometry() |> 
        dplyr::select(geoid, state_abbrev),
      by = dplyr::join_by("statefp"=="geoid")
    )
  
  if (!is.null(parsed$places)) {
    df <- df |>
      dplyr::filter(
        stringr::str_c(
          stringr::str_to_upper(.data$name), 
          stringr::str_to_upper(.data$state_abbrev), 
          sep = ",") %in% parsed$upper
        )
    
  }
  df
}

#' @name tigris_get_*
#' @export
tigris_get_places <- function(places, crs = 4326, ...) {
  parsed <- utils_parse_place_state(places)
  
  df <- tigris_get_multi(
    .function = tigris::places,
    places = places,
    crs = crs,
    ...
  ) |>
    dplyr::select(statefp, name) |>
    dplyr::left_join(
      STATES |> 
        sf::st_drop_geometry() |> 
        dplyr::select(geoid, state = state_abbrev),
      by = dplyr::join_by("statefp"=="geoid")
    ) |>
    dplyr::select(-statefp)
  
  
  
  if (!is.null(parsed$places)) {
    df <- df |>
      dplyr::filter(
        stringr::str_c(
          stringr::str_to_upper(.data$name), 
          stringr::str_to_upper(.data$state), 
          sep = ",") %in% parsed$upper
      )
    
  }
  df
}

#' @name tigris_get_*
#' @export
tigris_get_tracts <- function(places, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::tracts,
    states = places,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_block_groups <- function(places, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::block_groups,
    places = places,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_roads <- function(places, crs = 4326, counties = NULL, ...) {
  tigris_get_multi(
    .function = tigris::roads,
    places = places,
    crs = crs,
    ...
    )
}

#' @name tigris_get_*
#' @export
tigris_get_primary_roads <- function(crs = 4326, ...) {
  tigris::primary_roads(...) |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_primary_secondary_roads <- function(places, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::primary_secondary_roads,
    places = places,
    crs = crs,
    ...
    )
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
tigris_get_area_water <- function(places, crs = 4326, ...) {
  message("Downloading road geometries.")
  tigris_get_multi(
    .function = tigris::area_water,
    places = places,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_linear_water <- function(places, crs = 4326, ...) {
  message("Downloading road geometries.")
  tigris_get_multi(
    .function = tigris::linear_water,
    places = places,
    crs = crs,
    ...
  )
}