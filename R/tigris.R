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
  df <- tigris::states(...) |>
    st_preprocess(crs)
  
  print(states)
  if (!is.null(states)) {
    df <- df |>
      dplyr::filter(.data$stusps %in% states)
  }
  df
}

#' @name tigris_get_*
#' @export
tigris_get_multi <- function(.function, 
                             places, 
                             state = NULL,
                             county = NULL,
                             year = NULL,
                             ...,
                             crs = 4326) {
  parsed <- utils_parse_place(places)
  is_county <- utils_is_county(places)
  is_state <- utils_is_state(places)
  is_muni <- utils_is_muni(places)
  has_county <- "county" %in% formalArgs(.function)
  has_state <- "state" %in% formalArgs(.function)
  year <- if (is.null(year)) 2022 else year
  if (is_county & has_county) {
    data <- parsed |>
      purrr::map(
        \(x) .function(state = x[2], county = x[1], year = year)
      )  |>
      purrr::list_rbind()
  } else if ((is_county | is_muni | is_state) & has_state) {
    data <- utils_place_states(places) |>
      purrr::map(
        \(x) .function(state = x, year = year)
      ) |>
      purrr::list_rbind()
  } else {
    data <- .function(year = year) 
  }
  
  data <- data |>
    sf::st_as_sf() |>
    dplyr::rename_with(tolower)

  state <- NULL
  if ("statefp" %in% names(data)) {
    data <- data |>
      dplyr::left_join(
        STATES |>
          sf::st_drop_geometry() |>
          dplyr::select(geoid, state = state_abbrev),
        by = dplyr::join_by("statefp" == "geoid")
      )
    state <- "state"
  }

  data |>
    dplyr::select(..., {{state}}) |>
    sf::st_as_sf() |>
    st_preprocess(crs)
}

tigris_extent_to_counties <- function(extent, year = NULL) {
  COUNTIES |>
    sf::st_transform(sf::st_crs(extent)) |>
    sf::st_filter(extent) |>
    dplyr::mutate(
      place = stringr::str_c(county_name, " County", ", ", state_abbrev, sep=""),
    ) |>
    dplyr::pull(place) |>
    tigris_get_counties(
      year = NULL,
      crs = sf::st_crs(extent)
      ) |>
    sf::st_filter(extent)
}

#' @name tigris_get_*
#' 
tigris_get_states <- function(places, year = NULL, crs = 4326, filter = TRUE, ...) {
  data <- tigris_get_multi(
    .function = tigris::states,
    places = places,
    year = year,
    crs = crs,
    id = geoid,
    name = name,
    abbrev = stusps,
    ...
  ) |>
    dplyr::select(-state)
  if (filter) {
    data <- data |>
      utile_filter_by_state(places)
  }
}

#' @name tigris_get_*
#' @export
tigris_get_counties <- function(places, year = NULL, crs = 4326, filter = TRUE, ...) {
  data <- tigris_get_multi(
    tigris::counties,
    places = places,
    year = year,
    crs = crs,
    id = geoid,
    name = name,
    ...
  )
  if (utils_is_county(places) & filter) {
    data <- data |>
      utils_filter_by_place(places)
  }
  data
}

#' @name tigris_get_*
#' @export
tigris_get_places <- function(places, year = NULL, crs = 4326, filter = TRUE, ...) {
  data <- tigris_get_multi(
    .function = tigris::places,
    places = places,
    year = year,
    crs = crs,
    id = geoid,
    name = name,
    ...
  )
}


#' @name tigris_get_*
#' @export
tigris_get_postal <- function(places = NULL, year = 2010, crs = 4326, counties = NULL, ...) {
  tigris_get_multi(
    .function = tigris::zctas,
    places = places,
    year = year,
    crs = crs,
    id = geoid10,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_tracts <- function(places, year = NULL, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::tracts,
    places = places,
    year = year,
    crs = crs,
    id = geoid,
    name = name,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_block_groups <- function(places, year = NULL, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::block_groups,
    places = places,
    year = year,
    crs = crs,
    id = geoid,
    name = namelsad,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_roads <- function(places, year = NULL, crs = 4326, counties = NULL, ...) {
  tigris_get_multi(
    .function = tigris::roads,
    places = places,
    year = year,
    crs = crs,
    id = linearid,
    name = fullname,
    type = rttyp,
    ...
    )
}

#' @name tigris_get_*
#' @export
tigris_get_rails <- function(places, year = NULL, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::rails,
    places = places,
    year = year,
    crs = crs,
    id = linearid,
    name = fullname,
    type = mtfcc,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_area_water <- function(places, year = NULL, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::area_water,
    places = places,
    year = year,
    crs = crs,
    counties = counties,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_linear_water <- function(places, year = NULL, crs = 4326, ...) {
  tigris_get_multi(
    .function = tigris::linear_water,
    places = places,
    year = year,
    crs = crs,
    counties = counties,
    ...
  )
}