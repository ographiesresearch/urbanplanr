tigris_get_multi <- function(.function,
                             places,
                             state = NULL,
                             county = NULL,
                             year = NULL,
                             crs = 4326,
                             ...) {
  parsed <- utils_parse_place(places)
  is_county <- utils_is_county(places)
  is_state <- utils_is_state(places)
  is_muni <- utils_is_muni(places)
  has_county <- "county" %in% methods::formalArgs(.function)
  has_state <- "state" %in% methods::formalArgs(.function)
  
  year <- if (is.null(year)) 2022 else year
  
  if (is_county & has_county) {
    data <- parsed |>
      purrr::map(\(x) .function(
    state = x[2],
    county = x[1],
    year = year
      ))  |>
      purrr::list_rbind()
  } else if ((is_county | is_muni | is_state) & has_state) {
    data <- utils_place_states(places) |>
      purrr::map(\(x) .function(state = x, year = year)) |>
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
          dplyr::select("geoid", state = "state_abbrev"),
        by = dplyr::join_by("statefp" == "geoid")
      )
    state <- "state"
  }
  
  data |>
    dplyr::select(..., {{state}}) |>
    st_preprocess(crs)
}

tigris_extent_to_counties <- function(extent, year = NULL) {
  COUNTIES |>
    sf::st_transform(sf::st_crs(extent)) |>
    sf::st_filter(extent) |>
    dplyr::mutate(
      place = stringr::str_c(.data$county_name, 
                             " County", 
                             ", ", 
                             .data$state_abbrev),
    ) |>
    dplyr::pull("place") |>
    tigris_get_counties(year = NULL, crs = sf::st_crs(extent)) |>
    sf::st_filter(extent)
}

#' Get Census Geographies
#' @name tigris_get_
#' 
#' @description
#' `tigris_get_states()` retrieves states, commonwealths, and territories.
#'
#' `tigris_get_counties()` retrieves counties and equivalents.
#' 
#' `tigris_get_places()` retrieves census-designated places.
#'
#' `tigris_get_postal()` retrieves ZIP code tabulation areas.
#'
#' `tigris_get_tracts()` retrieves census tracts.
#'
#' `tigris_get_block_groups()` retrieves block groups.
#'
#' `tigris_get_roads()` retrieves roads.
#' 
#' `tigris_get_rails()` retrieves  rail lines.
#'
#' `tigris_get_area_water()` retrieves areal water features.
#'
#' `tigris_get_linear_water()` retrieves linear water features.
#'
#' @param places A vector of cities, counties or states.
#' @param crs EPSG code or `crs` object. `4326` default.
#' @param year Year of TIGER/Line geometries to pull.
#' @param filter If `TRUE`, will filter result by query feature. (For example,
#' `tigris_get_states()` will return only requested states, even though 
#' tigris::states() returns the entire nation). Note that this will not speed
#' the query, since the filtering happens after fetching the dataset.
#'
#' @returns Simple Features dataframe.
#' @export
tigris_get_states <- function(places,
                              year = NULL,
                              crs = 4326,
                              filter = TRUE) {
  data <- tigris_get_multi(
    .function = tigris::states,
    places = places,
    year = year,
    crs = crs,
    id = "geoid",
    name = "name",
    abbrev = "stusps"
  ) |>
    dplyr::select(-c("state"))
  if (filter) {
    data <- data |>
      utils_filter_by_state(places)
  }
  data
}

#' @name tigris_get_
#' @export
tigris_get_counties <- function(places,
                                year = NULL,
                                crs = 4326,
                                filter = TRUE) {
  data <- tigris_get_multi(
    tigris::counties,
    places = places,
    year = year,
    crs = crs,
    id = "geoid",
    name = "name"
  )
  if (utils_is_county(places) & filter) {
    data <- data |>
      utils_filter_by_place(places)
  }
  data
}

#' @name tigris_get_
#' @export
tigris_get_places <- function(places,
                              year = NULL,
                              crs = 4326,
                              filter = TRUE) {
  data <- tigris_get_multi(
    .function = tigris::places,
    places = places,
    year = year,
    crs = crs,
    id = "geoid",
    name = "name"
  )
  if (utils_is_muni(places) & filter) {
    data <- data |>
      utils_filter_by_place(places)
  }
  data
}


#' @name tigris_get_
#' @export
tigris_get_postal <- function(places = NULL,
                              year = 2010,
                              crs = 4326) {
  tigris_get_multi(
    .function = tigris::zctas,
    places = places,
    year = year,
    crs = crs,
    id = "geoid10"
  )
}

#' @name tigris_get_
#' @export
tigris_get_tracts <- function(places, 
                              year = NULL, 
                              crs = 4326) {
  tigris_get_multi(
    .function = tigris::tracts,
    places = places,
    year = year,
    crs = crs,
    id = "geoid",
    name = "name"
  )
}

#' @name tigris_get_
#' @export
tigris_get_block_groups <- function(places, 
                                    year = NULL, 
                                    crs = 4326) {
  tigris_get_multi(
    .function = tigris::block_groups,
    places = places,
    year = year,
    crs = crs,
    id = "geoid",
    name = "namelsad"
  )
}

#' @name tigris_get_
#' @export
tigris_get_roads <- function(places,
                             year = NULL,
                             crs = 4326) {
  tigris_get_multi(
    .function = tigris::roads,
    places = places,
    year = year,
    crs = crs,
    id = "linearid",
    name = "fullname",
    type = "rttyp"
  )
}

#' @name tigris_get_
#' @export
tigris_get_rails <- function(places, 
                             year = NULL, 
                             crs = 4326) {
  tigris_get_multi(
    .function = tigris::rails,
    places = places,
    year = year,
    crs = crs,
    id = "linearid",
    name = "fullname",
    type = "mtfcc"
  )
}

#' @name tigris_get_
#' @export
tigris_get_area_water <- function(places, 
                                  year = NULL, 
                                  crs = 4326) {
  tigris_get_multi(
    .function = tigris::area_water,
    places = places,
    year = year,
    crs = crs,
    id = "hydroid",
    name = "fullname",
    type = "mtfcc"
  )
}

#' @name tigris_get_
#' @export
tigris_get_linear_water <- function(places, 
                                    year = NULL, 
                                    crs = 4326) {
  tigris_get_multi(
    .function = tigris::linear_water,
    places = places,
    year = year,
    crs = crs,
    id = "linearid",
    name = "fullname"
  )
}