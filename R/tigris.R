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
#' @param crs EPSG code or `crs` object.
#' @param counties Character vector of county names or 5/3-digit FIPS codes. If 
#' `NULL` (the default), all counties are returned.
#' @param .function `tigris` function to run.
#' @param ... Passed on to `tigris` download function (e.g., 
#' `tigris::tracts()`).
#'
#' @returns Simple Features dataframe.
#' @export
#'
tigris_get_states <- function(states = NULL, crs, ...) {
  df <- tigris::states(...) |>
    st_preprocess(crs)
  
  if (!is.null(states)) {
    df <- df |>
      dplyr::filter(stusps %in% states)
  }
  df
}

#' @name tigris_get_*
#' @export
tigris_get_counties <- function(states, crs, counties = NULL, ...) {
  df <- tigris::counties(state = states, ...) |>
    st_preprocess(crs)
  
  if (!is.null(counties)) {
    df <- df |>
      dplyr::filter(name %in% counties)
  }
  df
}

#' @name tigris_get_*
#' @export
tigris_get_multistate <- function(.function, states, crs, ...) {
  units <- list()
  for (s in states) {
    units[[s]] <- .function(state = s, ...)
  }
  units |>
    dplyr::bind_rows() |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_multistate_by_county <- function(.function, states, crs, counties = NULL, ...) {
  units <- list()
  county_list <- COUNTIES |>
    dplyr::filter(state_abbrev %in% states)
  if (!is.null(counties)) {
    if (any(counties %in% county_list$county_name)) {
      county_list <- county_list |>
        dplyr::filter(county_name %in% counties)
    } else if (any(counties %in% county_list$county_id)) {
      county_list <- county_list |>
        dplyr::filter(county_id %in% counties)
    } else if (any(counties %in% county_list$county_geoid)) {
      county_list <- county_list |>
        dplyr::filter(county_geoid %in% counties)
    }
  }
  for (i in rownames(county_list)) {
    units[[i]] <- .function(state = county_list[i, "state_geoid"], county = county_list[i, "county_id"], ...)
  }
  units |>
    dplyr::bind_rows()  |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_counties <- function(states, crs, ...) {
  message("Downloading county geometries.")
  tigris_get_multistate(
    .function = tigris::counties,
    states = states,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_places <- function(states, crs, ...) {
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
tigris_get_tracts <- function(states, crs, ...) {
  message("Downloading tract geometries.")
  tigris_get_multistate(
    .function = tigris::tracts,
    states = states,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_block_groups <- function(states, crs, ...) {
  message("Downloading block group geometries.")
  tigris_get_multistate(
    .function = tigris::block_groups,
    states = states,
    crs = crs,
    ...
  )
}

#' @name tigris_get_*
#' @export
tigris_get_roads <- function(states, crs, counties = NULL, ...) {
  message("Downloading road geometries.")
  tigris_get_multistate_by_county(.function = tigris::roads,
                                  states = states,
                                  crs = crs,
                                  counties = counties,
                                  ...)
}

#' @name tigris_get_*
#' @export
tigris_get_primary_roads <- function(crs, ...) {
  message("Downloading road geometries.")
  tigris::primary_roads(...) |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_primary_secondary_roads <- function(states, crs, ...) {
  message("Downloading road geometries.")
  tigris_get_multistate(.function = tigris::primary_secondary_roads,
                                  states = states,
                                  crs = crs,
                                  ...)
}

#' @name tigris_get_*
#' @export
tigris_get_rails <- function(crs, ...) {
  message("Downloading road geometries.")
  tigris::rails(...) |>
    st_preprocess(crs)
}

#' @name tigris_get_*
#' @export
tigris_get_area_water <- function(states, crs, counties = NULL, ...) {
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
tigris_get_linear_water <- function(states, crs, counties = NULL, ...) {
  message("Downloading road geometries.")
  tigris_get_multistate_by_county(
    .function = tigris::linear_water,
    states = states,
    crs = crs,
    counties = counties,
    ...
  )
}