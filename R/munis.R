munis_process <- function(munis, name_col, state_abbrev) {
  munis |>
    dplyr::rename_with(tolower) |>
    dplyr::select(name = name_col)  |>
    tidyr::drop_na("name") |>
    dplyr::filter(!stringr::str_detect(name, "^ *$")) |>
    dplyr::mutate(state = state_abbrev) 
}

#' Downloads Municipal Boundaries by State
#' @name muni_get_state
#' 
#' @description
#' `muni_get_ma()` Downloads municipal boundaries for Massachusetts.
#' 
#' `muni_get_ct()` Downloads municipal boundaries for Connecticut.
#' 
#' `muni_get_me()` Downloads municipal boundaries for Maine.
#' 
#' `muni_get_nh()` Downloads municipal boundaries for New Hampshire.
#' 
#' `muni_get_nj()` Downloads municipal boundaries for New Jersey.
#' 
#' `muni_get_ny()` Downloads municipal boundaries for New York.
#' 
#' `muni_get_ri()` Downloads municipal boundaries for Rhode Island.
#' 
#' `muni_get_vt()` Downloads municipal boundaries for Vermont.
#'
#' @returns An `sf` object.
#' @export
munis_get_ma <- function() {
  message("Downloading Massachusetts municipal boundaries...")
  utils_get_arc("43664de869ca4b06a322c429473c65e5_0") |>
    munis_process(name_col = "town", state_abbrev = "MA")
}

#' @name muni_get_state
#' @export
munis_get_ct <- function() {
  message("Downloading Connecticut municipal boundaries...")
  utils_get_arc("df1f6d681b7e41dca8bdd03fc9ae0dd6_1")  |>
    munis_process(name_col = "town", state_abbrev = "CT")
}

#' @name muni_get_state
#' @export
munis_get_me <- function() {
  message("Downloading Maine municipal boundaries...")
  utils_get_arc("289a91e826fd4f518debdd824d5dd16d_0")  |>
    munis_process(name_col = "town", state_abbrev = "ME")
}

#' @name muni_get_state
#' @export
munis_get_nh <- function() {
  message("Downloading New Hampshire municipal boundaries...")
  utils_get_arc("4edf75ab263b4d92996f92fb9cf435fa_8")  |>
    munis_process(name_col = "name", state_abbrev = "NH")
}

#' @name muni_get_state
#' @export
munis_get_nj <- function() {
  url <- httr::parse_url("https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/ArcGIS/rest/services")
  url$path <- paste(url$path, "NJ_Municipal_Boundaries_3424/FeatureServer/0/query", sep = "/")
  url$query <- list(where = "POPDEN2020>0",
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- httr::build_url(url)
  
  sf::st_read(request) |>
    munis_process(name_col = "name", state_abbrev = "NJ")
}

#' @name muni_get_state
#' @export
munis_get_ny <- function() {
  utils_get_remote_shp(
    "https://gisdata.ny.gov/GISData/State/Civil_Boundaries/NYS_Civil_Boundaries.shp.zip",
    layer="Cities_Towns.shp"
  ) |>
    munis_process(name_col = "name", state_abbrev = "NY")
}

#' @name muni_get_state
#' @export
munis_get_ri <- function() {
  message("Downloading Rhode Island municipal boundaries...")
  utils_get_arc("957468e8bb3245e8b3321a7bf3b6d4aa_0") |>
    munis_process(name_col = "name", state_abbrev = "RI")
}

#' @name muni_get_state
#' @export
munis_get_vt <- function() {
  message("Downloading Vermont municipal boundaries...")
  utils_get_arc("3f464b0e1980450e9026430a635bff0a_0")  |>
    munis_process(name_col = "townnamemc", state_abbrev = "VT")
}

munis_router <- function(state, crs) {
  if (state == "MA") {
    data <- munis_get_ma() |>
      sf::st_transform(crs)
  } else if (state == "CT") {
    data <- munis_get_ct() |>
      sf::st_transform(crs)
  } else if (state == "ME") {
    data <- munis_get_me() |>
      sf::st_transform(crs)
  } else if (state == "NH") {
    data <- munis_get_nh() |>
      sf::st_transform(crs)
  } else if (state == "NJ") {
    data <- munis_get_nj() |>
      sf::st_transform(crs)
  } else if (state == "NY") {
    data <- munis_get_ny() |>
      sf::st_transform(crs)
  } else if (state == "RI") {
    data <- munis_get_ri() |>
      sf::st_transform(crs)
  } else if (state == "VT") {
    data <- munis_get_ri() |>
      sf::st_transform(crs)
  } else {
    data <- tigris_get_places(state, crs = crs, filter = FALSE)
  }
  data
}

#' Get Municipality by State and Munis
#'
#' @param places Character vector of state names.
#' @param crs target coordinate reference system.
#'
#' @returns An `sf` object
#' @export
munis_get_munis <- function(places, crs = 4326, filter = TRUE, fallbacks = c("cdp", "osm")) {
  
  states <- utils_place_states(places)
  
  data <- states |>
        purrr::map(\(x) {
          munis_router(x, crs)
        }) |>
        purrr::list_rbind() |> 
        sf::st_as_sf() |>
        sf::st_cast("MULTIPOLYGON") |>
        st_preprocess(crs)
  
  if (utils_is_muni(places) & filter) {
    data <- data |>
      utils_filter_by_place(places)
  }

  data |>
    dplyr::group_by(.data[["name"]], .data[["state"]]) |>
    dplyr::summarize(
      geometry = sf::st_union(.data[["geometry"]])
    ) |>
    dplyr::ungroup() |>
    utils_slugify(name, state) |>
    sf::st_cast("MULTIPOLYGON")
}

munis_defined <- function() {
  datasets::state.abb |>
    subset(
      "munis_get_" |> 
        stringr::str_c(
          stringr::str_to_lower(
            datasets::state.abb
          )
        ) |> 
        purrr::map(exists) |> 
        unlist()
    )
}



munis_all <- function(crs, defined_only = TRUE) {
  if (defined_only) {
    states <- munis_defined()
  } else {
    states <- datasets::state.abb
  }
  states |>
    munis_get_munis(crs)
}