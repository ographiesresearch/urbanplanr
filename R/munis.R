munis_process <- function(munis, name_col, state) {
  munis |>
    dplyr::rename_with(tolower) |>
    dplyr::select(name = .data[[name_col]])  |>
    dplyr::filter(
      !stringr::str_detect(name, "^ *$")
    ) |>
    dplyr::group_by(name) |>
    dplyr::summarize(
      geometry = sf::st_union(geometry)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(state = state)
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
munis_get_ma <- function(crs) {
  message("Downloading Massachusetts municipal boundaries...")
  utils_get_arc("43664de869ca4b06a322c429473c65e5_0") |>
    munis_process(name_col = "town", state = "MA")
}

#' @name muni_get_state
#' @export
munis_get_ct <- function(crs) {
  message("Downloading Connecticut municipal boundaries...")
  utils_get_arc("df1f6d681b7e41dca8bdd03fc9ae0dd6_1")  |>
    munis_process(name_col = "town", state = "CT")
}

#' @name muni_get_state
#' @export
munis_get_me <- function(crs) {
  message("Downloading Maine municipal boundaries...")
  utils_get_arc("289a91e826fd4f518debdd824d5dd16d_0")  |>
    munis_process(name_col = "town", state = "ME")
}

#' @name muni_get_state
#' @export
munis_get_nh <- function(crs) {
  message("Downloading New Hampshire municipal boundaries...")
  utils_get_arc("4edf75ab263b4d92996f92fb9cf435fa_8")  |>
    munis_process(name_col = "name", state = "NH")
}

#' @name muni_get_state
#' @export
munis_get_nj <- function(crs) {
  url <- httr::parse_url("https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/ArcGIS/rest/services")
  url$path <- paste(url$path, "NJ_Municipal_Boundaries_3424/FeatureServer/0/query", sep = "/")
  url$query <- list(where = "POPDEN2020>0",
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- httr::build_url(url)
  
  sf::st_read(request) |>
    munis_process(name_col = "name", state = "NJ")
}

#' @name muni_get_state
#' @export
munis_get_ny <- function(crs) {
  utils_get_remote_shp(
    "https://gisdata.ny.gov/GISData/State/Civil_Boundaries/NYS_Civil_Boundaries.shp.zip",
    layer="Cities_Towns.shp"
  ) |>
    munis_process(name_col = "name", state = "NY")
}

#' @name muni_get_state
#' @export
munis_get_ri <- function(crs) {
  message("Downloading Rhode Island municipal boundaries...")
  utils_get_arc("957468e8bb3245e8b3321a7bf3b6d4aa_0") |>
    munis_process(name_col = "name", state = "RI")
}

#' @name muni_get_state
#' @export
munis_get_vt <- function(crs) {
  message("Downloading Vermont municipal boundaries...")
  utils_get_arc("3f464b0e1980450e9026430a635bff0a_0")  |>
    munis_process(name_col = "townnamemc", state = "VT")
}

#' Get Municipality by State and Munis
#'
#' @param places Character vector of state names.
#' @param crs target coordinate reference system.
#'
#' @returns An `sf` object
#' @export
munis_get_munis <- function(places, crs = 4326, fallbacks = c("cdp", "osm")) {
  
  states <- utils_place_states(places)
  
  matched <- states %in% munis_defined()
  unmatched <- !(states %in% munis_defined())
  
  data <- list()
  if (sum(matched) > 0) {
    data[['matched']] <- states |>
      subset(matched) |>
        purrr::map(\(x) {
          do.call(
            glue::glue("munis_get_{stringr::str_to_lower(x)}"), 
            args = list(crs = crs)
            )
        }) |>
        purrr::list_rbind() |> 
        sf::st_as_sf() |>
        st_preprocess(crs)
  }
  if ("cdp" %in% fallbacks & sum(unmatched) > 0) {
    data[['unmatched']] <- places |>
      subset(unmatched) |>
      tigris_get_places(crs = crs)
  }
  data <- dplyr::bind_rows(data)
  
  if (utils_is_muni(places)) {
    data <- data |>
      utils_filter_by_place(places)
  }
  
  data |>
    tidyr::drop_na(name) |>
    dplyr::filter(!stringr::str_detect(name, "^ *$")) |>
    utils_slugify(name, state) |>
    sf::st_as_sf() |>
    sf::st_cast("MULTIPOLYGON")
}

munis_defined <- function() {
  state.abb |>
    subset(
      "munis_get_" |> 
        stringr::str_c(
          stringr::str_to_lower(
            state.abb
          )
        ) |> 
        purrr::map(exists) |> 
        unlist()
    )
}



munis_all <- function(crs) {
  munis_defined() |>
    munis_get_munis(crs)
}