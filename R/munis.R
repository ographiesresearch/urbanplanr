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
    st_preprocess(crs) |>
    dplyr::mutate(
      name = stringr::str_to_title(.data$town),
      state = "MA"
    ) |>
    dplyr::select("name", "state")
}

#' @name muni_get_state
#' @export
munis_get_ct <- function(crs) {
  message("Downloading Connecticut municipal boundaries...")
  utils_get_arc("df1f6d681b7e41dca8bdd03fc9ae0dd6_1") |>
    st_preprocess(crs) |>
    dplyr::filter(
      "town" != " ", "town" != ""
    ) |>
    dplyr::mutate(
      state = "CT"
    ) |>
    dplyr::group_by(name = .data$town, .data$state) |>
    dplyr::summarize(
      geometry = sf::st_union(.data$geometry)
    ) |>
    dplyr::ungroup()
}

#' @name muni_get_state
#' @export
munis_get_me <- function(crs) {
  message("Downloading Maine municipal boundaries...")
  utils_get_arc("289a91e826fd4f518debdd824d5dd16d_0") |>
    st_preprocess(crs) |>
    dplyr::filter(
      "town" != " "
    ) |>
    dplyr::mutate(
      state = "ME"
    ) |>
    sf::st_make_valid() |> 
    dplyr::group_by(name = .data$town, .data$state) |>
    dplyr::summarize(
      geometry = sf::st_union(.data$geometry)
    )
}

#' @name muni_get_state
#' @export
munis_get_nh <- function(crs) {
  message("Downloading New Hampshire municipal boundaries...")
  utils_get_arc("4edf75ab263b4d92996f92fb9cf435fa_8") |>
    st_preprocess(crs) |>
    dplyr::filter(
      "name" != " "
    ) |>
    dplyr::mutate(
      state = "NH"
    ) |>
    dplyr::select("name", "state")
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
    st_preprocess(crs) |>
    dplyr::select(
      "name"
    ) |>
    dplyr::mutate(state = "NJ")
}

#' @name muni_get_state
#' @export
munis_get_ny <- function(crs) {
  utils_get_remote_shp(
    "https://gisdata.ny.gov/GISData/State/Civil_Boundaries/NYS_Civil_Boundaries.shp.zip",
    layer="Cities_Towns.shp"
  ) |>
    st_preprocess(crs) |>
    dplyr::select(
      "name"
    ) |>
    dplyr::mutate(state = "NY")
}

#' @name muni_get_state
#' @export
munis_get_ri <- function(crs) {
  message("Downloading Rhode Island municipal boundaries...")
  utils_get_arc("957468e8bb3245e8b3321a7bf3b6d4aa_0") |>
    st_preprocess(crs) |>
    dplyr::filter(
      "name" != " ", "name" != ""
    ) |>
    dplyr::mutate(
      name = stringr::str_to_title(.data$name),
      state = "RI"
    ) |>
    dplyr::group_by(name = .data$name, .data$state) |>
    dplyr::summarize(
      geometry = sf::st_union(.data$geometry)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "name", "state"
    )
}

#' @name muni_get_state
#' @export
munis_get_vt <- function(crs) {
  message("Downloading Vermont municipal boundaries...")
  utils_get_arc("3f464b0e1980450e9026430a635bff0a_0") |>
    st_preprocess(crs) |>
    dplyr::filter(
      "townnamemc" != " "
    ) |>
    dplyr::mutate(
      state = "VT"
    ) |>
    dplyr::select(name = "townnamemc", "state")
}

#' Execute Municipality-Getter Using State Name
#'
#' @param states Character vector of state names.
#' @param crs target coordinate reference system.
#'
#' @returns An `sf` object
#' @export
munis_decision <- function(states, crs) {
  states |>
    stringr::str_to_lower() |>
    purrr::map(\(x) {
        func <- glue::glue("munis_get_{x}")
        if (exists(func)) {
          r <- do.call(func, args=list(crs=crs))
        } else {
          message(glue::glue("No source for municipalities defined via {func}."))
        }
        r
      }
    ) |>
    purrr::list_rbind() |>
    sf::st_as_sf() |>
    dplyr::mutate(
      name = dplyr::case_when(
        stringr::str_detect(name, "^ *$") ~ NA_character_,
        .default = name
      )
    ) |>
    tidyr::drop_na(name) |>
    utils_slugify(name, state) |>
    sf::st_cast("MULTIPOLYGON")|>
    st_preprocess(crs)
}

munis_all <- function(crs) {
  funcs <- stringr::str_c(
    "munis_get_",
    stringr::str_to_lower(state.abb)
  )
  
  state.abb[
    funcs |> 
      purrr::map(exists) |> 
      unlist()
  ] |>
    munis_decision(crs)
}