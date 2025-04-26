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

#' Get Municipality by State and Munis
#'
#' @param munis Character vector of state names.
#' @param crs target coordinate reference system.
#'
#' @returns An `sf` object
#' @export
munis_get <- function(munis, crs = 4326, fallback = FALSE) {
  parsed <- utils_parse_place_state(munis)
  if (!all(parsed$states %in% munis_defined())) {
    unmatched <- subset(parsed$states, !(parsed$states %in% munis_defined()))
    message("No municipal boundary function defined for 
            {stringr::str_c(unmatched, sep=',')}.")
    if (!fallback) {
      stop("Stopping.")
    } else {
      message("Falling back on CDPs from tigris::places().")
      census_places <- unmatched |>
        tigris_get_places(crs = crs) 
    }
  }
  df <- subset(parsed$states, parsed$states %in% munis_defined()) |>
    stringr::str_to_lower() |>
    purrr::map(\(x) {
        do.call(glue::glue("munis_get_{x}"), args=list(crs = crs))
      }
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      name = dplyr::case_when(
        stringr::str_detect(name, "^ *$") ~ NA_character_,
        .default = name
      )
    ) |>
    dplyr::bind_rows(census_places) |>
    tidyr::drop_na(name) |>
    utils_slugify(name, state) |>
    sf::st_as_sf() |>
    sf::st_cast("MULTIPOLYGON")|>
    st_preprocess(crs)
  # print(df)
  if (!is.null(parsed$places)) {
    df <- df |>
      utils_slugify(name, state, col="long", sep=",") |>
      dplyr::filter(
        long %in% stringr::str_to_lower(parsed$upper)
        ) |>
      dplyr::select(-long)
  }
  df
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
    munis_get(crs)
}