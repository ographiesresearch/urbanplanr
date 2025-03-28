munis_preprocess <- function(df) {
  df |>
    # TODO: Replace with `st_geom_to_xy()`.
    # center_xy() |>
    sf_preprocess() |>
    dplyr::select(
      pl_name,
      state,
      x_pl = x,
      y_pl = y
    ) |>
    dplyr::mutate(
      pl_id = stringr::str_c(
        stringr::str_to_lower(pl_name), 
        stringr::str_to_lower(state),
        sep = "_"
      )
    )
}

munis_get_ct <- function(crs) {
  message("Downloading Connecticut municipal boundaries...")
  get_from_arc("df1f6d681b7e41dca8bdd03fc9ae0dd6_1", crs = crs) |>
    dplyr::filter(
      town != " ", town != ""
    ) |>
    dplyr::mutate(
      state = "CT"
    ) |>
    dplyr::group_by(pl_name = town, state) |>
    dplyr::summarize(
      geometry = sf::st_union(geometry)
    ) |>
    dplyr::ungroup()
}

munis_get_ma <- function(crs) {
  message("Downloading Massachusetts municipal boundaries...")
  get_from_arc("43664de869ca4b06a322c429473c65e5_0", crs = crs) |>
    dplyr::mutate(
      town = stringr::str_to_title(town),
      state = "MA"
    ) |>
    dplyr::select(pl_name = town, state)
}

munis_get_me <- function(crs) {
  message("Downloading Maine municipal boundaries...")
  get_from_arc("289a91e826fd4f518debdd824d5dd16d_0", crs = crs) |>
    dplyr::filter(
      town != " "
    ) |>
    dplyr::mutate(
      state = "ME"
    ) |>
    sf::st_make_valid() |> 
    dplyr::group_by(pl_name = town, state) |>
    dplyr::summarize(
      geometry = sf::st_union(geometry)
    )
}

munis_get_nh <- function(crs) {
  message("Downloading New Hampshire municipal boundaries...")
  get_from_arc("4edf75ab263b4d92996f92fb9cf435fa_8", crs = crs) |>
    dplyr::filter(
      pbpname != " "
    ) |>
    dplyr::mutate(
      state = "NH"
    ) |>
    dplyr::select(pl_name = pbpname, state)
}

munis_get_nj <- function(crs) {
  url <- httr::parse_url("https://services2.arcgis.com/XVOqAjTOJ5P6ngMu/ArcGIS/rest/services")
  url$path <- paste(url$path, "NJ_Municipal_Boundaries_3424/FeatureServer/0/query", sep = "/")
  url$query <- list(where = "POPDEN2020>0",
                    outFields = "*",
                    returnGeometry = "true",
                    f = "geojson")
  request <- httr::build_url(url)
  
  sf::st_read(request) |>
    dplyr::rename_with(tolower) |>
    sf::st_transform(crs) |>
    dplyr::select(
      name
    ) |>
    dplyr::mutate(state = "NJ")
}


munis_get_ny <- function(crs) {
  get_shp_from_remote_zip(
    "https://gisdata.ny.gov/GISData/State/Civil_Boundaries/NYS_Civil_Boundaries.shp.zip",
    shpfile="Cities_Towns.shp",
    crs=crs
  ) |>
    dplyr::select(
      name
    ) |>
    dplyr::mutate(state = "NY")
}

munis_get_ri <- function(crs) {
  message("Downloading Rhode Island municipal boundaries...")
  get_from_arc("957468e8bb3245e8b3321a7bf3b6d4aa_0", crs = crs) |>
    dplyr::filter(
      name != " ", name != ""
    ) |>
    dplyr::mutate(
      name = stringr::str_to_title(name),
      state = "RI"
    ) |>
    dplyr::group_by(pl_name = name, state) |>
    dplyr::summarize(
      geometry = sf::st_union(geometry)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      pl_name, state
    )
}

munis_get_vt <- function(crs) {
  message("Downloading Vermont municipal boundaries...")
  get_from_arc("3f464b0e1980450e9026430a635bff0a_0", crs = crs) |>
    dplyr::filter(
      townnamemc != " "
    ) |>
    dplyr::mutate(
      state = "VT"
    ) |>
    dplyr::select(pl_name = townnamemc, state)
}

munis_process <- function(df) {
  df |>
    dplyr::mutate(
      city = stringr::str_detect(
        NAME, "city,"
      )
    )
}

munis_select <- function(place_geo, places) {
  searches <- dplyr::bind_rows(places) |>
    dplyr::mutate(
      pl_id = stringr::str_c(
        stringr::str_to_lower(place), 
        stringr::str_to_lower(state), 
        sep="_"
      ),
      pl_id = stringr::str_c("^", pl_id, "$", sep="")
    ) |>
    dplyr::pull(pl_id) |>
    stringr::str_c(collapse="|")
  
  places_vector <- dplyr::bind_rows(places) |>
    dplyr::pull(place)
  
  matched <- place_geo |>
    dplyr::mutate(
      selected = stringr::str_detect(pl_id, searches)
    )
  match_count <- nrow(matched |> dplyr::filter(selected))
  if (match_count == length(places)) {
    message(glue::glue("Exact match found for all place names ({stringr::str_c(places_vector, collapse=', ')})."))
  } else if (match_count > length(places)) {
    message(glue::glue("Ambiguous place names provided."))
    stop()
  } else {
    message(glue::glue("Unable to match all place names."))
    stop()
  }
  matched
}

muni_decision <- function(states, crs) {
  state_munis <- list()
  no_muni_st <- c()
  for (state in states) {
    if (state == "CT") {
      state_munis[[state]] <- munis_get_ct(crs) 
    } else if (state == "MA") {
      state_munis[[state]] <- munis_get_ma(crs)
    } else if (state == "ME") {
      state_munis[[state]] <- munis_get_me(crs)
    } else if (state == "NH") {
      state_munis[[state]] <- munis_get_nh(crs)
    } else if (state == "NJ") {
      state_munis[[state]] <- munis_get_nj(crs)
    } else if (state == "NY") {
      state_munis[[state]] <- munis_get_ny(crs)
    } else if (state == "RI") {
      state_munis[[state]] <- munis_get_ri(crs)
    } else if (state == "VT") {
      state_munis[[state]] <- munis_get_vt(crs)
    } else {
      no_muni_st <- append(no_muni_st, state)
    }
  }
  if (length(no_muni_st) > 0) {
    state_munis[["Other"]] <- get_places(states = no_muni_st, crs = crs)
  }
  dplyr::bind_rows(state_munis) |>
    munis_preprocess(crs)
}