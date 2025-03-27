get_remote_file <- function(url, path) {
  httr::GET(
    paste0(url), 
    httr::write_disk(path, overwrite = TRUE)
  )
}

get_shp_from_zip <- function(path, layer) {
  path <- stringr::str_c("/vsizip/", path, "/", layer)
  sf::st_read(path, quiet=TRUE)
}

get_from_arc <- function(dataset, crs) {
  prefix <- "https://opendata.arcgis.com/api/v3/datasets/"
  suffix <- "/downloads/data?format=geojson&spatialRefId=4326&where=1=1"
  sf::st_read(
    glue::glue("{prefix}{dataset}{suffix}")
  ) |>
    dplyr::rename_with(tolower) |>
    sf::st_transform(crs)
}


get_ma_munis <- function(crs) {
  message("Downloading Massachusetts municipal boundaries...")
  get_from_arc("43664de869ca4b06a322c429473c65e5_0", crs = crs) |>
    dplyr::mutate(
      town = stringr::str_to_title(town),
      state = "MA"
    ) |>
    dplyr::select(pl_name = town, state)
}

get_shp_from_remote <- function(url, shpfile, crs) {
  message(
    glue::glue("Downloading {shpfile} from {url}...")
  )
  temp <- base::tempfile(fileext = ".zip")
  get_remote_file(
    url = url,
    path = temp
  )
  get_shp_from_zip(temp, shpfile) |>
    dplyr::rename_with(tolower) |>
    sf::st_transform(crs)
}

get_me_munis <- function(crs) {
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

get_nh_munis <- function(crs) {
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

get_vt_munis <- function(crs) {
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

get_ct_munis <- function(crs) {
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

get_ri_munis <- function(crs) {
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

get_places <- function(states, 
                       year,
                       crs) {
  tigris::places(
    state = states,
    year = year,
    cb = TRUE
  ) |>
    sf::st_transform(crs) |>
    dplyr::rename_with(tolower) |>
    dplyr::select(
      pl_name = name,
      state = stusps
    )
}

get_counties <- function(states) {
  l <- list()
  for (st in states) {
    l[[st]] <- tigris::counties(state = st) |>
      dplyr::rename_with(tolower) |>
      sf::st_drop_geometry() |>
      dplyr::pull(name)
  }
  l
}

get_tigris_multistate <- function(states, .function) {
  all <- list()
  l <- get_counties(states)
  for (st in names(l)) {
    state <- list()
    for (county in l[[st]]) {
      state[[county]] <- .function(state = st, county = county)
    }
    all[[st]] <- dplyr::bind_rows(state)
  }
  dplyr::bind_rows(all)
}

get_water_multistate <- function(states) {
  get_tigris_multistate(states, tigris::area_water)
}

get_roads_multistate <- function(states) {
  get_tigris_multistate(states, tigris::roads)
}

get_census_units <- function(states, 
                             year, 
                             crs,
                             census_unit,
                             cb = TRUE) {
  unit_container <- list()
  for (st in states) {
    if (census_unit == "tract") {
      message("Downloading tract geometries.")
      df <- tigris::tracts(
        year = year, 
        state = st, 
        cb = TRUE,
        progress_bar = FALSE
      )
    } else if (census_unit == "bg") {
      message("Downloading block group geometries.")
      df <- tigris::block_groups(
        year = year, 
        state = st, 
        cb = TRUE,
        progress_bar = FALSE
      )
    } else {
      stop("census_unit parameter must be one of 'tracts' or 'block groups'.")
    }
    unit_container[[st]] <- df
  }
  unit_container |>
    dplyr::bind_rows() |>
    sf::st_transform(crs) |>
    dplyr::rename_with(tolower) |>
    dplyr::rename(
      unit_id = geoid
    ) |>
    dplyr::select(
      unit_id
    )
}

#' Get DEM from AWS Terrain Tiles
#'
#' @param area Simple features data frame or tibble (ideally polygon).
#' @param src One of "aws", "gl3", "gl1", "alos", "srtm15plus".
#'
#' @returns A `RasterLayer`.
#' @export
#'
get_dem <- function(area, z=14, src="aws") {
  dem <- elevatr::get_elev_raster(
    locations=area,
    z=z,
    src=src,
    clip="locations",
    neg_to_na=TRUE
  )
}