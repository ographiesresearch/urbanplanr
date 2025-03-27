#' Calculate a Hillshade from a Digital Elevation Model.
#' 
#' Can be used to calcualte a hillshade from a DEM `RasterLayer` of the type
#' returned by `get_dem()`.
#'
#' @param dem A digital elevation model stored as a `RasterLayer`.
#' @param angle Elevation angle of the light source (degrees). Defaults to 45.
#' @param direction Azimuth angle of the light source (degrees). Defaults to 300.
#' @param normalize Logical. If `TRUE` (default), values below zero are set to zero and the results are multiplied by 255.
#' @param overwrite Logical. If `TRUE` (and `filename` is set), overwrites existing raster file.
#' @param filename Character. Optional output filename.
#'
#' @returns A `RasterLayer`.
#' @export
#'
hillshade_from_dem <- function(
    dem, 
    angle=45, 
    direction=300, 
    normalize=TRUE,
    overwrite=TRUE,
    filename='') {
  raster::hillShade(
    slope=terra::terrain(dem, v="slope", unit="radians"), 
    aspect=terra::terrain(dem, v="aspect", unit="radians"), 
    angle=angle,
    direction=direction,
    filename=filename, 
    normalize=normalize, 
    overwrite=overwrite
    )
}

#' Retrieve Scaling Parameters, Given Model Size
#'
#' @param df Simple features dataframe or tibble.
#' @param model_size Optional. Largest dimension of model, as numeric or `units`. If numeric, treated as map units. If not provided, model will not be scaled.
#'
#' @returns Named list with `factor`, `x_delta`, `y_delta`.
#'
scale_to_model <- function(df, model_size = NULL) {
  if (!is.null(model_size)) {
    bbox <- sf::st_bbox(df)
    x_size <- abs(bbox$xmax - bbox$xmin)
    y_size <- abs(bbox$ymax - bbox$ymin)
    x_center <- as.numeric(bbox$xmax - (x_size / 2))
    y_center <- as.numeric(bbox$ymax - (y_size / 2))
    
    factor <- as.numeric(units::set_units(model_size, m)) / max(x_size, y_size)
  } else {
    x_center <- 0
    y_center <- 0
    factor <- 1
  }
  c(factor = factor, x_delta = x_center, y_delta = y_center)
}

#' Scale Layer Based on Scaling Parameters
#'
#' @param df Simple features dataframe or tibble.
#' @param scale Scaling parameters, as returend by `scale_to_model()`.
#' @param thickness Numeric. Thickness of model plywood sheets.
#' @param z_scale Numeric. Z-exaggeration for elevation.
#' @param contour_int Nuemeric. Intervals for contours.
#'
#' @returns Scaled simple features dataframe or tibble.
#' @export
#' 
model_scale <- function(df, scale, thickness, z_scale, contour_int) {
  crs <- sf::st_crs(df)
  scaled <- (sf::st_geometry(df) - c(scale['x_delta'], scale['y_delta'])) * scale['factor']
  
  if (!("sfc" %in% class(df))) {
    sf::st_geometry(df) <- scaled
    if ("z" %in% colnames(df))
      df <- df |> 
        dplyr::mutate(z = (z / (z_scale * contour_int)) * as.numeric(set_units(thickness, m)))
  }
  df <- df |>
    sf::st_set_crs(crs)
  df
}

set_census_api <- function(config) {
  if ("census_api" %in% names(config)) {
    message("Census API key set.")
    suppressMessages(tidycensus::census_api_key(config$census_api))
  } else {
    message("No census API key privided. Consider setting `census_api` in `config.json`.")
  }
  config
}

get_config <- function(args) {
  if (length(args) > 0) {
    if (class(args[1]) != "character") {
      stop("Expected the name of a configuration json, as a string")
    } else {
      config_json <- args[1]
    }
  } else {
    config_json <- 'config.json'
  }
  config_json
}

std_census_units <- function(config) {
  if (config$census_unit %in% c("tract", "tracts", "ct", "cts")) {
    config$census_unit <- "tract"
    config$lehd_unit <- "tract"
  } else if (configG$census_unit %in% c("block groups", "block group", 
                                        "cbg", "cbgs", "bg", "bgs")) {
    config$census_unit <- "cbg"
    config$lehd_unit <- "bg"
  } else {
    stop("census_unit parameter must be one of 'tracts' or 'block groups'.")
  }
  message(
    glue::glue("Census areal unit set to '{config$census_unit}'.\n
               LEHD areal unit set to '{config$lehd_unit}'."))
  config
}


std_format <- function(config) {
  if (config$format %in% c("shapefile", "shp")) {
    config$format <- "shp"
  } else if (config$format %in% c("geopackage", "gpkg")) {
    config$format <- "gpkg"
  } else if (config$format %in% c("geojson", "json")) {
    config$format <- "geojson"
  } else if (config$format == "postgis") {
    # do nothing.
  }else {
    stop("'format' parameter must be one of 'postgis', 'shp', 'gpkg', or 'geojson'.")
  }
  message(glue::glue("Output format set to {config$format}."))
  config
}

prompt_check <- function(prompt) {
  message(prompt)
  if (interactive()) {
    r <- readline()
  } else {
    r <- readLines("stdin",n=1);
  }
  if (r %in% c("Y", "y", "N", "n")) {
    check <- TRUE
  } else {
    message(
      glue::glue("Response '{r}' is invalid. Must be Y or N.")
    )
    check <- FALSE
  }
  if (!check) {
    prompt_check(prompt)
  } else {
    if (r %in% c("Y", "y")) {
      message(
        glue::glue("You answered '{r}'!.")
      )
      return(TRUE)
    } else if (r %in% c("N", "n")) {
      message(
        glue::glue("You answered '{r}'! Stopping.")
      )
      return(FALSE)
    }
  }
}

write_multi <- function(df, 
                        name, 
                        dir_name, 
                        format) {
  message(glue::glue("Writing {name}."))
  if (format == "gpkg") {
    sf::st_write(
      df,
      stringr::str_c(dir_name, format, sep="."),
      name,
      append = FALSE,
      delete_layer = TRUE,
      quiet = TRUE
    )
  } else if (format == "postgis") {
    conn <- db_create_conn(dir_name, admin=TRUE)
    on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
    sf::st_write(
      df,
      conn,
      name,
      append = FALSE,
      delete_layer = TRUE,
      quiet = TRUE
    )
  } else {
    dir.create(dir_name, showWarnings = FALSE)
    if ("sf" %in% class(df)) {
      sf::st_write(
        df,
        file.path(
          dir_name,
          stringr::str_c(name, format, sep=".")
        ),
        append = FALSE,
        delete_dsn = TRUE,
        quiet = TRUE
      )
    } else {
      readr::write_csv(
        df, 
        file.path(
          dir_name,
          stringr::str_c(name, "csv", sep=".")
        ),
        append = FALSE
      )
    }
  }
}

center_xy <- function(sdf) {
  sdf |>
    dplyr::mutate(
      point = sf::st_point_on_surface(geometry),
      x = sf::st_coordinates(point)[,1],
      y = sf::st_coordinates(point)[,2]
    ) |>
    dplyr::select(-point)
}

prep_munis <- function(df) {
  df |>
    center_xy() |>
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

place_decision <- function(states, crs) {
  state_munis <- list()
  no_muni_st <- c()
  for (state in states) {
    if (state == "MA") {
      state_munis[[state]] <- get_ma_munis(crs)
    } else if (state == "ME") {
      state_munis[[state]] <- get_me_munis(crs)
    } else if (state == "NH") {
      state_munis[[state]] <- get_nh_munis(crs)
    } else if (state == "VT") {
      state_munis[[state]] <- get_vt_munis(crs)
    } else if (state == "CT") {
      state_munis[[state]] <- get_ct_munis(crs)
    } else if (state == "RI") {
      state_munis[[state]] <- get_ri_munis(crs)
    } else {
      no_muni_st <- append(no_muni_st, state)
    }
  }
  if (length(no_muni_st) > 0) {
    state_munis[["Other"]] <- get_places(states = no_muni_st, crs = crs)
  }
  dplyr::bind_rows(state_munis) |>
    prep_munis()
}

remove_coords <- function(df) {
  df |>
    dplyr::select(-dplyr::starts_with(c("x", "y")))
}