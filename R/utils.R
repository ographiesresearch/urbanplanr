utils_slugify <- function(df, ..., col = "id", sep = "-") {
  cols <- rlang::enquos(...)
  df |>
    dplyr::mutate(
      !!col := df |>
        sf::st_drop_geometry() |>
        dplyr::select(!!!rlang::enquos(...)) |>
        purrr::pmap_chr(~ stringr::str_c(..., sep = sep) |> stringr::str_to_lower())
    )
}

utils_list_unique_by_index <- function(list, idx) {
  list |>
    purrr::map(idx) |>
    unlist() |>
    unique()
}

#' Extent to State/County
#'
#' @param extent `sf` object.
#'
#' @returns A named list of counties that the.extent intersects. Takes the form 
#' `list("MI" = c("Saginaw"))`.
#' @export
utils_extent_to_census <- function(extent) {
  c <-  COUNTIES |>
    sf::st_filter(
      extent |>
        sf::st_transform(sf::st_crs(COUNTIES))
      )
  if (nrow(c) == 0) {
    stop("Extent does not overlap with any US counties. 
    Currently this tool only supports US jurisdictions!")
  }
  c |>
    sf::st_drop_geometry() |>
    dplyr::group_by(state_name) |>
    dplyr::summarize(
      counties = list(county_name)
    ) |>
    dplyr::ungroup() |>
    tibble::deframe()
}

#' Initiate Environment
#'
#' @param config_name Name of configuration to read values from.
#' @param config_file Configuration file to read values from (config.yml is the 
#' default).
#'
#' @returns A list or vector as returned by `config()`.
#' @export
utils_init_env <- function(config_name = "default", 
                           config_file = "config.yml") {
  if(file.exists(".env")) {
    dotenv::load_dot_env()
    tidycensus::census_api_key(Sys.getenv("CENSUS_API"))
  }
  
  config::get(config=config_name, file=config_file)
}

#' Standardize Output File Format
#'
#' @param format A common name of shapefiles, geopackages, geojsons, or postgis.
#'
#' @returns Standardized character.
#' @export
utils_std_output_format <- function(format) {
  if (format %in% c("shapefile", "shp")) {
    format <- "shp"
  } else if (format %in% c("geopackage", "gpkg")) {
    format <- "gpkg"
  } else if (format %in% c("geojson", "json")) {
    format <- "geojson"
  } else if (format == "postgis") {
    # do nothing.
  }else {
    stop("'format' parameter must be one of 'postgis', 'shp', 'gpkg', or 'geojson'.")
  }
  format
}

#' Boolean Checking Prompt
#'
#' Little helper that prompts a user and accepts binary responses.
#'
#' @param prompt Text of user prompt.
#'
#' @returns Boolean user response.
#' @export
utils_prompt_check <- function(prompt) {
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
    utils_prompt_check(prompt)
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

utils_write_pg_raster <- function(raster, name, dbname, host, role, pass, port) {
  conn <- RPostgreSQL::dbConnect(
    drv = DBI::dbDriver("PostgreSQL"),
    dbname = dbname,
    host = host,
    user = role,
    pass = pass,
    port = port
  )
  on.exit(RPostgreSQL::dbDisconnect(conn))
  
  rpostgis::pgWriteRast(
    conn=conn, 
    name=name, 
    raster=raster, 
    overwrite=TRUE
  )
  raster
}

#' Write Dataframe to One or Multiple Common Output Formats
#'
#' @param df Dataframe or data frame extension (e.g., tibble).
#' @param name Character. Name of output table or file.
#' @param dir_db Character. Name of directory or database.
#' @param format Character vector including one or multiple of `"gpkg"`, 
#' `"postgis"`, `"dxf"`, or `"csv"`.
#'
#' @returns Original dataframe.
#' @export
utils_write_multi <- function(data, 
                        name, 
                        dir_db, 
                        format,
                        conn = NULL) {
  message(glue::glue("Writing {name}."))
  raster <- "SpatRaster" %in% class(data)
  sf <- "sf" %in% class(data)
  if (format == "gpkg") {
    if (!raster) {
      sf::st_write(
        obj = data,
        dsn = stringr::str_c(dir_db, format, sep="."),
        layer = name,
        append = FALSE,
        delete_layer = TRUE,
        quiet = TRUE
      )
    } else {
      sf::st_delete(
        dsn = stringr::str_c(dir_db, format, sep="."),
        layer = name,
        driver = "GPKG",
        quiet = FALSE
        )
      terra::writeRaster(
        x = data,
        filename = stringr::str_c(dir_db, format, sep="."),
        filetype = "GPKG",
        gdal = c(glue::glue("RASTER_TABLE={name}"), "APPEND_SUBDATASET=YES", "OVERWRITE=YES")
      )
    }
  } else if (format == "postgis") {
    if (!raster) {
      sf::st_write(
        obj = data,
        dsn = conn,
        layer = name,
        append = FALSE,
        delete_layer = TRUE,
        quiet = TRUE
      )
    } else {
      rpostgis::pgWriteRast(
        conn = conn, 
        name = name, 
        raster = data, 
        overwrite = TRUE
      )
    }
  } else if (format == "dxf") {
    dir.create(dir_db, showWarnings = FALSE)
    if (!raster) {
      sf::st_write(
        obj = data,
        dsn = file.path(
          dir_db,
          stringr::str_c(dir_db, format, sep = ".")
        ),
        name = name,
        driver = format,
        append = FALSE,
        delete_layer = TRUE,
        quiet = TRUE
      )
    }
    else {
      terra::writeRaster(
        x = data, 
        filename = file.path(
          dir_db,
          stringr::str_c(name, "tif", sep = ".")
        ),
        overwrite = TRUE
      )
    }
  } else if (format == "csv") {
    dir.create(dir_db, showWarnings = FALSE)
    if (!raster) {
      sf::st_write(
        obj = data,
        dsn = file.path(
          dir_db,
          stringr::str_c(name, format, sep = ".")
        ),
        append = FALSE,
        delete_dsn = TRUE,
        quiet = TRUE
      )
    } else if (!sf) {
      readr::write_csv(
        data, 
        file.path(
          dir_db,
          stringr::str_c(name, "csv", sep = ".")
        ),
        append = FALSE
      )
    }
    else if (raster) {
      terra::writeRaster(
        x = data, 
        filename = file.path(
          dir_db,
          stringr::str_c(name, "tif", sep = ".")
        ),
        overwrite = TRUE
      )
    }
  } else {
    stop("Invalid output format provided.")
  }
  data
}

#' Get Remote File and Write to Disk
#'
#' @param url URL of remote file.
#' @param path Path to saved file.
#'
#' @returns A `response()` object.
#' @export
utils_get_remote <- function(url, path) {
  httr::GET(
    paste0(url), 
    httr::write_disk(path, overwrite = TRUE)
  )
}

#' Read Single Shapefile from `.zip` File
#' 
#' Given a `.zip` file that conceivably contains multiple shapefiles, read only
#' one of them.
#'
#' @param path Path to `.zip` file.
#' @param layer Name of shapefile to read.
#'
#' @returns Object of class `sf`.
#' @export
utils_get_zipped_shp <- function(path, layer) {
  path <- stringr::str_c("/vsizip/", path, "/", layer)
  sf::st_read(path, quiet=TRUE)
}

#' Read Remote ArcGIS Open Data Layer
#'
#' @param id Character. ArcGIS online ID.
#'
#' @returns Object of class `sf`.
#' @export
utils_get_arc <- function(id) {
  prefix <- "https://opendata.arcgis.com/api/v3/datasets/"
  suffix <- "/downloads/data?format=geojson&spatialRefId=4326&where=1=1"
  sf::st_read(
    glue::glue("{prefix}{id}{suffix}")
  )
}

utils_parse_place_state <- function(ps) {
  upper <- ps |>
    stringr::str_to_upper() |>
    stringr::str_replace("( COUNTY)?, ", ",")
  
  parsed <- upper |>
    stringr::str_split(pattern = ",")
  parsed_length <- purrr::map(parsed, length)
  places <- NULL
  if (all(parsed_length == 2)) {
    elem <- 2
    places <- utils_list_unique_by_index(parsed, elem - 1)
  } else if (all(parsed_length == 1)) {
    elem <- 1
  } else {
    stop("Misspecified places input. Should be either a list of states or a list
         of counties/municipalities in the form c('Saginaw, MI',...).")
  }
  states <- utils_list_unique_by_index(parsed, elem)
  if (!all(states %in% STATES$state_abbrev)) {
    stop("Provided states do not exist.")
  }
  
  list(
    upper = upper,
    parsed = parsed,
    states = states,
    places = places
  )
}

#' Read Remote Shapefile Stored in `.zip` File
#'
#' @param url URL of remote file containing shapefiles.
#' @param layer Name of Shapefile to read.
#'
#' @returns Object of class `sf`.
#' @export
utils_get_remote_shp <- function(url, layer) {
  temp <- base::tempfile(fileext = ".zip")
  utils_get_remote(
    url = url,
    path = temp
  )
  utils_get_zipped_shp(temp, layer)
}