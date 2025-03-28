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
  } else if (format == "dxf") {
    conn <- db_create_conn(dir_name, admin=TRUE)
    on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
    sf::st_write(
      df,
      name,
      driver = "dxf",
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
  )
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
  get_shp_from_zip(temp, shpfile)
}