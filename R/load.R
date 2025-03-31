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
  message(glue::glue("Output format set to {config$format}."))
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

#' Write Dataframe to One or Multiple Common Output Formats
#'
#' @param df Dataframe or data frame extension (e.g., tibble).
#' @param name Character. Name of output table or file.
#' @param dir_name Character. Name of directory or database.
#' @param format Character vector including one or multiple of `"gpkg"`, 
#' `"postgis"`, `"dxf"`, or `"csv"`.
#'
#' @returns Original dataframe.
#' @export
utils_write_multi <- function(df, 
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
  } else if (format == "csv") {
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
  } else {
    stop("Invalid output format provided.")
  }
  df
}

#' Title
#'
#' @param url 
#' @param path 
#'
#' @returns
#' @export
#'
#' @examples
utils_get_remote <- function(url, path) {
  httr::GET(
    paste0(url), 
    httr::write_disk(path, overwrite = TRUE)
  )
}

#' Title
#'
#' @param path 
#' @param layer 
#'
#' @returns
#' @export
#'
#' @examples
utils_get_zipped_shp <- function(path, layer) {
  path <- stringr::str_c("/vsizip/", path, "/", layer)
  sf::st_read(path, quiet=TRUE)
}

#' Title
#'
#' @param dataset 
#'
#' @returns
#' @export
#'
#' @examples
utils_get_arc <- function(dataset) {
  prefix <- "https://opendata.arcgis.com/api/v3/datasets/"
  suffix <- "/downloads/data?format=geojson&spatialRefId=4326&where=1=1"
  sf::st_read(
    glue::glue("{prefix}{dataset}{suffix}")
  )
}

#' Title
#'
#' @param url 
#' @param layer 
#'
#' @returns
#' @export
#'
#' @examples
utils_get_remote_shp <- function(url, layer) {
  message(
    glue::glue("Downloading {shpfile} from {url}...")
  )
  temp <- base::tempfile(fileext = ".zip")
  utils_get_remote(
    url = url,
    path = temp
  )
  utils_get_zipped_shp(temp, layer)
}