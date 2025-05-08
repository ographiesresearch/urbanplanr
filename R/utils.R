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

utils_unique_by_idx <- function(list, idx) {
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
    dplyr::group_by(.data[["state_name"]]) |>
    dplyr::summarize(
      counties = list(.data[["county_name"]])
    ) |>
    dplyr::ungroup() |>
    tibble::deframe()
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
        gdal = c(glue::glue("RASTER_TABLE={name}"), "APPEND_SUBDATASET=YES")
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

utils_write_named_list <- function(data,
                              dir_db,
                              format,
                              conn = NULL) {
  data |>
    purrr::imap(
      \(x,i) utils_write_multi(
        x, 
        name = i, 
        dir_db = dir_db, 
        format = format, 
        conn = conn
        )
    )
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

COUNTY_EQUIVS <- c(
  "COUNTY",
  # Louisiana
  "PARISH", 
  # Connecticut
  "PLANNING REGION", 
  # Alaska
  "BOROUGH", 
  "CENSUS AREA",
  # Puerto Rico
  "MUNICIPIO"
)

utils_parse_place <- function(x) {
  types <- COUNTY_EQUIVS |>
    stringr::str_c(collapse="|")
  
  find <- glue::glue(" ?({types})?, ?")
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace(find, ",") |>
    stringr::str_split(pattern = ",")
}

utils_place_states <- function(x) {
  is_state <- utils_is_state(x)
  is_place <- utils_is_place(x)
  if (is_state) {
    idx <- 1
  } else if (is_place) {
    idx <- 2
  } else {
    stop("Invalid place passed to `utils_place_states`")
  }
  utils_unique_by_idx(utils_parse_place(x), idx)
}

utils_filter_by_state <- function(df, places, col="abbrev") {
  df <- df |>
    dplyr::filter(
      .data[[col]] %in% utils_place_states(places)
    )
}

utils_filter_by_place <- function(df, places, cols=c("name", "state")) {
  is_sf <- "sf" %in% class(df)
  parsed <- utils_parse_place(places)
  df <- parsed |>
    purrr::map(\(i) dplyr::filter(
      df, 
      stringr::str_to_upper(.data[[cols[1]]]) == i[1] & 
        stringr::str_to_upper(.data[[cols[2]]]) == i[2]
      )
    ) |>
    purrr::list_rbind()
  if (is_sf) {
    df <- sf::st_as_sf(df)
  }
  df
}

utils_is_state <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_detect("^[A-Z]{2}$") |>
    all()
}

utils_is_place <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_detect("[A-Z ]+, ?[A-Z]{2}$") |>
    all()
}

utils_is_county <- function(x) {
  types <- COUNTY_EQUIVS |>
    stringr::str_c(collapse="|")
  
  x <- x |>
    stringr::str_to_upper()
  
  county <- x |>
    stringr::str_detect(glue::glue("( {types}),"))
  
  all(all(county) && utils_is_place(x))
}

utils_is_muni <- function(x) {
  all(!utils_is_county(x) && utils_is_place(x))
}

utils_is_filepath <- function(x) {
  length(x) == 1 &
    class(x) == "character" & 
    all(tools::file_ext(x) > 0)
}

utils_is_format <- function (x, formats) {
  x |>
    stringr::str_to_upper() |>
    tools::file_ext() %in% stringr::str_to_upper(formats) |>
    all()
}

utils_is_coords <- function(x) {
  x |> 
    purrr::map(\(c) 
               is.numeric(unlist(c)) & 
                 length(c) == 2 &
                 dplyr::between(unlist(c)[1], -90, 90) &
                 dplyr::between(unlist(c)[2], -180, 180)
    ) |> 
    unlist() |> 
    all()
}

utils_place_picker <- function(places, buffer = NULL, crs=4326) {
  if (!is.null(buffer)) {
    buffer <- units::as_units(buffer, "miles")
  }
  if (utils_is_filepath(places)) {
    if (utils_is_format(places, c("shp", "geojson"))) {
      if (file.exists(places)) {
        extent <- places |>
          sf::st_read() |> 
          st_preprocess(crs)
        if (!("id" %in% names(extent))) {
          extent <- extent |>
            tibble::rowid_to_column("id")
        }
        if (!("name" %in% names(extent))) {
          extent <- extent |>
            dplyr::mutate(name = .data[["id"]])
        }
      } else {
        stop("You passed a file path to places, but the file doesn't exist.")
      }
    } else {
      stop("Invalid file format.")
    }
  } else if (is.null(names(places))) {
    if (utils_is_state(places)) {
      extent <- places |>
        tigris_get_states(crs = crs) |>
        dplyr::mutate(type="state")
    } else if (utils_is_county(places)) {
      extent <- places |>
        tigris_get_counties(crs = crs) |>
        dplyr::mutate(type="county")
    } else if (utils_is_muni(places)) {
      extent <- places |>
        munis_get_munis(crs = crs) |>
        dplyr::mutate(type="muni")
    } else {
      stop("Invalid strings passed to places. Mixed state/county/muni lists are
           not accepted.")
    }
  } else {
    if (utils_is_coords(places)) {
      if (is.null(buffer)) {
        message("If you're passing in a coordinate, you should also pass in a
                value to buffer! Defaulting to a 1 mile buffer around the point")
        buffer <- units::as_units(1, "miles")
      }
      extent <- places |>
        purrr::imap(\(x, idx) 
                   st_point_from_coords(
                     c(x[[2]], x[[1]]), 
                     name = idx,
                     crs = crs
                   )) |>
        purrr::list_rbind() |>
        sf::st_as_sf() |>
        dplyr::mutate(type="coord")
    } else {
      stop("Invalid coordinates.")
    }
  }
  if (!is.null(buffer)) {
    extent <- extent |>
      sf::st_buffer(buffer)
  }
  extent
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