#' Detect UTM Zone
#'
#' @param df Simple features data frame or tibble.
#'
#' @returns Object of class `crs`.
#' @export
#'
detect_utm <- function(df) {
  utm_zone <- df |>
    st_transform(
      # `utm_zones` is from `R/sysdata.Rda`
      sf::st_crs(utm_zones)
    ) |>
    sf::st_join(
      utm_zones,
      sf::st_intersects,
      largest = TRUE
    ) |>
    dplyr::pull(zone_num)
  
  center <- df |>
    sf::st_transform(4326) |>
    sf::st_coordinates()
  
  # UTM convention is that unmarked zones are
  # N, S zones are marked S (because Eurocentrism).
  # Here, we test if latitude > 0 to determine
  # whether it is N or S.
  if(center[1,2] < 0){
    utm_zone <- base::paste0(utm_zone, "S")
  }
  
  # Set up projection using custom proj4string.
  sf::st_crs(
    base::paste0(
      "+proj=utm +zone=",
      utm_zone,
      " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    )
  )
}

#' Add Z Dimension to Points from Column
#'
#' @param df Simple features data frame or tibble.
#' @param zcol Character. Name of column containing Z values.
#'
#' @returns Simple features data frame or tibble.
#' @export
#'
elevate_points <- function(df, zcol) {
  df |>
    dplyr::mutate(
      coords = sf::st_coordinates(geom),
      x = coords[,"X"],
      y = coords[,"Y"]
    ) |> 
    sf::st_drop_geometry() |>
    sf::st_as_sf(coords=c("x", "y", zcol)) |>
    dplyr::select(-c(coords))
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

db_create_conn <-function(dbname, admin=FALSE) {
  dotenv::load_dot_env()
  if (admin) {
    user <- Sys.getenv("DB_ADMIN_USER")
    password <- Sys.getenv("DB_ADMIN_PASS")
  }
  else {
    user <- Sys.getenv("DB_USER")
    password <- Sys.getenv("DB_PASS")
  }
  RPostgres::dbConnect(
    drv=RPostgres::Postgres(),
    dbname=dbname,
    host=Sys.getenv("DB_HOST"),
    port=Sys.getenv("DB_PORT"),
    password=password,
    user=user
  )
}

db_exists <- function(dbname) {
  conn <- db_create_conn("postgres", admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add=TRUE)
  
  exists <- dbname %in% RPostgres::dbGetQuery(
    conn, 
    glue::glue("SELECT datname FROM pg_database WHERE datname = '{dbname}';")
    )$datname
  if (exists) {
    message(glue::glue("Database '{dbname}' exists!"))
  } else {
    message(glue::glue("Database '{dbname}' does not exist."))
  }
  exists
}

db_create_postgis <- function(dbname) {
  conn <- db_create_conn(dbname, admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
  
  RPostgres::dbExecute(conn, "CREATE EXTENSION postgis;")
  message(
    glue::glue("Created PostGIS extension on database '{dbname}'.")
  )
}

db_drop <- function(dbname) {
  conn <- db_create_conn("postgres", admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add=TRUE)
  
  RPostgres::dbExecute(
    conn, 
    glue::glue("DROP DATABASE IF EXISTS {dbname};")
  )
  
}

db_create <- function(dbname) {
  conn <- db_create_conn("postgres", admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add=TRUE)
  
  RPostgres::dbExecute(
    conn, 
    glue::glue("CREATE DATABASE {dbname};")
    )
  
  message(
    glue::glue("Created database '{dbname}'.")
  )
}

db_role_exists <- function(role) {
  conn <- db_create_conn("postgres", admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add=TRUE)
  
  exists <- role %in% RPostgres::dbGetQuery(
    conn, 
    glue::glue("SELECT rolname FROM pg_roles WHERE rolname = '{role}';")
  )$rolname
  
  if (exists) {
    message(glue::glue("Role '{role}' exists!"))
  } else {
    message(glue::glue("Role '{role}' does not exist."))
  }
  exists
}

db_role_create <- function(role, pass) {
  conn <- db_create_conn("postgres", admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
  
  RPostgres::dbExecute(
    conn, 
    glue::glue("CREATE ROLE {role} WITH LOGIN PASSWORD '{pass}';")
  )
  message(
    glue::glue("Role '{role}' created.")
  )
}

db_grant_access <- function(dbname, role) {
  conn <- db_create_conn("postgres", admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
  
  RPostgres::dbExecute(
    conn, 
    glue::glue("GRANT CONNECT ON DATABASE {dbname} TO {role};")
    )
  message(
    glue::glue("Granted CONNECT privilege on database '{dbname}' to role '{role}'.")
  )
}

db_set_defaults <- function(dbname, role) {
  conn <- db_create_conn(dbname, admin=TRUE)
  on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
  
  RPostgres::dbExecute(
    conn, 
    glue::glue("ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO {role};")
    )
  message(
    glue::glue("Set default SELECT privileges in '{dbname}' to role '{role}'.")
  )
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

db_create_if <- function(dbname, role, pass) {
  if (db_exists(dbname)) {
    overwrite <- prompt_check("Would you like to overwrite the database?")
    if (overwrite) {
      db_drop(dbname)
      db_create(dbname)
      db_create_postgis(dbname)
    } else {
      message(
        glue::glue("User chose to not overwrite database '{dbname}'.")
        )
    }
  } else {
    db_create(dbname)
    db_create_postgis(dbname)
  }
  
  if (!db_role_exists(role)) {
    db_role_create(role, pass)
    db_grant_access(dbname, role)
    db_set_defaults(dbname, role)
  } else {
    db_grant_access(dbname, role)
    db_set_defaults(dbname, role)
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

get_remote_zip <- function(url, path) {
  httr::GET(
    paste0(url), 
    httr::write_disk(path, overwrite = TRUE)
  )
}

read_shp_from_zip <- function(path, layer) {
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

get_shp_from_remote_zip <- function(url, shpfile, crs) {
  message(
    glue::glue("Downloading {shpfile} from {url}...")
    )
  temp <- base::tempfile(fileext = ".zip")
  get_remote_zip(
    url = url,
    path = temp
  )
  read_shp_from_zip(temp, shpfile) |>
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
