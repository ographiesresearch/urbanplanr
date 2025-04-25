#' Check if Layer Projection Matches CRS and Transform if Not
#'
#' @param df `sf` object
#' @param crs EPSG code or `crs` object.
#'
#' @returns Reprojected `sf` object.
#' @export
st_check_for_proj <- function(df, crs=4326) {
  if (sf::st_crs(df)$epsg != crs) {
    df <- df |>
      sf::st_transform(crs)
  }
  df
}

#' Bounding Box as `sf` Object
#'
#' Given an input `sf` object, returns that object's bounding box as an `sf`
#' object.
#' 
#' @param df An `sf` object.
#'
#' @returns An `sf` object.
#' @export
st_bbox_sf <- function(df) {
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      geometry = sf::st_as_sfc(sf::st_bbox(geometry))
      ) |>
    dplyr::ungroup()
}

#' Create a Point Dataframe Row from Coordinate Pair
#'
#' @param coords vector of longitude, latitude coordinates (in EPSG:4326, unless
#' coord_crs is changed).
#' @param crs output `crs`
#' @param name Optional. Name to give point (in name field).
#' @param coord_crs `crs` of provided points. 4326 default.
#'
#' @returns `sf` object
#' @export
st_point_from_coords <- function(coords, crs, name = NULL, coord_crs=4326) {
  df <- sf::st_point(x=coords, dim="XY") |>
    sf::st_sfc(crs=coord_crs) |>
    sf::st_as_sf() |>
    sf::st_set_geometry("geometry") |>
    sf::st_transform(crs)
  
  if(!is.null(name)) {
    df <- df |>
      dplyr::mutate(
        name = name
      )
  }
  df
}

st_get_extent <- function(crs, point = NULL, path = NULL, places = NULL) {
  if (!is.null(point)) {
    if (all(c("coords", "dist") %in% names(point))){
      extent <- c(point$coords[2], point$coords[1]) |>
        st_point_from_coords(
          crs = 4326
        ) |>
        sf::st_buffer(
          dist = units::as_units(point$dist, "miles")
        )
      if ("name" %in% names(point)) {
        extent <- extent |>
          dplyr::mutate(
            name = point$name
          )
      }
    } else {
      stop(
        glue::glue("utils_get_study_area(): Point requires both coords and dist.")
      )
    }
  } else if (!is.null(path)) {
    format <- tools::file_ext(path)
    if (format == "shp" | format == "geojson") {
      extent <- sf::st_read(path)
    } else {
      stop(
        glue::glue("utils_get_study_area(): Invalid file format ({format}) passed to path.")
      )
    }
  } else if (!is.null(places)) {
    if (all(c("names", "type") %in% names(places))) {
      places_parse <- places$names |>
        stringr::str_to_upper() |>
        stringr::str_split(pattern = "( COUNTY)?, ?")
      if (places$type == "muni") {
        states <- utils_list_unique_by_index(places_parse, 2)
        munis <- utils_list_unique_by_index(places_parse, 1)
        extent <- states |>
          munis_get(
            crs = crs,
            munis = munis
          )
      } else if (places$type == "state") {
        states <- utils_list_unique_by_index(places_parse, 1)
        extent <- states |>
          tigris_get_states(
            crs = crs
          )
      } else if (places$type == "county") {
        states <- utils_list_unique_by_index(places_parse, 2)
        counties <- utils_list_unique_by_index(places_parse, 1)
        extent <- states |>
          tigris_get_counties(
            counties = counties,
            crs = crs
          )
      } else {
        stop(
          glue::glue("utils_get_study_area(): place type must be state, county or muni.")
        )
      }
      if(nrow(extent) < length(places$names)) {
        stop(glue::glue("Found no matches for at least one of {stringr::str_c(places$names, collapse = ',')}"))
      } else if (nrow(extent) > length(places$names)) {
        stop(glue::glue("Non-unique matches for at least one of {stringr::str_c(places$names, collapse = ',')}"))
      }
    } else {
      stop(
        glue::glue("utils_get_study_area(): place requires names, type params.")
      )
    }
  }
  extent |>
    st_preprocess(crs)
}

#' Determine Zoom Level from Extent and Requested Tile Resolution
#'
#' @param extent `sf` object
#' @param tiles_on_side Numeric. Tile 'resolution' (i.e., number of map tiles on
#' the longest side of the extent's bounding box). Must be two raised to a power
#' or 1.
#'
#' @returns An integer zoom level.
st_zoom_from_extent <- function(extent, tiles_on_side = 2) {
  
  if (!(log2(tiles_on_side) %% 1 == 0)) {
      stop("`tiles_on_side` must be two to a power or 1.")
  }
  
  extent <- extent |>
    st_check_for_proj(4326)
  
  bbox <- sf::st_bbox(extent)
  
  w <- bbox["xmax"] - bbox["xmin"]
  h <- bbox["ymax"] - bbox["ymin"]
  
  h_scale <- cos(
    mean(c(bbox["ymax"], bbox["ymin"])) * (pi / 180)
  )
  
  zoom_w <- log2(360 / w)
  zoom_h <- log2(180 / (h * h_scale))
  
  floor(min(zoom_w, zoom_h)) + log2(tiles_on_side)
}

#' Pre-processing operations for spatial data.
#'
#' @param df Simple features dataframe.
#' @param crs EPSG code or `crs` object.
#' @param name Character. Name of geometry column. `"geometry"` is default.
#'
#' @returns Simple Features dataframe.
#' @export
#'
st_preprocess <- function(df, crs, name="geometry") {
  df |> 
    sf::st_transform(crs) |>
    dplyr::rename_with(tolower) |>
    sf::st_set_geometry(name) |>
    st_geom_to_xy(retain_geom = TRUE)
}

#' Get DEM from AWS Terrain Tiles
#'
#' @param extent Simple features data frame or tibble (ideally polygon). Must be
#' projected and in linear units.
#' @param tiles_on_side Number of tiles on one side of extent. Must be the
#' result of 2 raised to a power or 1.
#' @param expand 	A numeric value of a distance, in map units, used to expand 
#' the bounding box that is used to fetch the terrain tiles. 
#' @param z Number. Zoom level. If specified, `tiles_on_side` is ignored. If not
#' specified, the result of `st_zoom_from_extent(extent, tiles_on_side)`. Note
#' that `elevatr::get_elev_raster()` has a max `z` of 14.
#' @param src One of "aws", "gl3", "gl1", "alos", "srtm15plus".
#'
#' @returns A `RasterLayer`.
#' @export
#'
st_get_dem <- function(extent, 
                       tiles_on_side = 8, 
                       expand=NULL,
                       z = st_zoom_from_extent(
                         extent = extent, 
                         tiles_on_side = tiles_on_side
                       ), 
                       src = "aws") {
  
  # get_elev_master has a max z of 14.
  z <- min(c(14,z)) 
  
  dem <- extent |>
    elevatr::get_elev_raster(
      z=z,
      src=src,
      clip="bbox",
      expand=expand,
      neg_to_na=TRUE,
      verbose=FALSE
    ) |>
    terra::rast()
  
  terra::set.crs(dem, glue::glue("epsg:{sf::st_crs(extent)$epsg}"))
  
  min_res <- 256 * tiles_on_side
  
  fact <- ceiling(min_res / terra::ncol(dem))
  
  if (fact > 1) {
    dem <- terra::disagg(dem, fact = fact, method = "bilinear")
  }
  dem
}

#' Calculate a Hillshade from a Digital Elevation Model.
#'
#' Can be used to calcualte a hillshade from a DEM `RasterLayer` of the type
#' returned by `get_dem()`.
#'
#' @param dem A digital elevation model stored as a `RasterLayer`.
#' @param angle Elevation angle of the light source (degrees). Defaults to 45.
#' @param direction Azimuth angle of the light source (degrees). Defaults to
#' 300.
#' @param z_scale How much to exaggerate the z scale. Default is 1. Values of 
#' 3-4 are fairly standard in cartographic practice.
#' @param normalize Logical. If `TRUE` (default), values below zero are set to
#' zero and the results are multiplied by 255.
#' @param overwrite Logical. If `TRUE` (and `filename` is set), overwrites
#' existing raster file.
#' @param filename Character. Optional output filename.
#'
#' @returns A `RasterLayer`.
#' @export
st_hillshade <- function(dem,
                         angle = 45,
                         direction = 300,
                         z_scale = 1,
                         normalize = TRUE,
                         overwrite = TRUE,
                         filename = "") {
  if (z_scale > 1) {
    dem <- dem * z_scale
  } else {
    stop("Invalid z_scale. Must be >= 1.")
  }
  terra::shade(
    slope = terra::terrain(dem, v = "slope", unit = "radians"),
    aspect = terra::terrain(dem, v = "aspect", unit = "radians"),
    angle = angle,
    direction = direction,
    filename = filename,
    normalize = normalize,
    overwrite = overwrite
  )
}
#' Contours from Raster
#' 
#' Generally used to create contour lines from a digital elevation model (DEM).
#'
#' @param raster `RasterLayer`, for example returned by `st_get_dem()`.
#' @param interval Numeric. Elevation interval at which contours should be 
#' drawn. In units of crs.
#' @param maxcells Maximum number of raster cells to use. Reduce if function is 
#' failing.
#' @param threshold_length Threshold length below which contours will be 
#' dropped. Helps to remove shards. Defaults to `units::as_units(250, "m").`
#'
#' @returns `sf` object with `LINESTRING` geometries.
#' @export
st_contours <- function(raster, 
                       interval,
                       maxcells = terra::ncell(raster) / 4,
                       threshold_length = units::as_units(250, "m")
                       ) {
  raster |>
    terra::as.contour(
      maxcells = maxcells,
      levels = seq(
        from = floor(terra::minmax(raster)[1]),
        to = ceiling(terra::minmax(raster)[2]),
        by = interval
      )
    ) |>
    sf::st_as_sf() |>
    dplyr::rename(
      z = "level"
    ) |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("LINESTRING") |>
    dplyr::mutate(
      length = sf::st_length(.data$geometry)
    ) |>
    dplyr::filter(
      .data$length > threshold_length
    )
}

#' Enclose Contours Using Polygon Features
#'
#' @param contours `sf` object, as returned by `st_contour()`.
#' @param enclosure_layer Feature layer used to enclose contour lines.
#' @param bbox Boolean. If `FALSE` (default), contours are enclosed by feature
#' boundaries. If `TRUE`, contours are enclosed by their bounding box.
#' @param poly Boolean. If `FALSE` (default), returns `MULTILINESTRING`s. If 
#' `TRUE`, returns `MULTIPOLYGON`s.
#'
#' @returns `sf` object, `POLYGON` geometries.
#' @export
st_contours_enclose <- function(
    contours, 
    enclosure_layer,
    poly = FALSE,
    bbox = FALSE
    ) {
  if (bbox) {
    enclosure_layer <- st_bbox_sf(enclosure_layer)
  }
  
  message(sf::st_crs(contours)$epsg)
  message(sf::st_crs(enclosure_layer)$epsg)
  if(poly) {
    out_geom <- "MULTIPOLYGON"
  } else {
    out_geom <- "MULTILINESTRING"
  }
  
  enclosed <- enclosure_layer |>
    sf::st_union() |>
    sf::st_as_sf() |>
    lwgeom::st_split(contours) |>
    sf::st_collection_extract("POLYGON") |>
    sf::st_set_geometry("geometry") |>
    tibble::rowid_to_column("id")
  
  z <- enclosed |> 
    sf::st_join(contours) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(.data$id) |>
    dplyr::summarize(
      z = as.integer(min(.data$z))
    ) |>
    dplyr::ungroup()
  
  enclosed |>
    dplyr::left_join(z, by = c("id" = "id")) |>
    dplyr::group_by(.data$z) |>
    dplyr::summarize(
      geometry = sf::st_cast(sf::st_union(.data$geometry), out_geom)
    ) |>
    dplyr::ungroup()
}

#' Elevate Contours and Finish
#' 
#' This function is primarily used in the case where the user is preparing a
#' stepped contour model in CAD (or using a laser cutter).
#'
#' @param enclosed_contours `sf` object, as returned by `st_contours_close()`
#' @param full_plates If `TRUE` (default), contours extended to the edge of the
#' model. If `FALSE`, they will emeerge in 'strips'.
#' @param etch_guides If `TRUE`, guides are created by etching edge of the above
#' level into each level. If `FALSE` (default), no guides created. Requires that
#' `poly` and `full_plates` both be `TRUE`.
#'
#' @returns `sf` object.
#' @export
st_contours_model <- function(
    enclosed_contours, 
    full_plates = TRUE,
    etch_guides = FALSE
    ) {
  
  if (full_plates) {
    enclosed_contours <- enclosed_contours |>
      dplyr::arrange(dplyr::desc(.data$z)) |>
      dplyr::mutate(
        geometry = sf::st_as_sfc(
          purrr::accumulate(
            .data$geometry,
            sf::st_union
            ), 
          crs = sf::st_crs(enclosed_contours)
          )
      )
  }
  if (etch_guides & full_plates) {
    enclosed_contours <- enclosed_contours |>
      dplyr::bind_rows(
        enclosed_contours |>
          dplyr::mutate(
            z = dplyr::lead(.data$z)
          ) |>
          tidyr::drop_na("z")
      )
  }
  enclosed_contours
}

#' GIS-esque Clip Function
#'
#' @param x Layer to be clipped.
#' @param y Layer to do the clipping.
#'
#' @returns Layer `x` clipped to `y`.
#' @export
#'
st_clip <- function(x, y) {
  init_type <- as.character(sf::st_geometry_type(x, by_geometry=FALSE))
  clip <- x |>
    sf::st_set_agr("constant") |>
    sf::st_intersection(
      y |>
        sf::st_union() |>
        sf::st_geometry()
    ) |>
    dplyr::filter(
      as.character(sf::st_geometry_type(.data$geometry)) %in% c(
        init_type, 
        stringr::str_c("MULTI", init_type, sep=""),
        stringr::str_remove(init_type, "MULTI")
      )
    )
  
  clip_type_multi <- base::any(
    stringr::str_detect(
      clip |>
        sf::st_geometry_type(by_geometry=TRUE) |>
        as.character() |>
        base::unique(), 
      "MULTI"
    )
  )
  
  init_type_multi <- stringr::str_detect(init_type, "MULTI")
  
  if (clip_type_multi & !init_type_multi) {
    cast_to <- stringr::str_c("MULTI", init_type, sep="")
  } else {
    cast_to <- init_type
  }
  
  clip |>
    sf::st_cast(cast_to)
}

#' Retrieve Scaling Parameters, Given Model Size
#'
#' @param df Simple features dataframe or tibble.
#' @param model_size Optional. Largest dimension of model, as numeric or
#' `units`. If numeric, treated as map units. If not provided, model will not be
#' scaled.
#'
#' @returns Named list with `factor`, `x_delta`, `y_delta`.
#'
st_scale_to_model <- function(df, model_size = NULL) {
  if (!is.null(model_size)) {
    bbox <- sf::st_bbox(df)
    x_size <- abs(bbox$xmax - bbox$xmin)
    y_size <- abs(bbox$ymax - bbox$ymin)
    x_center <- as.numeric(bbox$xmax - (x_size / 2))
    y_center <- as.numeric(bbox$ymax - (y_size / 2))
    
    factor <- as.numeric(units::set_units(model_size, "m")) / max(x_size, y_size)
  } else {
    x_center <- 0
    y_center <- 0
    factor <- 1
  }
  c(factor = factor,
    x_delta = x_center,
    y_delta = y_center)
}

#' Scale Layer Based on Scaling Parameters
#'
#' @param df Simple features dataframe or tibble.
#' @param scale Scaling parameters, as returend by `scale_to_model()`.
#' @param thickness Numeric. Thickness of model plywood sheets.
#' @param contour_int Nuemeric. Intervals for contours.
#'
#' @returns Scaled simple features dataframe or tibble.
#' @export
#'
st_model_scale <- function(df, scale, thickness, contour_int) {
  crs <- sf::st_crs(df)
  scaled <- (sf::st_geometry(df) - c(scale["x_delta"], scale["y_delta"])) * scale["factor"]
  
  if (!("sfc" %in% class(df))) {
    sf::st_geometry(df) <- scaled
    if ("z" %in% colnames(df)) {
      df <- df |>
        dplyr::mutate(
          z = (.data$z / (contour_int)) * as.numeric(units::set_units(thickness, "m"))
        )
    }
  }
  df <- df |>
    sf::st_set_crs(crs)
  df
}

#' Detect UTM Zone Based on Location
#'
#' @param df Simple features data frame or tibble.
#'
#' @export
#'
st_detect_utm <- function(df) {
  zone <- df |>
    sf::st_transform(
      sf::st_crs(UTM_ZONES)
      ) |>
    sf::st_join(UTM_ZONES, sf::st_intersects, largest = TRUE) |>
    dplyr::pull("zone_num")
  
  center <- df |>
    sf::st_transform(4326) |>
    sf::st_coordinates()
  
  # UTM convention is that unmarked zones are
  # N, S zones are marked S (because Eurocentrism).
  # Here, we test if latitude > 0 to determine
  # whether it is N or S.
  if (center[1, 2] < 0) {
    zone <- base::paste0(zone, "S")
  }
  
  # Set up projection using custom proj4string.
  sf::st_crs(
    base::paste0(
      "+proj=utm +zone=",
      zone,
      " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    )
  )
}

#' Make Line from Coordinate Pair
#'
#' @param xyxy Vector of coordinate pair in the form c(x1, y1, x2, y2)
#'
#' @return A LINESTRING.
#'
#' @export
#'
st_make_line <- function(xyxy) {
  sf::st_linestring(base::matrix(xyxy, nrow = 2, byrow = TRUE))
}

#' XY-XY To Lines
#'
#' Takes a dataframe with four columns containing two XY pairs (in the form 
#' `c('x_h`, `y_h`, `x_w`, `y_w)`) and returns an sf dataframe with the same 
# number of rows with those coordinates converted into linestrings.
#'
#' @param df A dataframe with four columns containing two XY pairs.
#' @param names list of the names of the four columns in the form 
#' `c("x_h", "y_h", "x_w", "y_w")`.
#' @param retain_cols Boolean. If `FALSE` (the default), do not retain the
#' OD coordinate columns.
#'
#' @return A dataframe with LINESTRING geometries.
#'
#' @export
#'
st_xyxy_to_lines <- function(df,
                             names = c("x_h", "y_h", "x_w", "y_w"),
                             retain_cols = FALSE) {
  df <- df |>
    sf::st_sf(
      geometry = df |>
        dplyr::select(dplyr::all_of(names)) |>
        base::apply(1, st_make_line, simplify = FALSE) |>
        sf::st_sfc(crs = sf::st_crs(df))
    )
  
  if (!retain_cols) {
    df <- df |>
      dplyr::select(-dplyr::all_of(names))
  }
  df
}

#' Tests `sf` Dataframes for Geometry Types
#' @name st_is_type
#' 
#' @description
#' `st_is_type()` returns `TRUE` if `type` matches geometry type returned by 
#' `sf::st_geometry_type()`.
#' 
#' `st_is_multipolygon()` returns `TRUE` if geometry column contains 
#' MULTIPOLYGONS.
#' 
#' `st_is_polygon()` returns `TRUE` if geometry column contains 
#' POLYGONS (or MULTIPOLYGONS, if `exact` is FALSE).
#'
#' `st_is_multilinestring()` returns `TRUE` if geometry column contains 
#' MULTILINESTRINGS.
#'
#' `st_is_linestring()` returns `TRUE` if geometry column contains 
#' LINESTRINGS (or MULTILINESTRINGS if `exact` is FALSE).
#'
#' `st_is_multipoint()`  returns `TRUE` if geometry column contains 
#' MULTIPOINTS.
#'
#' `st_is_point()` returns `TRUE` if geometry column contains 
#' POINTS (or MULTIPOINTS if `exact` is FALSE).
#'
#' @param df An `sf` dataframe.
#' @param type Character. Type to match.
#' @param exact Boolean. If `FALSE` (default), returns unexact matches. (E.g., 
#' `"POLYGON"` will also match `"MULTIPOLYGON".`)
#' @param ... Additional arguments passed to `st_is_type()`.
#'
#' @returns Boolean.
#' @export
#'
st_is_type <- function(df, type, exact=FALSE) {
  df_type <- sf::st_geometry_type(df, by_geometry=FALSE)
  if (exact) {
    type <- stringr::str_c("^", type, "$")
  }
  stringr::str_detect(df_type, type)
}

#' @name st_is_type
#' @export
st_is_multipolygon <- function(df, ...) {
  st_is_type(df = df, type = "MULTIPOLYGON", ...)
}

#' @name st_is_type
#' @export
st_is_polygon <- function(df, ...) {
  st_is_type(df = df, type = "POLYGON", ...)
}

#' @name st_is_type
#' @export
st_is_multilinestring <- function(df, ...) {
  st_is_type(df = df, type = "MULTILINESTRING", ...)
}

#' @name st_is_type
#' @export
st_is_linestring <- function(df, ...) {
  st_is_type(df = df, type = "LINESTRING", ...)
}

#' @name st_is_type
#' @export
st_is_multipoint <- function(df, ...) {
  st_is_type(df = df, type = "MULTIPOINT", ...)
}

#' @name st_is_type
#' @export
st_is_point <- function(df, ...) {
  st_is_type(df = df, type = "POINT", ...)
}

#' Return Geometry Centers for Multiple Types.
#'
#' @param df A dataframe with point geometries.
#' @param on_surface If `TRUE` (the default), generates polygon centroids using 
#' `st_point_on_surface()`. If `FALSE`, uses `st_centroid()`.
#' @param retain_geom Boolean. If `TRUE` (the default), returns points. If 
#' `FALSE` preserves geometry type of input. 
#'
#' @returns An `sf` dataframe.
#' @export
#'
st_multi_type_center <- function(df,
                                 on_surface = TRUE,
                                 retain_geom = FALSE) {
  
  if (retain_geom) {
    center_col <- "center"
  } else {
    center_col <- attr(df, "sf_column")
  }
  if (st_is_polygon(df) & on_surface) {
    df <- df |>
      dplyr::mutate(
        "{center_col}" := sf::st_point_on_surface(sf::st_geometry(df))
        )
  } else if (st_is_linestring(df) |
             (st_is_polygon(df) & !on_surface)) {
    df <- df |>
      dplyr::mutate(
        "{center_col}" := sf::st_centroid(sf::st_geometry(df))
      )
  }
  df |>
    sf::st_set_geometry(center_col)
}

#' Geometry to XY Columns
#'
#' Takes a simple features dataframe containing geometries and returns
#' the frame with additional columns containing x and y coordinates. If
#' `retain_geom` is TRUE, returns with original (non-point) geometry.
#'
#' @param df A dataframe with point geometries.
#' @param cols Character vector of names for x and y columns. (Default 
#' `c("x", "y")`.)
#' @param retain_geom Boolean. If `FALSE` (default), returns point geometry. If
#' `TRUE`, returns original geometries 
#' @param ... Arguments passed on to `st_multi_type_center()`.
#'
#' @return Simple features dataframe.
#'
#' @export
#'
st_geom_to_xy <- function(df, 
                          cols = c("x", "y"), 
                          crs = 4326,
                          retain_geom=FALSE, 
                          ...
                          ) {
  init_geo_col <- attr(df, "sf_column")
  init_crs <- sf::st_crs(df)
  
  df <- df |> 
    st_multi_type_center(retain_geom=retain_geom, ...) |>
    sf::st_transform(crs = crs)
  
  df <- df |>
    dplyr::mutate(
      coords = sf::st_coordinates(sf::st_geometry(df)),
      "{cols[1]}" := .data$coords[, "X"],
      "{cols[2]}" := .data$coords[, "Y"]
    ) |>
    dplyr::select(-c("coords")) |>
    sf::st_transform(init_crs)
  
  if(retain_geom) {
    df <- df |>
      sf::st_drop_geometry() |>
      sf::st_set_geometry(init_geo_col)
  }
  df |>
    sf::st_transform(init_crs)
}

#' Add Z Dimension to Points from Column
#'
#' @param df Simple features data frame or tibble.
#' @param cols Character vector. Contains names of x, y, and z columns.
#'
#' @returns Simple features data frame or tibble.
#' @export
#'
st_elevate_points <- function(df, cols=c("x", "y", "z")) {
  if (!(cols[1:2] %in% names(df))) {
    df <- df |>
      st_geom_to_xy(cols=cols[1:2])
  }
  df |>
    sf::st_drop_geometry() |>
    sf::st_as_sf(coords = cols)
}
