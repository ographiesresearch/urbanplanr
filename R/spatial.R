#' Pre-processing operations for spatial data.
#'
#' @param df Simple features dataframe.
#' @param crs EPSG code or `crs` object.
#'
#' @returns Simple Features dataframe.
#' @export
#'
sf_preprocess <- function(df, crs) {
  df |> 
    sf::st_transform(crs) |>
    dplyr::rename_with(tolower)
}

#' Get DEM from AWS Terrain Tiles
#'
#' @param area Simple features data frame or tibble (ideally polygon).
#' @param z Zoom level to return.
#' @param src One of "aws", "gl3", "gl1", "alos", "srtm15plus".
#'
#' @returns A `RasterLayer`.
#' @export
#'
st_get_dem <- function(area, z=14, src="aws") {
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
#' @param direction Azimuth angle of the light source (degrees). Defaults to
#' 300.
#' @param normalize Logical. If `TRUE` (default), values below zero are set to
#' zero and the results are multiplied by 255.
#' @param overwrite Logical. If `TRUE` (and `filename` is set), overwrites
#' existing raster file.
#' @param filename Character. Optional output filename.
#'
#' @returns A `RasterLayer`.
#' @export
st_hillshade_from_dem <- function(dem,
                                    angle = 45,
                                    direction = 300,
                                    normalize = TRUE,
                                    overwrite = TRUE,
                                    filename = "") {
  raster::hillShade(
    slope = terra::terrain(dem, v = "slope", unit = "radians"),
    aspect = terra::terrain(dem, v = "aspect", unit = "radians"),
    angle = angle,
    direction = direction,
    filename = filename,
    normalize = normalize,
    overwrite = overwrite
  )
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
      as.character(sf::st_geometry_type(geometry)) %in% c(
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
#' @param z_scale Numeric. Z-exaggeration for elevation.
#' @param contour_int Nuemeric. Intervals for contours.
#'
#' @returns Scaled simple features dataframe or tibble.
#' @export
#'
st_model_scale <- function(df, scale, thickness, z_scale, contour_int) {
  crs <- sf::st_crs(df)
  scaled <- (sf::st_geometry(df) - c(scale["x_delta"], scale["y_delta"])) * scale["factor"]
  
  if (!("sfc" %in% class(df))) {
    sf::st_geometry(df) <- scaled
    if ("z" %in% colnames(df)) {
      df <- df |>
        dplyr::mutate(z = (z / (z_scale * contour_int)) * as.numeric(units::set_units(thickness, "m")))
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
      sf::st_crs(UTM_ZONES)) |>
    sf::st_join(UTM_ZONES, sf::st_intersects, largest = TRUE) |>
    dplyr::pull(zone_num)
  
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
#' Takes a dataframe with four columns containing two XY pairs (e.g., [X_{1},
#' Y_{1}], [X_{2}, Y{2}]) and returns an sf dataframe with the same number of
#' rows with those coordinates converted into linestrings.
#'
#' @param df A dataframe with four columns containing two XY pairs.
#' @param names list of the names of the four columns.
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
        base::apply(1, make_line, simplify = FALSE) |>
        sf::st_sfc(crs = sf::st_crs(df))
    )
  
  if (!retain_cols) {
    df <- df |>
      dplyr::select(-all_of(names))
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
sf_is_multipoint <- function(df, ...) {
  st_is_type(df = df, type = "MULTIPOINT", ...)
}

#' @name st_is_type
#' @export
sf_is_point <- function(df, ...) {
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
#' @param ... Arguments passed on to `sf_multi_type_center()`.
#'
#' @return Simple features dataframe.
#'
#' @export
#'
st_geom_to_xy <- function(df, cols=c("x", "y"), retain_geom=FALSE, ...) {
  sf_col <- attr(df, "sf_column")
  
  df <- df |> 
    st_multi_type_center(retain_geom=retain_geom, ...)
  
  df <- df |>
    dplyr::mutate(
      coords = sf::st_coordinates(sf::st_geometry(df)),
      "{cols[1]}" := coords[, "X"],
      "{cols[2]}" := coords[, "Y"]
    ) |>
    dplyr::select(-c(coords))
  
  if(retain_geom) {
    df |>
      sf::st_drop_geometry() |>
      sf::st_set_geometry(sf_col)
  }
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
