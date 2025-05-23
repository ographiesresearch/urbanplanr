osm_extent_to_opq <- function(extent) {
  extent |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    osmdata::opq()
}

osm_get_type <- function(data, type) {
  if (type == "polygons") {
    simple <- data$osm_polygons
    multi <- data$osm_multipolygons
    cast_to <- "MULTIPOLYGON"
  }
  if (type == "lines") {
    simple <- data$osm_lines
    multi <- data$osm_multilines
    cast_to <- "MULTILINESTRING"
  }
  if (!is.null(simple) & !is.null(multi)) {
    data <- simple |>
      dplyr::bind_rows(multi)
  } else if (!is.null(simple)) {
    data <- simple
  } else {
    data <- multi
  }
  data |>
    sf::st_cast(cast_to) |>
    sf::st_make_valid()
}

#' Get OpenStreetMap Features
#' @name osm_get_
#' 
#' @description
#' `osm_get_buildings()` Retrieves buildings from OpenStreetMap, given an 
#' input extent.
#' 
#' `osm_get_open_space()` Retrieves open space from OpenStreetMap, given an 
#' input extent.
#' 
#' `osm_get_bike_lanes()` Retrieves bike lanes from OpenStreetMap, given an
#' input extent.
#' 
#' @param extent `sf` object used to generate the overpass query.
#' @param key OpenStreetMap feature key.
#' @param features Named list or vector with key value pairs.
#' @param key A feature key (e.g., "building").
#' @param type String. "Polygons" or "lines".
#'
#' @returns `sf` object
#' 
#' @export
osm_get_features <- function(extent, features, type) {
  extent |>
    osm_extent_to_opq() |>
    osmdata::add_osm_features(features = features) |>
    osmdata::osmdata_sf() |>
    osm_get_type(type = type) |>
    sf::st_transform(sf::st_crs(extent))
}

#' @name osm_get_
#' @export
osm_get_key <- function(extent, key, type) {
  extent |>
    osm_extent_to_opq() |>
    osmdata::add_osm_feature(key = key) |>
    osmdata::osmdata_sf() |>
    osm_get_type(type = type) |>
    sf::st_transform(sf::st_crs(extent))
}

#' @name osm_get_
#' @export
osm_get_buildings <- function(extent) {
  extent |>
    osm_get_key(key = "building", type = "polygons") |>
    dplyr::select(id = "osm_id", "name")
}

#' @name osm_get_
#' @export
osm_get_open_space <- function(extent) {
  extent |>
    osm_get_features(
      list(
        leisure = "park",
        leisure = "nature_reserve",
        leisure = "recreation_ground",
        leisure = "golf_course",
        boundary = "protected_area",
        landuse = "forest",
        landuse = "meadow",
        natural = "wood"
      ), 
      type = "polygons"
      ) |>
    dplyr::mutate(
      type = dplyr::case_when(
          leisure %in% c("park", "recreation_ground") ~ "park",
          leisure == "nature_reserve" | boundary == "protected_area" ~ "reserve",
          landuse == "forest" | natural == "wood" ~ "forest",
          landuse == "meadown" ~ "meadow",
          .default = NULL
        )
      ) |>
    dplyr::select(id = "osm_id", "name", "type")
}

#' @name osm_get_
#' @export
osm_get_bike_lanes <- function(extent) {
  extent |>
    osm_get_features(
      list(
        highway = "cycleway",
        cycleway = "lane",
        cycleway = "shared",
        cycleway = "separate",
        "cycleway:right" = "lane",
        "cycleway:left" = "lane",
        cycleway = "track",
        "cycleway:right" = "track",
        "cycleway:left" = "track",
        cycleway = "shared_lane",
        "cycleway:right" = "shared_lane",
        "cycleway:left" = "shared_lane",
        cycleway = "opposite_lane",
        "cycleway:right" = "opposite_lane",
        "cycleway:left" = "opposite_lane",
        cycleway = "opposite_track",
        "cycleway:right" = "opposite_track",
        "cycleway:left" = "opposite_track"
      ), 
      type = "lines"
      ) |>
    dplyr::mutate(
      type = dplyr::case_when(
        dplyr::if_any(
          dplyr::starts_with("cycleway"), 
          ~ .x %in% c("lane", "track", "shared", "separate", "shared_lane", "opposite_lane", "opposite_track")) ~ "bikelane",
        highway == "cycleway" ~ "dedicated",
        .default = NULL
      )
    ) |> 
    dplyr::select(id = "osm_id", "name", "type")
}
