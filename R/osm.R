#' Get OpenStreetMap Features by Key
#' @name get_osm
#' 
#' @description
#' `get_osm_key()` retrieves an arbitrary feature type based on its key.
#' `get_osm_buildings()` retrieves buildings based on bounding box of a feature.
#'
#' @param extent `sf` object used to generate `bbox`
#' @param key OpenStreetMap feature key.
#'
#' @returns `sf` object
#' @export
osm_extent_to_opq <- function(extent) {
  extent |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    osmdata::opq()
}

osm_get_type <- function(data, type) {
  if (type == "polygons") {
    data <- data$osm_polygons
  }
  if (type == "lines") {
    data <- data$osm_lines
  }
  data
}

osm_get_features <- function(extent, features, type) {
  extent |>
    osm_extent_to_opq() |>
    osmdata::add_osm_features(features = features) |>
    osmdata::osmdata_sf() |>
    osm_get_type(type = type) |>
    sf::st_transform(sf::st_crs(extent))
}

osm_get_key <- function(extent, key, type) {
  extent |>
    osm_extent_to_opq() |>
    osmdata::add_osm_feature(key = key) |>
    osmdata::osmdata_sf() |>
    osm_get_type(type = type) |>
    sf::st_transform(sf::st_crs(extent))
}

#' @name get_osm
#' @export
osm_get_buildings <- function(extent) {
  extent |>
    osm_get_key(key = "building", type = "polygons") |>
    dplyr::select(id = osm_id, name)
}

#' @name get_osm
#' @export
osm_get_open_space <- function(extent) {
  extent |>
    osm_get_features(
      list(
        leisure = "park",
        leisure = "nature_reserve",
        leisure = "recreation_ground",
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
    dplyr::select(id = osm_id, name, type)
}

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
    dplyr::select(id = osm_id, name, type)
}
