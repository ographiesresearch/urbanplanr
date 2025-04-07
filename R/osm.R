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
osm_get_key <- function(extent, key) {
  df <- extent |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    osmdata::opq() |>
    osmdata::add_osm_feature(key = key) |>
    osmdata::osmdata_sf()
}

#' @name get_osm
#' @export
osm_get_buildings <- function(extent) {
  df <- osm_get_key(extent, "building")
  df$osm_polygons
}
