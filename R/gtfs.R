#' GTFS Helper Functions
#' @name gtfs_helpers
#' @description
#' `gtfs_routes()` takes a `tidygtfs` object and returns routes as `sf`.
#' `gtfs_stops()` takes a `tidygtfs` object and returns stops as `sf`.
#' `gtfs_process()` removes stops with no lat-longs and builds `sf` objects.
#' 
#' @param gtfs A `tidygtfs` object.
#' @param crs `crs` object or EPSG code.
#'
#' @returns An `sf` object
#' @export
gtfs_routes <- function(gtfs, crs) {
  routes <- gtfs |>
    tidytransit::get_route_geometry() |>
    dplyr::left_join(
      gtfs$routes,
      by=dplyr::join_by("route_id")
    ) |>
    sf::st_transform(crs)
}

#' @name gtfs_helpers
#' @export
gtfs_stops <- function(gtfs, crs) {
  stops <- gtfs$stops |>
    tidytransit::stops_as_sf() |>
    sf::st_transform(crs)
}

#' @name gtfs_helpers
#' @export
gtfs_process <- function(gtfs, crs) {
  gtfs <- gtfs |>
    tidytransit::empty_strings_to_na()
  
  gtfs$stops <- gtfs$stops |> 
    tidyr::drop_na("stop_lat", "stop_lon")
  
  gtfs <- gtfs |>
    tidytransit::gtfs_as_sf()
  
  list(
    routes = gtfs_routes(gtfs, crs),
    stops = gtfs_stops(gtfs, crs)
  )
}

#' Retrieve Various GTFS Feeds
#' @name gtfs_feeds
#' 
#' @description
#' `gtfs_mbta()` retrieves the GTFS feed of the Massachusetts Bay Transportation
#' Authority (MBTA) servicing eastern Massachusetts.
#' 
#' `gtfs_mta()` retrieves the GTFS feed of the Metropolitan Transportation
#' Authority (MTA) servicing the New York City metro area.
#'
#' @inheritParams gtfs_helpers
#'
#' @returns A list including `routes` and `stops` as `sf` objects.
#' @export
gtfs_get_mbta <- function(crs) {
  tidytransit::read_gtfs("https://cdn.mbta.com/MBTA_GTFS.zip") |>
    gtfs_process(crs)
}

#' @name gtfs_feeds
#' @export
gtfs_get_mta <- function(crs) {
  tidytransit::read_gtfs(
    "http://web.mta.info/developers/data/nyct/subway/google_transit.zip"
    ) |>
    gtfs_process(crs)
}