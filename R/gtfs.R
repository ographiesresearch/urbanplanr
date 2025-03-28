gtfs_process <- function(gtfs, crs) {
  gtfs <- gtfs |>
    tidytransit::empty_strings_to_na()
  
  gtfs$stops <- gtfs$stops  |> 
    tidyr::drop_na(stop_lat, stop_lon)
  
  routes <- gtfs |>
    tidytransit::gtfs_as_sf() |>
    tidytransit::get_route_geometry(
      route_ids = gtfs$routes$route_id
    ) |>
    dplyr::left_join(
      gtfs$routes |>
        dplyr::select(
          route_id,
          desc = route_desc,
          name = route_long_name,
          short_name = route_short_name
        ),
      by=dplyr::join_by(route_id)
    ) |>
    sf::st_transform(crs)
  
  gtfs$stops <- gtfs |>
    tidytransit::filter_stops(
      service_ids=gtfs$calendar$service_id,
      route_ids=route_ids
    )
  
  stops <- gtfs$stops |>
    tidytransit::stops_as_sf() |>
    dplyr::group_by(stop_name) |>
    dplyr::slice_head(n=1) |>
    dplyr::ungroup() |>
    dplyr::select(
      id = stop_id,
      name = stop_name
    )  |>
    sf::st_transform(crs)
  list(
    routes=routes, 
    stops=stops
  )
}

gtfs_preprocess <- function(gtfs) {
  
}

gtfs_get_mbta <- function(crs) {
  gtfs <- tidytransit::read_gtfs("https://cdn.mbta.com/MBTA_GTFS.zip")
  gtfs$routes <- gtfs$routes |>
    dplyr::filter(
      route_desc %in% c("Rapid Transit", "Commuter Rail") |
        stringr::str_detect(route_short_name, "^SL"))
  gtfs
}


gtfs_get_mta <- function(crs) {
  gtfs <- tidytransit::read_gtfs("http://web.mta.info/developers/data/nyct/subway/google_transit.zip")
}