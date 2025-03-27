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