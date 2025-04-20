#' Terrain Workflow
#'
#' @inheritParams st_get_dem
#' @inheritParams st_contours
#' @inheritParams st_hillshade
#' @param mask Boolean. If `TRUE`, DEM and subsequent processed rasters are
#' masked to the boundaries of the extent.
#' @param stepped Boolean. If `TRUE`, returns rasterized stepped topography 
#' based on contours.
#'
#' @returns Named list
#' @export
flow_terrain <- function(extent,
                         expand = NULL,
                         mask = FALSE,
                         interval = 10,
                         z_scale = 3,
                         tiles_on_side = 8,
                         stepped = FALSE,
                         mask = TRUE
  ) {
  message("Downloading digital elevation model..")
  dem <- st_get_dem(extent, tiles_on_side = tiles_on_side, expand = expand)
  message("Done.")
  
  message("Generating contour lines...")
  contours <- dem |>
    st_contours(interval = interval)
  message("Done.")
  
  message("Cropping digital elevation model...")
  dem <- dem |> 
    terra::crop(extent, snap = "in", mask = mask)
  message("Done.")
  
  message(terra::crs(dem))
  
  message("Generating hillshade...")
  hillshade <- dem |>
    st_hillshade(z_scale = z_scale)
  message("Done.")
  
  message("Enclosing contours...")
  contours_enc <- contours |>
    st_contours_enclose(extent, poly = TRUE)
  message("Done.")

  stepped_contours <- NULL
  if(stepped) {
    message("Creating stepped topography raster...")
    stepped_contours <- contours_enc |>
      terra::vect() |>
      terra::rasterize(dem, field="z") |>
      st_hillshade(z_scale = 4)
    message("Done.")
  }

  list(
    dem = dem,
    contours = contours,
    hillshade = hillshade,
    contours_enc = contours_enc,
    stepped = stepped_contours
  )
}

# flow_basemap <- function(
#       states, 
#       counties,
#       extent = NULL,
#       year = NULL,
#       crs = ifelse(
#         is.null(extent), 
#         4326, 
#         sf::st_crs(extent)
#         )
#   ) {
#   # if (!is.null(extent) & (missing(states) & !missing(counties))) {
#   #   
#   # }
#   list(
#     states = tigris_get_states(
#       states = states, 
#       crs = crs,
#       year = year
#     ),
#     counties = tigris_get_counties(
#       states = states, 
#       counties = counties, 
#       crs = crs,
#       year = year
#     ),
#     places = tigris_get_places(
#       states = states,
#       crs = crs,
#       year = year
#     ),
#     block_groups = tigris_get_block_groups(
#       states = states, 
#       crs = crs,
#       year = year
#     ),
#     # |>
#     #   sf::st_join(
#     #     places |>
#     #       dplyr::select(placefp), 
#     #     largest = TRUE
#     #   ),
#     census_tracts = tigris_get_tracts(
#       states = states, 
#       crs = crs,
#       year = year
#     ),
#     # |>
#     #   sf::st_join(
#     #     places |>
#     #       dplyr::select(.data$placefp), 
#     #     largest = TRUE
#     #   ),
#     area_water = tigris_get_area_water(
#       states = states, 
#       counties = counties, 
#       crs = crs,
#       year = year
#     ),
#     linear_water = tigris_get_linear_water(
#       states = states, 
#       counties = counties, 
#       crs = crs,
#       year = year
#     ),
#     roads = tigris_get_roads(
#       states = states, 
#       counties = counties, 
#       crs = crs,
#       year = year
#     ),
#     rails = tigris_get_rails(
#       crs = crs,
#       year = year
#     )
#   )
#   
#   
#   
#   
#   # message("Downloading places...")
#   # place_geo <- place_decision(config$states, crs = config$crs)
#   # 
#   # if ("places" %in% names(config)) {
#   #   place_geo <- place_geo |>
#   #     select_places(places = config$places)
#   # }
# }
