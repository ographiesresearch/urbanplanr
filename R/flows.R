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
                         stepped = FALSE
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
    terra::crop(extent, mask=mask)
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