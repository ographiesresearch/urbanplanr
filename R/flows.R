#' Terrain Workflow
#'
#' @inheritParams st_get_dem
#' @inheritParams st_contours
#' @inheritParams st_hillshade
#' @param stepped Boolean. If `TRUE`, returns rasterized stepped topography 
#' based on contours.
#'
#' @returns Named list
#' @export
flow_terrain <- function(extent,
                         expand = NULL,
                         interval = 10,
                         z_scale = 3,
                         tiles_on_side = 8,
                         stepped = FALSE
  ) {
  dem <- st_get_dem(extent, tiles_on_side = tiles_on_side, expand = expand)
  
  contours <- dem |>
    st_contours(interval = interval)
  
  dem <- dem |> 
    terra::crop(extent, mask=TRUE)
  
  hillshade <- dem |>
    st_hillshade(z_scale = z_scale)
  
  contours_enc <- contours |>
    st_contours_enclose(extent, poly = TRUE)
  
  stepped_contours <- NULL
  if(stepped) {
    stepped_contours <- contours_enc |> 
      terra::vect() |>
      terra::rasterize(dem, field="z") |>
      st_hillshade(z_scale = 4)
  }
  
  list(
    dem = dem,
    contours = contours,
    contours_enc = contours_enc,
    hillshade = hillshade,
    stepped = stepped_contours
  )
}