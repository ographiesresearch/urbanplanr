UTM_ZONES <- file.path("data-raw", "utm_zones.geojson") |>
  sf::st_read() |>
  dplyr::select(zone_num)

usethis::use_data(UTM_ZONES, overwrite = TRUE)