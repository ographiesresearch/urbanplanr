targets::tar_option_set(
  # packages = c("dplyr", "sf", "stringr"),
  format = "qs"
)

targets::tar_source()
load("R/sysdata.rda")

list(
  targets::tar_target(
    name = places,
    command = utils_place_picker(
      places = config$places,
      crs = config$crs,
      buffer = config$buffers$places
    ) |>
      utils_write_multi(
        name = "places",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = region,
    command = places |>
      st_regionalize()
  ),
  targets::tar_target(
    name = places_boundary,
    command = places |>
      dplyr::group_by(type) |>
      dplyr::summarize(
        geometry = sf::st_union(geometry)
      ) |>
      dplyr::ungroup() |>
      utils_write_multi(
        name = "places_boundary",
        dir_db = dir_db,
        format = config$format
      )
  )
)