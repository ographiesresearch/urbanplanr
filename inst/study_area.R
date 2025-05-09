targets::tar_option_set(
  format = "qs"
)

targets::tar_source()
load("R/sysdata.rda")

list(
  targets::tar_target(
    name = cfg_path,
    command = file.path(
      tools::R_user_dir("urbanplanr", "config"), 
      "study_area.yaml"
      ),
    format = "file"
  ),
  targets::tar_target(
    name = cfg,
    command = yaml::read_yaml(cfg_path)
  ),
  targets::tar_target(
    name = places,
    command = utils_place_picker(
      places = cfg$places,
      crs = cfg$crs
    ) |>
      utils_write_multi(
        name = "places",
        dir_db = cfg$dir_db,
        format = cfg$format
      )
  ),
  targets::tar_target(
    name = region,
    command = places |>
      st_regionalize(
        dist = units::as_units(
          cfg$region_dist, 
          cfg$region_units
          ),
        type = cfg$region_type
      ) |>
      utils_write_multi(
        name = "region",
        dir_db = cfg$dir_db,
        format = cfg$format
      )
  ),
  targets::tar_target(
    name = places_boundary,
    command = places |>
      sf::st_exterior_ring() |>
      dplyr::group_by(type) |>
      dplyr::summarize(
        geometry = sf::st_union(geometry)
      ) |>
      dplyr::ungroup() |>
      utils_write_multi(
        name = "places_boundary",
        dir_db = cfg$dir_db,
        format = cfg$format
      )
  )
)