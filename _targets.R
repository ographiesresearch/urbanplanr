targets::tar_option_set(
  # packages = c("dplyr", "sf", "stringr"),
  format = "qs"
)

targets::tar_source()
load("R/sysdata.rda")

list(
  targets::tar_target(
    name = config_yaml,
    command = "_targets.yaml",
    format = "file"
  ),
  targets::tar_target(
    name = config,
    command = targets::tar_config_yaml(config = config_yaml)[['main']]
  ),
  targets::tar_target(
    name = dir_db,
    command = if (is.null(config$dir_db)) "results" else config$dir_db
  ),
  targets::tar_target(
    name = region_list,
    command = if (is.null(config$region)) config$places else config$region
  ),
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
    command = utils_place_picker(
      places = region_list,
      crs = config$crs,
      buffer = config$buffers$region
    )
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
  ),
  targets::tar_target(
    name = region_boundary,
    command = region |>
      dplyr::group_by(type) |>
      dplyr::summarize(
        geometry = sf::st_union(geometry)
        ) |>
      dplyr::ungroup() |>
      st_bbox_sf() |>
      utils_write_multi(
        name = "region_boundary",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = counties,
    command = {
      if (all(region[["type"]] == "county")) {
        counties = region
      } else {
        counties = tigris_extent_to_counties(region)
      }
      counties |>
        st_clip(region_boundary)  |>
        utils_write_multi(
          name = "counties",
          dir_db = dir_db,
          format = config$format
        )
    }
  ),
  targets::tar_target(
    name = county_list,
    command = counties |>
      dplyr::mutate(
        concat = stringr::str_c(name, "County,", state, sep=" ")
      ) |>
      dplyr::pull(concat) 
  ),
  targets::tar_target(
    name = tracts,
    command = tigris_get_tracts(
      county_list,
        crs = config$crs
        ) |>
      st_filter_and_join(
        join = places,
        clip = region_boundary
      ) |>
      utils_write_multi(
        name = "tracts",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = block_groups,
    command = tigris_get_block_groups(
      county_list,
      crs = config$crs
    ) |>
      st_filter_and_join(
        join = places,
        clip = region_boundary
      ) |>
      utils_write_multi(
        name = "block_groups",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = postal,
    command = tigris_get_postal(
      county_list,
      crs = config$crs,
      year = 2010
    ) |>
      st_clip(region_boundary) |>
      utils_write_multi(
        name = "postal",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = roads,
    command = tigris_get_roads(
      county_list,
      crs = config$crs
    ) |>
      st_filter_and_join(
        join = places,
        clip = region_boundary
      ) |>
      utils_write_multi(
        name = "roads",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = rails,
    command = tigris_get_rails(
      county_list,
      crs = config$crs
    ) |>
      st_filter_and_join(
        join = places,
        clip = region_boundary
      ) |>
      utils_write_multi(
        name = "rails",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = area_water,
    command = tigris_get_area_water(
      county_list,
      crs = config$crs
    ) |>
      st_filter_and_join(
        join = places,
        clip = region_boundary
      ) |>
      utils_write_multi(
        name = "area_water",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = linear_water,
    command = tigris_get_linear_water(
      county_list,
      crs = config$crs
    ) |>
      st_filter_and_join(
        join = places,
        clip = region_boundary
      ) |>
      utils_write_multi(
        name = "linear_water",
        dir_db = dir_db,
        format = config$format
      )
  ),
  geotargets::tar_terra_rast(
    name = dem_og,
    command = st_get_dem(
      region_boundary,
      tiles_on_side = config$elev$tiles_on_side,
      expand = config$elev$expand,
      rowwise = config$elev$rowwise
    )
  ),
  targets::tar_target(
    name = contours,
    command = st_contours(
      dem_og,
      interval = config$elev$contour_int
    ) |>
      st_clip(region_boundary) |>
      utils_write_multi(
        name = "contours",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = contours_enc,
    command = st_contours_enclose(
      contours,
      region_boundary,
      poly = TRUE,
      bbox = FALSE
    ) |>
      utils_write_multi(
        name = "contours_enc",
        dir_db = dir_db,
        format = config$format
      )
  ),
  geotargets::tar_terra_rast(
    name = dem_crop,
    command = terra::crop(
      dem_og,
      region_boundary,
      mask = TRUE
    ) |>
      utils_write_multi(
        name = "dem",
        dir_db = dir_db,
        format = config$format
      )
  ),
  geotargets::tar_terra_rast(
    name = hillshade,
    command = st_hillshade(
      dem_crop,
      z_scale = config$elev$z_scale
    ) |>
      utils_write_multi(
        name = "hillshade",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = munis,
    command = region_list |>
      munis_get_munis(
        filter = FALSE,
        crs = config$crs
        ) |>
      st_clip(region_boundary) |>
      utils_write_multi(
        name = "munis",
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = tracts_housing,
    command = acs_get_multi(
      vars = c("housing", "age", "race"),
      places = county_list, 
      year = 2022, 
      geography="tract"
    ) |>
      utils_write_named_list(
        dir_db = dir_db,
        format = config$format
      )
  ),
  targets::tar_target(
    name = block_group_housing,
    command = acs_get_multi(
        vars = c("housing", "age", "race"),
        places = county_list, 
        year = 2022, 
        geography="block_group"
      ) |>
      utils_write_named_list(
        dir_db = dir_db,
        format = config$format
      )
  )
)
