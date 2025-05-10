targets::tar_option_set(
  format = "qs"
)

targets::tar_source()
load("R/sysdata.rda")

list(
  # Dependent on outputs from "studyarea.R" project.
  targets::tar_target(
    name = cfg_file,
    command = "study_area/objects/cfg",
    format = "file"
  ),
  targets::tar_target(
    name = cfg,
    command = qs2::qread(cfg)
  ),
  targets::tar_target(
    name = places_file,
    command = "study_area/objects/places",
    format = "file"
  ),
  targets::tar_target(
    name = places,
    command = qs2::qread(places_file)
  ),
  targets::tar_target(
    name = region_file,
    command = "study_area/objects/region",
    format = "file"
  ),
  targets::tar_target(
    name = region,
    command = qs2::qread(region)
  )
  # ,
  # # Start basemap flow.
  # targets::tar_target(
  #   name = counties,
  #   command = tigris_extent_to_counties(region) |>
  #     st_clip(region)  |>
  #     utils_write_multi(
  #       name = "counties",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = county_list,
  #   command = counties |>
  #     dplyr::mutate(
  #       concat = stringr::str_c(name, "County,", state, sep=" ")
  #     ) |>
  #     dplyr::pull(concat) 
  # ),
  # targets::tar_target(
  #   name = tracts,
  #   command = tigris_get_tracts(
  #     county_list,
  #     crs = config$crs
  #   ) |>
  #     st_filter_and_join(
  #       join = places,
  #       clip = region
  #     ) |>
  #     utils_write_multi(
  #       name = "tracts",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = block_groups,
  #   command = tigris_get_block_groups(
  #     county_list,
  #     crs = config$crs
  #   ) |>
  #     st_filter_and_join(
  #       join = places,
  #       clip = region
  #     ) |>
  #     utils_write_multi(
  #       name = "block_groups",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = postal,
  #   command = tigris_get_postal(
  #     county_list,
  #     crs = config$crs,
  #     year = 2010
  #   ) |>
  #     st_clip(region) |>
  #     utils_write_multi(
  #       name = "postal",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = roads,
  #   command = tigris_get_roads(
  #     county_list,
  #     crs = config$crs
  #   ) |>
  #     st_filter_and_join(
  #       join = places,
  #       clip = region
  #     ) |>
  #     utils_write_multi(
  #       name = "roads",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = rails,
  #   command = tigris_get_rails(
  #     county_list,
  #     crs = config$crs
  #   ) |>
  #     st_filter_and_join(
  #       join = places,
  #       clip = region
  #     ) |>
  #     utils_write_multi(
  #       name = "rails",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = area_water,
  #   command = tigris_get_area_water(
  #     county_list,
  #     crs = config$crs
  #   ) |>
  #     st_filter_and_join(
  #       join = places,
  #       clip = region
  #     ) |>
  #     utils_write_multi(
  #       name = "area_water",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = linear_water,
  #   command = tigris_get_linear_water(
  #     county_list,
  #     crs = config$crs
  #   ) |>
  #     st_filter_and_join(
  #       join = places,
  #       clip = region
  #     ) |>
  #     utils_write_multi(
  #       name = "linear_water",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # geotargets::tar_terra_rast(
  #   name = dem_og,
  #   command = st_get_dem(
  #     region,
  #     tiles_on_side = config$elev$tiles_on_side,
  #     expand = config$elev$expand,
  #     rowwise = config$elev$rowwise
  #   )
  # ),
  # targets::tar_target(
  #   name = contours,
  #   command = st_contours(
  #     dem_og,
  #     interval = config$elev$contour_int
  #   ) |>
  #     st_clip(region) |>
  #     utils_write_multi(
  #       name = "contours",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = contours_enc,
  #   command = st_contours_enclose(
  #     contours,
  #     region,
  #     poly = TRUE,
  #     bbox = FALSE
  #   ) |>
  #     utils_write_multi(
  #       name = "contours_enc",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # geotargets::tar_terra_rast(
  #   name = dem_crop,
  #   command = terra::crop(
  #     dem_og,
  #     region,
  #     mask = TRUE
  #   ) |>
  #     utils_write_multi(
  #       name = "dem",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # geotargets::tar_terra_rast(
  #   name = hillshade,
  #   command = st_hillshade(
  #     dem_crop,
  #     z_scale = config$elev$z_scale
  #   ) |>
  #     utils_write_multi(
  #       name = "hillshade",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = munis,
  #   command = region_list |>
  #     munis_get_munis(
  #       filter = FALSE,
  #       crs = config$crs
  #     ) |>
  #     st_clip(region) |>
  #     utils_write_multi(
  #       name = "munis",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = open_space,
  #   command = osm_get_open_space(region) |>
  #     st_clip(region) |>
  #     utils_write_multi(
  #       name = "open_space",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = bike_lanes,
  #   command = osm_get_bike_lanes(region) |>
  #     st_clip(region) |>
  #     utils_write_multi(
  #       name = "bike_lanes",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # ),
  # targets::tar_target(
  #   name = buildings,
  #   command = osm_get_buildings(places) |>
  #     st_clip(places) |>
  #     utils_write_multi(
  #       name = "buildings",
  #       dir_db = dir_db,
  #       format = config$format
  #     )
  # )
)