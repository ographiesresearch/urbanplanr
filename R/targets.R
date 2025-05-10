tar_generic <- function(project, requires, params) {
  
  utils_packages_exist(requires)
  
  config_path <- tools::R_user_dir("urbanplanr", "config")
  dir.create(config_path, showWarnings = FALSE, recursive = TRUE)
  
  yaml::write_yaml(
    params, 
    file.path(config_path, glue::glue("{project}.yaml"))
  )
  
  targets::tar_config_set(
    script = system.file(glue::glue("{project}.R"), package = "urbanplanr"), 
    config = system.file("_targets.yaml", package = "urbanplanr"),
    store = file.path(config_path, "_targets", project), 
    project = project
  )
  
  Sys.setenv(TAR_PROJECT = project)
  targets::tar_make()
  
}

#' Study Area
#' 
#' @description
#' Construct a study area from municipalities, counties, or states, and build
#' a region based on a buffer distance and geometry type.
#'
#' @returns Nothing.
#' @export
tar_study_area <- function(places,
                           crs,
                           format,
                           dir_db,
                           region_type = "bbox",
                           region_dist = NULL,
                           region_units = "miles") {
  tar_generic(
    project = "study_area",
    requires = c("targets"),
    params = as.list(environment())
  )
}

tar_basemap <- function(contour_int,
                        tiles_on_side = 16,
                        z_scale = 1,
                        rowwise = NULL) {
  tar_generic(
    project = "basemap",
    requires = c("targets", "geotargets"),
    params = as.list(environment())
  )
}
