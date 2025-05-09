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
                           region_units = "miles",
                           project = "study_area") {
  crs <- as.integer(crs)
  config_path <- tools::R_user_dir("urbanplanr", "config")
  dir.create(config_path, showWarnings = FALSE, recursive = TRUE)
  
  yaml::write_yaml(
    as.list(environment()), 
    file.path(config_path, glue::glue("{project}.yaml"))
  )
  
  targets <- system.file("_targets.yaml", package = "urbanplanr")
  
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Packages 'targets' is required to run workflows. Please install it.")
  }
  
  targets::tar_config_set(
    script = system.file(glue::glue("{project}.R"), package = "urbanplanr"), 
    config = targets,
    store = file.path(targets, project), 
    project = project
    )
  
  Sys.setenv(TAR_PROJECT = project)
  targets::tar_make()
}



# NOT NECESSARY FOR study_area---ONLY FOR BASEMAP.
# if (!requireNamespace("geotargets", quietly = TRUE)) {
#   stop("Packages 'geotargets' is required to run workflows. Please install it.")
# }
