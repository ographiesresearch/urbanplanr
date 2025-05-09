#' Study Area
#' 
#' @description
#' Construct a study area from municipalities, counties, or states, and build
#' a region based on a buffer distance and geometry type.
#'
#' @returns Nothing.
#' @export
tar_study_area <- function() {
  script <- system.file("study_area.R", package = "urbanplanr")
  if (script == "") {
    stop("Could not find 'inst/study_area.R'. Try re-installing `urbanplanr`.", call. = FALSE)
  }
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop("Packages 'targets' is required to run workflows. Please install it.")
  }
  # NOT NECESSARY FOR study_area---ONLY FOR BASEMAP.
  # if (!requireNamespace("geotargets", quietly = TRUE)) {
  #   stop("Packages 'geotargets' is required to run workflows. Please install it.")
  # }
  store <- file.path(system.file(package = "urbanplanr"), "_targets", "study_area")
  
  targets::tar_make(script = script, store = store)
}
