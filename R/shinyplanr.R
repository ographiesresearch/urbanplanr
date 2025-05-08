#' ShinyPlanR
#' 
#' @description
#' Run a shiny interface walking through configuration of a batch download
#' workflow.
#'
#' @returns Nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' shinyplanr()
#' }
shinyplanr <- function() {
  appDir <- system.file("shinyplanr", package = "urbanplanr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `urbanplanr`.", call. = FALSE)
  }
  if (!(requireNamespace("mapgl", quietly = TRUE) & requireNamespace("shiny", quietly = TRUE))) {
    stop("Package 'mapgl' is required to run this app. Please install it.")
  }
  shiny::runApp(appDir, display.mode = "normal")
}
