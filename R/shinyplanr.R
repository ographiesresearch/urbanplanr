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
  dir <- system.file("shinyplanr", package = "urbanplanr")
  if (dir == "") {
    stop("Could not find 'inst/shinyplanr/'. Try re-installing `urbanplanr`.", call. = FALSE)
  }
  if (!requireNamespace("mapgl", quietly = TRUE)) {
    stop("Packages 'mapgl' is required to run `shinyplanr`. Please install it.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Packages 'shiny' is required to run `shinyplanr`. Please install it.")
  }
  shiny::runApp(dir, display.mode = "normal")
}
