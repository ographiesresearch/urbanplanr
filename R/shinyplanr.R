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
#' shinyplanr()
shinyplanr <- function() {
  appDir <- system.file("shinyplanr", package = "urbanplanr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
