#' @export
shinyplanr <- function() {
  appDir <- system.file("shinyplanr", package = "urbanplanr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
