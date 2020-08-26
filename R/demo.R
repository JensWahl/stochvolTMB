#' Run shiny demo
#' @export
demo <- function() {
  appDir <- system.file("shiny-example", package = "stochvolTMB")
  if (appDir == "") {
    stop("Could not find folder for shiny app. Try to re-install `stochvolTMB`.", 
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}