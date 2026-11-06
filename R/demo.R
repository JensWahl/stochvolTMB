#' Run the shiny demo
#'
#' Starts a shiny application where you can simulate from the models and see the effect of the parameters.
#' The \code{shiny} package must be installed.
#' @return Called for its side effect.
#' @export
run_shiny_demo <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is needed for the demo. Install it with install.packages(\"shiny\").", call. = FALSE)
  }
  appDir <- system.file("shiny-example", package = "stochvolTMB")
  if (appDir == "") {
    stop("Could not find folder for shiny app. Try to re-install `stochvolTMB`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

#' Run the shiny demo (deprecated)
#'
#' Use \code{\link{run_shiny_demo}} instead. Without arguments, \code{demo()} runs the shiny demo with a
#' deprecation warning. With arguments, it calls \code{\link[utils]{demo}}, so \code{demo(topic)} still works
#' when stochvolTMB is attached.
#' @param ... Passed to \code{\link[utils]{demo}}.
#' @return Called for its side effect.
#' @keywords internal
#' @export
demo <- function(...) {
  if (...length() > 0) {
    # utils::demo reads `topic` unevaluated, so pass the original call on instead of `...`
    call <- match.call()
    call[[1]] <- quote(utils::demo)
    return(eval(call, parent.frame()))
  }
  .Deprecated("run_shiny_demo", package = "stochvolTMB")
  run_shiny_demo()
}
