#' @useDynLib stochvolTMB, .registration = TRUE
#' @keywords internal
.onLoad <- function(libname = find.package("stochvolTMB"), pkgname = "stochvolTMB"){
  
  # CRAN Note avoidance
  utils::globalVariables(
    c(
      "rowname", "Estimate","Std. Error", "z value", "Pr(>|z^2|)"
    )
  )
  invisible()
}

.onUnload <- function(libpath){
}