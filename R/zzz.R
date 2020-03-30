.onLoad <- function(libname = find.package("stochvolTMB"), pkgname = "stochvolTMB"){
  
  # CRAN Note avoidance
  utils::globalVariables(
    c(
      ".N", "type", "Estimate","Std. Error", "z value", "Pr(>|z^2|)"
    )
  )
  invisible()
}

.onUnload <- function(libpath){
}