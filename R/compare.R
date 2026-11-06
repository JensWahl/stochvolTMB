#' Fit several models and compare them
#'
#' Fits each model with \code{\link{estimate_parameters}} and returns a table ranked by AIC.
#'
#' The table also shows if the optimizer converged and if the Hessian is positive definite. A model that fails one of
#' these checks has unreliable estimates and standard errors, even when its AIC is small. A model that gives an error
#' is reported with \code{NA} values and the error message.
#'
#' @param data A vector of observations.
#' @param models Character vector of models. See \code{\link{estimate_parameters}}. The default is all models.
#' @param ... Additional arguments passed to \code{\link{estimate_parameters}}. \code{silent = TRUE} is used
#'   unless you set it.
#' @return A \code{data.table} with one row for each model, ordered by AIC, and the columns \code{model},
#'   \code{n_par}, \code{log_lik}, \code{aic}, \code{bic}, \code{delta_aic} (the difference to the smallest AIC),
#'   \code{converged}, \code{pd_hessian} and \code{message}. The fitted models are in the attribute \code{"fits"},
#'   a named list of \code{stochvolTMB} objects.
#' @export
#' @examples
#' \donttest{
#' comparison <- compare_models(spy$log_return, models = c("gaussian", "t", "leverage"))
#' comparison
#' best <- attr(comparison, "fits")[[comparison$model[1]]]
#' }
compare_models <- function(data,
                           models = c("gaussian", "t", "leverage", "skew_gaussian", "skew_gaussian_leverage",
                                      "skew_t", "skew_t_leverage"),
                           ...) {

  aic <- NULL

  args <- list(...)
  if (is.null(args$silent)) args$silent <- TRUE

  fits <- lapply(models, function(m) {
    tryCatch(do.call(estimate_parameters, c(list(data = data, model = m), args)),
             error = function(e) conditionMessage(e))
  })
  names(fits) <- models

  res <- rbindlist(lapply(models, function(m) {
    fit <- fits[[m]]
    if (is.character(fit)) {
      return(data.table(model = m, n_par = NA_integer_, log_lik = NA_real_, aic = NA_real_, bic = NA_real_,
                        converged = FALSE, pd_hessian = NA, message = fit))
    }
    ll <- stats::logLik(fit)
    data.table(model = m, n_par = attr(ll, "df"), log_lik = as.numeric(ll),
               aic = stats::AIC(fit), bic = stats::BIC(fit),
               converged = fit$fit$convergence == 0, pd_hessian = isTRUE(fit$rep$pdHess),
               message = fit$fit$message)
  }))

  res[, delta_aic := aic - min(aic, na.rm = TRUE)]
  setcolorder(res, c("model", "n_par", "log_lik", "aic", "bic", "delta_aic"))
  ord <- order(res$aic, na.last = TRUE)
  res <- res[ord]
  attr(res, "fits") <- fits[res$model]
  res[]
}
