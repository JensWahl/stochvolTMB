#' Function for parameter estimation of stochastic volatility models
#'@param data vector of observations 
#'@param modelstring specifying distribution of error term in observational equation
#'@return list of summary report and opt object
#'@export
get_nll <- function(data, model= "gaussian"){
  
  # set column names of data to NULL to remove notes from R CMD check
  # see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  
  if (!is.vector(data)) stop("data needs to be a vector")
  if (!is.character(model)) stop("modelhas to be a character")
  if (!(model%in% c("gaussian", "skew_gaussian", "t", "leverage"))) stop("model not implemented")

  rowname <- Estimate <- `Std. Error` <- `z value` <- `Pr(>|z^2|)` <- NULL
  
  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                phi_logit = 2,
                df = if(model== "t"){2}else{numeric(0)},
                alpha = if(model%in% c("skew_gaussian")) {0} else {numeric(0)},
                rho_logit = if(model%in% c("leverage")) {0} else {numeric(0)},
                h = rep(0, length(data)))
  
  data <- list(y = data,
               model= ifelse(model== "gaussian", 0,
                               ifelse(model== "t", 1, 
                                      ifelse(model== "skew_gaussian", 2,
                                             ifelse(model== "leverage", 3, 4)))))
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB")
  
  return(obj)
  
}

#' Estimate parameters for the stochastic volatility model
#' 
#' @description 
#' \code{estimate parameters} estimate the parameters of a stochastic volatility model with a latent log-volatility \code{h} that follows an 
#' autoregressive process of order one with normally distributed noise. The following distributions are implemented for the observed process: 
#' 
#' \itemize{
#' \item Gaussian distribution
#' \item t-distribution
#' \item Leverage: Gaussian distribution with leverage where the noise of the latent process \code{h} is correlated with the observational distribution
#' \item Skew gaussian distribution
#' }
#' 
#' The parameters is estimated by minimizing the negative log-likelihood (nll) and the latent log-volatility is integrated out by applying the
#' Laplace approximation
#' 
#' @param data A vector of observations.
#' @param model A character specifying the model. Must be one of the following: gaussian, t, leverage, skew_gaussian.
#' @param opt.control An optional list of parameters for nlminb
#' @param report Parameters to report with uncertanty . Can be any subset of "fixed", "transformed" or "random" (see \link[TMB]{summary.sdreport}). "fixed" 
#' report the parameters on the scale they were estimated, for example are all standard deviations are estimated on log scale. "transformed" 
#' reports all transformed parameters, for example estimated standard deviations transformed from log scale by taking the exponential. Lastly, "random"
#' report the estimated latent log-volatility. 
#' 
#' @return Object of class \code{stochvolTMB}
#' 
#' @export 
estimate_parameters <- function(data,
                                model, 
                                opt.control = NULL,  
                                report = c("all", "fixed", "transformed", "random")){
  
  # create TMB object
  obj = get_nll(data, model)
  
  # Optimize nll 
  fit <- stats::nlminb(obj$par, obj$fn, obj$gr, opt.control)
  
  # Calculate standard error for all parameters (including latent)
  rep <- TMB::sdreport(obj)

  if ("all" %in% report) report <- c("fixed", "transformed", "random")
  
  srep_fixed <- srep_report <- srep_random <- NULL
  
  if ("fixed" %in% report){
    srep_fixed <- TMB::summary.sdreport(rep, select = c("fixed"), p.value = TRUE) %>% 
      tibble::as_tibble(rownames = NA) %>%
      tibble::rownames_to_column() %>% 
      dplyr::rename(parameter = rowname, 
                    estimate = Estimate, 
                    std_error = `Std. Error`,
                    z_value = `z value`,
                    p_value = `Pr(>|z^2|)`) %>% 
      dplyr::mutate(type = "fixed")
  }
  
  if ("transformed" %in% report){
    srep_report <- TMB::summary.sdreport(rep, select = c("report"), p.value = TRUE) %>% 
      tibble::as_tibble(rownames = NA) %>%
      tibble::rownames_to_column() %>% 
      dplyr::rename(parameter = rowname, 
                    estimate = Estimate, 
                    std_error = `Std. Error`,
                    z_value = `z value`,
                    p_value = `Pr(>|z^2|)`) %>% 
      dplyr::mutate(type = "transformed") 
  }
  
  if ("random" %in% report){
    srep_random <- TMB::summary.sdreport(rep, select = c("random"), p.value = TRUE) %>% 
      tibble::as_tibble(rownames = NA) %>%
      tibble::rownames_to_column() %>% 
      dplyr::rename(parameter = rowname, 
                    estimate = Estimate, 
                    std_error = `Std. Error`,
                    z_value = `z value`,
                    p_value = `Pr(>|z^2|)`) %>% 
      dplyr::mutate(type = "random")
  }
  
  
  srep <- dplyr::bind_rows(srep_fixed, srep_report, srep_random)
  
  
  opt <- list()
  class(opt) <- "stochvolTMB"

  opt$rep <- srep
  opt$obj <- obj
  opt$fit <- fit
  opt$aic <- 2 * fit$objective + 2 * length(fit$par)
  opt$bic <- 2 * fit$objective + log(length(data)) * length(fit$par)
  opt$model <- model

  return(opt)
}
  
#' Calculate AIC of model
#' @rdname AIC
#' @param object A \code{stochvolTMB} object.
#' @return \code{AIC}: AIC of fitted model.
#' @export
AIC.stochvolTMB <- function(object) object$aic

#' Calculate BIC of model
#' @rdname BIC
#' @param object A \code{stochvolTMB} object.
#' @return \code{BIC}: BIC of fitted model.
#' @export
AIC.stochvolTMB <- function(object) object$bic

