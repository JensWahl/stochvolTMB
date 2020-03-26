#' Function for parameter estimation of stochastic volatility models
#'@param data vector of observations 
#'@param method string specifying distribution of error term in observational equation
#'@return list of summary report and opt object
#'@export
get_nll <- function(data, 
                    method = "gaussian"){
  
  # set column names of data to NULL to remove notes from R CMD check
  # see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  
  
  if(!is.vector(data)) stop("data needs to be a vector")
  if(!is.character(method)) stop("method has to be a character")
  if(!(method %in% c("gaussian", "skew_gaussian", "t", "leverage"))) stop("method not implemented")
  #if(!(report %in% c("fixed", "report", "random"))) stop("report not valid")
  
  rowname <- Estimate <- `Std. Error` <- `z value` <- `Pr(>|z^2|)` <- NULL
  
  
  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                phi_logit = 2,
                df = if(method == "t"){2}else{numeric(0)},
                alpha = if(method %in% c("skew_gaussian")) {0} else {numeric(0)},
                rho_logit = if(method %in% c("leverage")) {0} else {numeric(0)},
                h = rep(0, length(data)))
  
  data <- list(y = data,
               method = ifelse(method == "gaussian", 0,
                               ifelse(method == "t", 1, 
                                      ifelse(method == "skew_gaussian", 2,
                                             ifelse(method == "leverage", 3, 4)))))
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB")
  
  return(obj)
  
}

#' Optimize negative log likelihood
#' @param obj TMB object returned from \code{get_nll}.
#' @param opt.control An optional list of parameters for nlminb
#' @param report Vector of character  

#' 
estimate_parameters <- function(data,
                                method, 
                                opt.control = NULL,  
                                report = c("fixed", "report", "random")){
  
  # create TMB object
  obj = get_nll(data, method)
  
  # Optimize nll 
  fit <- stats::nlminb(obj$par, obj$fn, obj$gr, opt.control)
  
  # Calculate standard error for all parameters (including latent)
  rep <- TMB::sdreport(obj)

  
  srep_fixed <- srep_report <- srep_random <- NULL
  
  if("fixed" %in% report){
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
  
  if("report" %in% report){
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
  
  if("random" %in% report){
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

  return(opt)
}
  
#' Calculate AIC of model
#' @rdname aic
#' @return \code{AIC}: AIC of fitted model.
#' @export
AIC.stochvolTMB <- function(object, ...) object$aic

#' Calculate BIC of model
#' @rdname bic
#' @return \code{BIC}: BIC of fitted model.
#' @export
AIC.stochvolTMB <- function(object, ...) object$bic

