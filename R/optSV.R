#' Function for parameter estimation of stochastic volatility models
#'@param data vector of observations 
#'@param method string specifying distribution of error term in observational equation
#'@param report character vector 
#'@return list of summary report and opt object
#'@export
optSV <- function(data, 
                  method = "gaussian",
                  report = c("fixed", "report", "random")){
  
  # set column names of data to NULL to remove notes from R CMD check
  # see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  
  
  if(!is.vector(data)) stop("data needs to be a vector")
  if(!is.character(method)) stop("method has to be a character")
  if(!(method %in% c("gaussian", "skew_gaussian", "t", "leverage", "skew_gaussian_leverage"))) stop("method not implemented")
  #if(!(report %in% c("fixed", "report", "random"))) stop("report not valid")
  
  rowname <- Estimate <- `Std. Error` <- `z value` <- `Pr(>|z^2|)` <- NULL
  
  
  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                phi_logit = 2,
                df = if(method == "t"){2}else{numeric(0)},
                alpha = if(method %in% c("skew_gaussian", "skew_gaussian_leverage")){-5}else{numeric(0)},
                rho_logit = if(method %in% c("leverage", "skew_gaussian_leverage")){0}else{numeric(0)},
                h = rep(0, length(data)))
  
  data <- list(y = data,
               method = ifelse(method == "gaussian", 0,
                               ifelse(method == "t", 1, 
                                      ifelse(method == "skew_gaussian", 2,
                                             ifelse(method == "leverage", 3,
                                                    ifelse(method == "skew_gaussian_leverage", 4, 5))))))
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB")
  opt <- stats::nlminb(obj$par, obj$fn, obj$gr, control = list(trace = TRUE))
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
  
  return(list(report = srep, opt = opt))
  
}
