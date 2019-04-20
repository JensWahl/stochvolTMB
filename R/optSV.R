#' Function for parameter estimation of stochastic volatility model
#'@param data vector of observations 
#'@param method string specifying distribution of error term in observational equation
#'@return tibble of parameter estimates and standard error of estimates
#'@export
optSV <- function(data, 
                  method = "gaussian",
                  report = c("fixed", "random")){
  
  # Starting values for parameters
  param <- list(log_sigma_y = 0,
                log_sigma_h = 0, 
                phi_logit = 2,
                df = if(method == "t"){2}else{numeric(0)},
                alpha = if(method == "skew_gaussian"){-5}else{numeric(0)},
                rho_logit = if(method == "leverage"){0}else{numeric(0)},
                h = rep(0, length(data)))
  
  data <- list(y = data,
               method = ifelse(method == "gaussian", 0,
                               ifelse(method == "t", 1, 
                                      ifelse(method == "skew_gaussian", 2,
                                             ifelse(method == "leverage", 3, 4)))))
  
  obj <- TMB::MakeADFun(data = data, parameters = param, random = "h", DLL = "stochvolTMB")
  opt <- stats::nlminb(obj$par, obj$fn, obj$gr, control = list(trace = TRUE))
  rep <- TMB::sdreport(obj)
  
    srep_fixed <- summary(rep, select = c("report", "fixed"), p.value = TRUE) %>% 
      tibble::as_tibble(rownames = NA) %>%
      tibble::rownames_to_column() %>% 
      dplyr::rename(parameter = rowname, 
                    estimate = Estimate, 
                    std_error = `Std. Error`,
                    z_value = `z value`,
                    p_value = `Pr(>|z^2|)`) %>% 
      mutate(type = "fixed")
    
    srep_random <- summary(rep, select = c("random"), p.value = TRUE) %>% 
      tibble::as_tibble(rownames = NA) %>%
      tibble::rownames_to_column() %>% 
      dplyr::rename(parameter = rowname, 
                    estimate = Estimate, 
                    std_error = `Std. Error`,
                    z_value = `z value`,
                    p_value = `Pr(>|z^2|)`) %>% 
      mutate(type = "random") %>% 
    dplyr::mutate(parameter = ifelse(parameter == "h", paste0("h", 1:n()), parameter))
    

  # if("fixed" %in% report){
  #   srep_fixed <- summary(rep, select = c("report", "random"), p.value = TRUE) %>% 
  #     tibble::as_tibble(rownames = NA) %>%
  #     tibble::rownames_to_column() %>% 
  #     dplyr::rename(parameter = rowname, 
  #                   estimate = Estimate, 
  #                   std_error = `Std. Error`,
  #                   z_value = `z value`,
  #                   p_value = `Pr(>|z^2|)`)
  # }
  
    srep <- dplyr::bind_rows(srep_fixed, srep_random)
  
  return(list(report = srep, opt = opt))
  
}
