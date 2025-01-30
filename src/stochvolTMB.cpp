#define TMB_LIB_INIT R_init_stochvolTMB
#include<TMB.hpp>

// Skew normal distribution with mean zero and unit variance
// @param x double. Where to evaluate density
// @param alpha double. Skewness parameter 
// @sigma_y double. Parameter from observational equation
// @param h double. Latent variable 
// @param give_log bool. Return log of density
template<class Type> 
Type skew_norm(Type x, Type alpha, Type omega, Type xi, bool give_log){
  Type dens = log(Type(2)) + dnorm(x, xi, omega, true) + log(Type(0.00001) + pnorm(alpha * (x - xi) / omega));
  if(give_log) return dens;
  else return exp(dens);
  
}


// Helper function for phi and rho
// Transform x from the real line to [-1,1]
template<class Type>
Type f(Type x){
  Type y = (exp(x) -Type(1))/(Type(1) + exp(x));
  return(y);
}


template<class Type> 
Type objective_function<Type>::operator()(){
  
  // Data-----------------
  DATA_VECTOR(y);
  DATA_INTEGER(model);
  DATA_VECTOR_INDICATOR(keep, y);  // For one-step predictions
  
  // Parameters-----------------
  PARAMETER(log_sigma_y); 
  PARAMETER(log_sigma_h);
  PARAMETER(logit_phi); 
  PARAMETER_VECTOR(log_df_minus_two); // Degrees of freedom in t-distribution
  PARAMETER_VECTOR(alpha); // Skewness parameter in skew normal model
  PARAMETER_VECTOR(logit_rho); // Correlation in leverage model
  PARAMETER_VECTOR(h); // Latent process 
  
  // Transform parameters------------------
  Type sigma_y = exp(log_sigma_y);
  Type sigma_h = exp(log_sigma_h); 
  Type phi = f(logit_phi); 
  
  ADREPORT(sigma_y); 
  ADREPORT(sigma_h); 
  ADREPORT(phi); 
  
  // Negative log likelihood
  Type nll = 0; 
  int N = y.size();
  
  // Contribution from latent process
  // Assume stationary distribution
  nll -= dnorm(h(0), Type(0), sigma_h / sqrt(1 - phi * phi), true); 
  
  for(int i = 1; i < N; i++){
    nll -= dnorm(h(i), phi * h(i - 1), sigma_h, true); 
  }
  
  // Contribution from observations
  
  for(int i = 0; i < N; i++){
    
    switch(model){
      
    // Gaussian
    case 0:
      nll -= keep(i) * dnorm(y(i), Type(0), exp(h(i) / Type(2)) * sigma_y, true);
      break;
    
    // Centered t-distribution
    // last term is contribution from jacobian of linear transformation y = a * x
    case 1:{
      Type df = exp(log_df_minus_two(0)) + Type(2); // To assure that the variance exist
      if(i == 0) ADREPORT(df); 
      Type normalization = exp(h(i) / 2) * sigma_y * sqrt((df - Type(2)) / df); 
      nll -= keep(i) * (dt(y(i) / normalization, df, true) - log(normalization));
      break; 
      }
    
    // Skew normal distribution
    case 2:{
      Type scale = sigma_y * exp(h(i) / Type(2));
      Type delta = alpha(0) / sqrt(1 + alpha(0) * alpha(0)); 
      Type omega = scale / sqrt(Type(1) - Type(2) * delta * delta / M_PI); 
      Type xi = - omega * delta * sqrt(Type(2) / M_PI);
      nll -= keep(i) * skew_norm(y(i), alpha(0), omega, xi, true);
      break;
      }
    
    // Leverage model - normal distribution
    case 3:{
      Type rho = f(logit_rho(0));
      if(i == 0){
        ADREPORT(rho); // only report once
      }
      if(i < (N - 1)){
        Type eta = (h(i + 1) - phi * h(i)) / sigma_h; 
        nll -= keep(i) * dnorm(y(i), sigma_y * exp(h(i) / Type(2)) * rho * eta, sigma_y * exp(h(i) / Type(2)) * sqrt(Type(1) - rho * rho), true); 
      }
      break;
    }
      
    // TODO: 
    // Skew t distribution
    // Skew gaussian with leverage
    // Skew t with leverage
    
    default:
      Rf_error("This distribution is not implementet!");
      break;
    }
  }

  return nll; 
}
  