#include<TMB.hpp>


// Helper function for phi
// Transform x from the real line to [-1,1]
template<class Type>
Type f(Type x){
  Type y = (exp(x) -Type(1))/(Type(1) + exp(x));
  return(y);
}


template<class Type> 
Type objective_function<Type>::operator()(){
  
  // Data
  DATA_VECTOR(y);
  DATA_INTEGER(n); 
  
  // Parameters
  PARAMETER(log_sigma_y); 
  PARAMETER(log_sigma);
  PARAMETER(phi_logit); 
  PARAMETER_VECTOR(h); // Latent process 
  
  // Transform parameters
  Type sigma_y = exp(log_sigma_y);
  Type sigma = exp(log_sigma); 
  Type phi = f(phi_logit); 
  
  ADREPORT(sigma_y); 
  ADREPORT(sigma); 
  ADREPORT(phi); 
  
  // Negative log likelihood
  Type nll = 0; 
  
  // Contribution from latent process
  // Assume stationary distribution
  nll -= dnorm(h(0), Type(0), sigma/sqrt(1 - phi*phi), true); 
  
  for(int i = 1; i < n; i++){
    
    nll -= dnorm(h(i), phi*h(i-1), sigma, true); 
    
  }
  
  // Contribution from observations
  
  for(int i = 0; i < n; i++){
    
    nll -= dnorm(y(i), Type(0), exp(h(i)/2)*sigma_y, true);
    
  }
  // Add estimate for conditional variance 
  vector<Type> cond_var = exp(h)*sigma_y*sigma_y; 
  
  ADREPORT(cond_var);
  
  return nll; 
}
  