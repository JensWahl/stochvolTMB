#include<TMB.hpp>

// Skew normal distribution with mean zero and unit variance
// @param x double. Where to evaluate density
// @param alpha double. Skewness parameter 
// @sigma_y double. Parameter from observational equation
// @h double. Latent variable 
//@give_log bool. Return log of density
template<class Type> 
Type skew_norm(Type x, Type alpha, Type sigma_y, Type h, bool give_log){
  
  Type scale = sigma_y * exp(h / 2);
  Type delta = alpha / sqrt(1 + alpha * alpha); 
  Type omega = scale / sqrt(1 - 2 * delta * delta / M_PI); 
  Type epsilon = - omega * delta * sqrt(2 / M_PI);
  
  Type dens = log(2) + dnorm(x, epsilon, omega, true) + log(pnorm(alpha * (x - epsilon) / omega));
  
  if(give_log) return dens;
  else return exp(dens);
  
  }


// Helper function for phi
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
  DATA_INTEGER(method);
  
  // Parameters-----------------
  PARAMETER(log_sigma_y); 
  PARAMETER(log_sigma_h);
  PARAMETER(phi_logit); 
  PARAMETER_VECTOR(df); // Degrees of freedom in t-distribution
  PARAMETER_VECTOR(alpha); // Skewness parameter in skew normal model
  PARAMETER_VECTOR(rho); // Correlation in leverage model
  PARAMETER_VECTOR(h); // Latent process 
  
  // Transform parameters------------------
  Type sigma_y = exp(log_sigma_y);
  Type sigma_h = exp(log_sigma_h); 
  Type phi = f(phi_logit); 
  
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
    
    switch(method){
      
    // Gaussian
    case 0:
      nll -= dnorm(y(i), Type(0), exp(h(i) / 2) * sigma_y, true);
      break;
    
    // Centered t-distibution
    // last term is contribution from jacobian of linear transformation y = a * x
    case 1:
      nll -= dt(y(i) / (exp(h(i) / 2) * sigma_y), df(0), true) - log((exp(h(i) / 2) * sigma_y));
      break; 
    
    // Skew normal distribution
    case 2: {
      nll -= skew_norm(y(i), alpha(0), sigma_y, h(i), true);
      // Type delta = alpha(0) / sqrt(1 + alpha(0) * alpha(0));
      // Type std_y = (y(i) - delta * sqrt(2 / M_PI) * exp(h(i) / 2) * sigma_y);
      // std_y = std_y / (sqrt(1 - delta * delta / M_PI) * exp(h(i) / 2) * sigma_y); 
      // nll -= dsn(std_y, alpha(0), true);
      break;
      }
    
    // Leverage model - normal distribution
    case 3:{
      
      if(i < (N - 1)){
      nll -= dnorm(y(i), sigma_y * exp(h(i) / 2) * (rho(0) / sigma_h * (h(i + 1) - phi * h(i))), 
                   sigma_y * exp(h(i) / 2) * sqrt(1 - rho(0) * rho(0)), true); 
      }
      break;
    }
    
    
    
    // TO DO: 
    // Leverage model 
    // Skew normal distribution - Ok 
    // Skew t distribution
    
    default:
      std::cout << "This distribution is not implementet!" << std::endl;
      break;
    }
    
    
    
  }
  // Add estimate for conditional variance 
  //vector<Type> cond_var = exp(h) * sigma_y * sigma_y; 
  
  //ADREPORT(cond_var);
  
  return nll; 
}
  