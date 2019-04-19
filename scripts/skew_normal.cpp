#include<TMB.hpp>

// Skew normal distribution with mean zero and unit variance
// @param x double. Where to evaluate density
// @param alpha double. Skewness parameter 
// @sigma_y double. Parameter from observational equation
// @h double. Latent variable 
//@give_log bool. Return log of density
template<class Type>
Type skew_norm(Type x, Type alpha, bool give_log){

  Type delta = alpha / sqrt(1 + alpha * alpha);
  Type omega = sqrt(1 / (1 - 2 * delta * delta / M_PI));
  Type epsilon = - omega * delta * sqrt(2 / M_PI);

  Type dens = log(2.0) - log(omega) + dnorm((x - epsilon) / omega, Type(0), Type(1), true) + log(pnorm(alpha * (x - epsilon) / omega));

  if(give_log) return dens;
  else return exp(dens);

}

template<class Type> 
Type objective_function<Type>::operator()(){
  
  DATA_VECTOR(x);
  DATA_INTEGER(mod);
  PARAMETER(alpha);
  
  Type nll = 0; 
  int n = x.size();
  
  for(int i = 0; i < n; i++){
    
    switch(mod){
      case 0:
        nll -= skew_norm(x(i), alpha, true);
      break;
      
      case 1:
        nll -= dsn(x(i), alpha, true);
      break;
      
      default:
        std::cout << "Not implemented" << std::endl; 
      break;
    }
  }
  
  return nll; 
}