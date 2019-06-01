#include<TMB.hpp>

// Incomplete Beta function 
template<class Type> 
Type 


template<class Type> 
Type dst2(Type x, Type lambda, Type nu, bool give_log){
  
  Type c = exp(lgamma((nu + 1) / 2)) / (M_PI * (nu - 2) * exp(lgamma(nu / 2))); 
  Type a = 4 * lambda * c * ((nu - 2) / (nu - 1)); 
  Type b = sqrt( 1 + 3 * lambda * lambda - a * a); 
  
  Type dens = 0; 
  
  if( x < - a / b){
    dens = log(b) + log(c) - (nu + 1) / 2 * log(1 + 1 / (nu - 1) * pow(((b * x + a) / (1 + lambda)), 2));
  } else{
    dens =     dens = log(b) + log(c) - (nu + 1) / 2 * log(1 + 1 / (nu - 1) * pow(((b * x + a) / (1 - lambda)), 2));
  }
  
  if(give_log) return dens; 
  else return exp(dens);
}



template<class Type> 
Type objective_function<Type>::operator()(){
  
  DATA_VECTOR(x); 
  PARAMETER(lambda); 
  PARAMETER(nu); 
  
  Type nll = 0; 
  
  for(int i = 0; i < x.size(); i++){
    nll -= dst(x(i), lambda, nu, true);
  }
  
  return nll; 
}