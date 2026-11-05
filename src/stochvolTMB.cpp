#define TMB_LIB_INIT R_init_stochvolTMB
#include<TMB.hpp>

// Stable log(pnorm(x)): pnorm underflows in the far left tail, so use the Mills-ratio expansion
// below x = -10. Both branches get clamped inputs so the AD tape never sees Inf/NaN.
template<class Type>
Type log_pnorm(Type x){
  Type cut = Type(-10);
  Type x_hi = CppAD::CondExpGe(x, cut, x, cut);
  Type x_lo = CppAD::CondExpLt(x, cut, x, cut);
  Type val_hi = log(pnorm(x_hi));
  Type x2 = x_lo * x_lo;
  Type val_lo = dnorm(x_lo, Type(0), Type(1), true) - log(-x_lo) +
    log(Type(1) - Type(1) / x2 + Type(3) / (x2 * x2) - Type(15) / (x2 * x2 * x2));
  return CppAD::CondExpGe(x, cut, val_hi, val_lo);
}

// Skew normal distribution with location xi, scale omega and shape alpha
// @param x double. Where to evaluate density
// @param alpha double. Shape parameter
// @param omega double. Scale parameter
// @param xi double. Location parameter
// @param give_log bool. Return log of density
template<class Type>
Type skew_norm(Type x, Type alpha, Type omega, Type xi, bool give_log){
  Type dens = log(Type(2)) + dnorm(x, xi, omega, true) + log_pnorm(alpha * (x - xi) / omega);
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

// Shape alpha of a skew normal with skewness gamma1 (inverts gamma1 = (4 - pi) / 2 * mu^3 / (1 - mu^2)^(3 / 2),
// mu = delta * sqrt(2 / pi)). Undefined derivative at gamma1 = 0, so never start the optimizer exactly there.
template<class Type>
Type skewness_to_alpha(Type gamma1){
  Type abs_gamma1 = CppAD::CondExpGe(gamma1, Type(0), gamma1, -gamma1);
  Type r = pow(Type(2) * abs_gamma1 / (Type(4) - M_PI), Type(1) / Type(3));
  r = CppAD::CondExpGe(gamma1, Type(0), r, -r);
  Type delta = r / sqrt(Type(1) + r * r) / sqrt(Type(2) / M_PI);
  return delta / sqrt(Type(1) - delta * delta);
}

// Theoretical bound is 0.9953 (alpha -> Inf); 0.99 caps |alpha| at about 27, away from the half-normal limit.
#define MAX_SKEWNESS 0.99


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
  // Skewness in skew normal models. Estimated instead of alpha because the likelihood is flat
  // (~ alpha^3) at alpha = 0, which traps the optimizer, but it is regular in the skewness.
  PARAMETER_VECTOR(logit_skewness);
  PARAMETER_VECTOR(logit_rho); // Correlation in leverage model
  PARAMETER_VECTOR(h); // Latent process

  // Transform parameters------------------
  Type sigma_y = exp(log_sigma_y);
  Type sigma_h = exp(log_sigma_h);
  Type phi = f(logit_phi);

  ADREPORT(sigma_y);
  ADREPORT(sigma_h);
  ADREPORT(phi);

  // Standardized skew normal (mean zero, unit variance): shape alpha, scale omega0, location xi0
  Type skewness = 0, alpha = 0, delta = 0, omega0 = 1, xi0 = 0;
  if(model == 2 || model == 4){
    skewness = Type(MAX_SKEWNESS) * f(logit_skewness(0));
    alpha = skewness_to_alpha(skewness);
    delta = alpha / sqrt(Type(1) + alpha * alpha);
    omega0 = Type(1) / sqrt(Type(1) - Type(2) * delta * delta / M_PI);
    xi0 = -omega0 * delta * sqrt(Type(2) / M_PI);
    ADREPORT(skewness);
    ADREPORT(alpha);
  }

  Type rho = 0;
  if(model == 3 || model == 4){
    rho = f(logit_rho(0));
    ADREPORT(rho);
  }

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
      Type df = exp(log_df_minus_two(0)) + Type(2); // To ensure that the variance exist
      if(i == 0) ADREPORT(df);
      Type normalization = exp(h(i) / 2) * sigma_y * sqrt((df - Type(2)) / df);
      nll -= keep(i) * (dt(y(i) / normalization, df, true) - log(normalization));
      break;
      }

    // Skew normal distribution
    case 2:{
      Type scale = sigma_y * exp(h(i) / Type(2));
      nll -= keep(i) * skew_norm(y(i), alpha, omega0 * scale, xi0 * scale, true);
      break;
      }

    // Leverage model - normal distribution
    case 3:{
      if(i < (N - 1)){
        Type eta = (h(i + 1) - phi * h(i)) / sigma_h;
        nll -= keep(i) * dnorm(y(i), sigma_y * exp(h(i) / Type(2)) * rho * eta, sigma_y * exp(h(i) / Type(2)) * sqrt(Type(1) - rho * rho), true);
      }
      break;
    }

    // Skew gaussian with leverage: eta(i) = rho * z(i) + sqrt(1 - rho^2) * e(i), z(i) skew normal return shock.
    // The reverse order (skew noise added to rho * eta) is very sharp in h(i + 1) and breaks the Laplace approximation.
    case 4:{
      Type scale = sigma_y * exp(h(i) / Type(2));
      Type obs = skew_norm(y(i), alpha, omega0 * scale, xi0 * scale, true);
      if(i < (N - 1)){
        // Factorise as p(h(i + 1) | h(i)) * p(y(i) | h(i), h(i + 1)) so y only enters keep-weighted terms (OSA residuals).
        // Marginally eta is skew normal, since it is a skew normal plus an independent gaussian.
        Type c = sqrt(Type(1) - rho * rho);
        Type z = y(i) / scale;
        Type eta = (h(i + 1) - phi * h(i)) / sigma_h;
        Type omega_eta = sqrt(rho * rho * omega0 * omega0 + c * c);
        Type delta_eta = rho * omega0 * delta / omega_eta;
        Type alpha_eta = delta_eta / sqrt(Type(1) - delta_eta * delta_eta);
        Type log_p_eta = skew_norm(eta, alpha_eta, omega_eta, rho * xi0, true);
        // Replace the gaussian AR(1) term added above with the skew normal transition
        nll -= log_p_eta - dnorm(eta, Type(0), Type(1), true);
        obs += dnorm(eta, rho * z, c, true) - log_p_eta;
      }
      nll -= keep(i) * obs;
      break;
    }

    // TODO:
    // Skew t distribution
    // Skew t with leverage

    default:
      Rf_error("This distribution is not implementet!");
      break;
    }
  }

  return nll;
}
