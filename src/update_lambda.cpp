// RcppDist
#include <RcppDist.h>

// [[Rcpp::plugins("cpp11")]]

// Update λ for driver i 
// @return A matrix of λ where rows correspond to time and columns to rank type.
// [[Rcpp::export]]
arma::mat hdyRank_update_lambda(
  const arma::mat  &Z,
  const arma::mat  &Omega,
  const arma::vec  &lambda_mean, 
  const arma::mat  &Sigma,
  const int        &n_rank_types,
  const arma::ivec &driver_attr,
  const bool       &is_fix
) {
  
  // obtain tenure length
  int n_tenure = driver_attr(0);
  
  // new mean 
  arma::mat lambda(n_tenure, n_rank_types);
  
  // inverse 
  arma::mat sigma_inv = arma::inv(Sigma);
  
  for (int tt = 0; tt < n_tenure; ++tt) {
    // prior mean
    arma::vec lambda_prior(n_rank_types);
    lambda_prior.fill(lambda_mean(tt));
    
    // variance 
    arma::mat SS = arma::inv(arma::diagmat(Omega.row(tt)) + sigma_inv);
    
    // mean 
    arma::vec mm = SS * (arma::trans(Z.row(tt)) + sigma_inv * lambda_prior);
    
    // sample from multivariate normal 
    // lambda.row(tt) = rmv(mm, SS);
    arma::mat lambda_tmp = rmvnorm(1, mm, SS);
    lambda.row(tt) = lambda_tmp.row(0);
  }
  
  if (is_fix) { lambda.row(0) = arma::zeros<arma::rowvec>(lambda.n_cols); }
  
  return lambda; 
}

//' FFBS for Multivariate Model
//' @keywords internal
// [[Rcpp::export]]
arma::vec FFBSmult_cpp(
  const arma::mat &lambda_mat, 
  const arma::mat &Sigma,
  const bool      &is_fix,
  const double    m0 = 0.0,
  const double    s0 = 0.25,
  const double    delta = 0.25
) {
  arma::vec lambda_mean = arma::zeros(lambda_mat.n_rows);
  
  // get info 
  int time_len = lambda_mat.n_rows; 
  // int n_rank_types = lambda_mat.n_rows;
  
  // Σ^{-1}
  arma::mat Sinv = arma::inv(Sigma);
  
  // initialize objects 
  arma::vec mu_tt(time_len);      arma::vec var_tt(time_len);
  arma::vec mu_onestep(time_len); arma::vec var_onestep(time_len);
  arma::vec mu_back(time_len);    arma::vec var_back(time_len);
  
  /*
  * Forward-filtering stage 
  *
  * - Use Kalman Filter to compute moments of p(λ[t] | Y[1:t])
  *
  */
  for (int tt = 0; tt < time_len; ++tt) {
    if (tt == 0) {
      mu_onestep(tt)  = m0; 
      var_onestep(tt) = s0 + delta;
    } else {
      mu_onestep(tt)  = mu_tt(tt-1);
      var_onestep(tt) = var_tt(tt-1) + delta; 
    }
    
    // update μ[t|t] and σ[t|t]
    var_tt(tt) = 1.0 / (arma::accu(Sinv) + 1.0 / var_onestep(tt));
    mu_tt(tt)  = var_tt(tt) * (
      arma::accu(Sinv * arma::trans(lambda_mat.row(tt))) + 
      mu_onestep(tt) / var_onestep(tt)
    );
  }

  /*
  * Backward sampling 
  *
  */
  for (int tt = time_len-1; tt >= 0; --tt) {
    if (tt == (time_len-1)) {
      mu_back(tt)  = mu_tt(tt);
      var_back(tt) = var_tt(tt);
    } else {
      double at = var_tt(tt) / (var_tt(tt) + delta);
      var_back(tt) = (1.0 - at) * var_tt(tt);
      mu_back(tt)  = (1.0 - at) * mu_tt(tt) + at * lambda_mean(tt+1);
    }
    
    // sample 
    lambda_mean(tt) = R::rnorm(mu_back(tt), sqrt(var_back(tt)));
  }
  
  
  if (is_fix) lambda_mean(0) = 0.0;
  
  return lambda_mean;
  
}


//' Update variance-covariance matrix 
//'
//' @keywords internal
//' @param lambda_mat Observed λ matrix.
//' @param v0 Degree of freedom parameter of the inverse Wishart (prior).
//' @param S0 Scale matrix of the inverse Wishart (prior).
//' @return A single draw from the inverse Wishart distribution.
// [[Rcpp::export]]
arma::mat update_cov_cpp(
  const arma::mat &lambda_mat,
  const arma::vec &lambda_mean,
  const double    &v0,
  const arma::mat &S0
) {
  
  // number of observations 
  int n_obs = lambda_mat.n_rows; 
  int vn = v0 + n_obs; 
  
  arma::mat lambda_demean = lambda_mat;
  
  // demeaning 
  for (int i = 0; i < n_obs; ++i) {
    lambda_demean.row(i) -= lambda_mean(i);
  }

  arma::mat Sn = S0;
  if (n_obs == 1) {
     Sn += lambda_demean.t() * lambda_demean;
  } else {
    // update parameters of inverse Wishart
    Sn += n_obs * arma::cov(lambda_demean);
  }
  
  // draw from the inverse Wishart
  return riwish(vn, Sn);
}


//' C++ implementation of FFBB algorithm 
//' @keywords internal
// [[Rcpp::export]]
arma::mat FFBSsingle_cpp(
  const arma::mat &Z,
  const arma::mat &Omega,
  const bool      &is_fix, 
  const double    m0 = 0.0,
  const double    s0 = 0.25, 
  const double    delta = 0.25
) {
    
  // get info 
  int time_len = Z.n_rows;
  arma::mat lambda = arma::zeros(time_len, 1);

  // initialize objects 
  arma::vec mu_tt(time_len);      arma::vec var_tt(time_len);
  arma::vec mu_onestep(time_len); arma::vec var_onestep(time_len);
  arma::vec mu_back(time_len);    arma::vec var_back(time_len);
  
  /*
  * Forward-filtering stage 
  *
  * - Use Kalman Filter to compute moments of p(λ[t] | Y[1:t])
  *
  */

  for (int tt = 0; tt < time_len; ++tt) {
    if (tt == 0) {
      mu_onestep(tt)  = m0; 
      var_onestep(tt) = s0 + delta;
    } else {
      mu_onestep(tt)  = mu_tt(tt-1);
      var_onestep(tt) = var_tt(tt-1) + delta; 
    }
    
    // update μ[t|t] and σ[t|t]
    var_tt(tt) = 1.0 / (Omega(tt,0) + 1.0 / var_onestep(tt));
    mu_tt(tt)  = var_tt(tt) * (Z(tt,0) + mu_onestep(tt) / var_onestep(tt));
  }

  /*
  * Backward sampling 
  *
  */
  for (int tt = time_len-1; tt >= 0; --tt) {
    if (tt == (time_len-1)) {
      mu_back(tt)  = mu_tt(tt);
      var_back(tt) = var_tt(tt);
    } else {
      double at = var_tt(tt) / (var_tt(tt) + delta);
      var_back(tt) = (1.0 - at) * var_tt(tt);
      mu_back(tt)  = (1.0 - at) * mu_tt(tt) + at * lambda(tt+1,0);
    }
    
    // sample 
    lambda(tt,0) = R::rnorm(mu_back(tt), sqrt(var_back(tt)));
  }
  
  
  // reference group fix the ability to zero
  if (is_fix) lambda(0,0) = 0.0;
  
  return lambda;
}
