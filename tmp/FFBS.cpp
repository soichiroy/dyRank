// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]

//' C++ implementation of FFBB algorithm 
//' @keywords internal
// [[Rcpp::export]]
arma::vec FFBScpp(
  const arma::mat &lambda_mat, 
  const double    &tau, 
  const double    m0 = 0.0,
  const double    s0 = 0.5, 
  const double    delta = 0.5
) {
  
  arma::vec lambda_mean = arma::zeros(lambda_mat.n_cols);
  
  
  // get info 
  int time_len = lambda_mat.n_cols; 
  int n_rank_types = lambda_mat.n_rows;
  
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
    var_tt(tt) = 1.0 / (n_rank_types / tau + 1.0 / var_onestep(tt));
    mu_tt(tt)  = var_tt(tt) * (
      sum(lambda_mat.col(tt)) / tau + mu_onestep(tt) / var_onestep(tt)
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
  return lambda_mean;
}
