

arma::mat update_lambda_cpp(
  const arma::mat  &Z,
  const arma::mat  &Omega,
  const arma::vec  &lambda_mean, 
  const arma::mat  &Sigma,
  const int        &n_rank_types,
  const arma::ivec &driver_attr,
  const bool       &is_fix  
);

arma::vec FFBSmult_cpp(
  const arma::mat &lambda_mat, 
  const arma::mat &Sigma,
  const bool      &is_fix,
  const double    m0 = 0.0,
  const double    s0 = 0.5,
  const double    delta = 0.5
);

arma::mat update_cov_cpp(
  const arma::mat &lambda_mat,
  const arma::vec &lambda_mean,
  const double    &v0,
  const arma::mat &S0
);
