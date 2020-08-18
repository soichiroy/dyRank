

Rcpp::List update_moments(
  const arma::imat             &dat,
  const arma::imat             &race_attr,
  const arma::ivec             &driver_attr,
  const arma::mat              &lambda,
        std::vector<arma::mat> &c_mk,
        int                     trunc = 5
);

void update_counts(
  const arma::imat             &dat, 
  const arma::mat              &lambda, 
  const arma::imat             &race_attr,  
  const arma::ivec             &driver_attr,
        std::vector<arma::mat> &c_mk,
        int                     trunc = 5        
);

std::vector<arma::mat> initialize_counts(
  const std::vector<arma::imat> &dat, 
  const std::vector<arma::mat>  &lambda, 
  const arma::imat              &race_attr,  
  const std::vector<arma::ivec> &driver_attr,
  const int                     &n_race,
        int                     trunc = 5  
);
