
#include <RcppArmadillo.h>
#include "update_count.h"
#include "update_lambda.h"

// single iteration 
// [[Rcpp::export]]
void hdyRank_gibbs(
        std::vector<arma::mat>  &lambda,
        std::vector<arma::vec>  &lambda_mean,
        std::vector<arma::mat>  &sigma,
        std::vector<arma::mat>  &c_mk,
  const std::vector<arma::imat> &dat, 
  const arma::imat              &race_attr,  
  const std::vector<arma::ivec> &driver_attr,
  const int                     &id_driver_fix,
  const int                     &trunc

) {
  
  // info 
  int n_driver = dat.size();
  int n_rank_types = race_attr.n_cols;
  
  // hyper params 
  double v0 = 4; 
  arma::mat S0 = arma::eye(n_rank_types, n_rank_types);
  
  //
  // loop over drivers 
  //
  for (int i = 0; i < n_driver; ++i) {
    // check if this drive is the reference category
    bool is_fix = (id_driver_fix == i) ? true : false;
    
    // data augmentation & update c_mk 
    // Rcpp::Rcout << "Updating moments ..." << std::endl;
    Rcpp::List moments = update_moments(
      dat[i], race_attr, driver_attr[i], lambda[i], c_mk, trunc
    );
    
    // sample λ
    // Rcpp::Rcout << "Updating λ ..." << std::endl;
    lambda[i] = hdyRank_update_lambda(
      moments["Z"], moments["Omega"], lambda_mean[i],
      sigma[i], n_rank_types, driver_attr[i], is_fix
    );
        
    // update lambda mean 
    // Rcpp::Rcout << "Updating λ mean ..." << std::endl; 
    lambda_mean[i] = FFBSmult_cpp(lambda[i], sigma[i], is_fix);
    
    // update Σ
    // Rcpp::Rcout << "Updating Σ ..." << std::endl;
    sigma[i] = update_cov_cpp(
      lambda[i], lambda_mean[i], v0, S0
    );
    
    // update counts
    // Rcpp::Rcout << "Updating counts ..." << std::endl;    
    update_counts(
      dat[i], lambda[i], race_attr, driver_attr[i], c_mk, trunc
    );
    
    // Rcpp::Rcout << "Done with driver" << i << std::endl;
  }
}




// dyRank Rcpp version 
// [[Rcpp::export]]
Rcpp::List hdyRank_cpp(
        std::vector<arma::mat>  &lambda,
        std::vector<arma::vec>  &lambda_mean,
        std::vector<arma::mat>  &sigma,
        std::vector<arma::mat>  &c_mk,
  const std::vector<arma::imat> &dat, 
  const arma::imat              &race_attr,  
  const std::vector<arma::ivec> &driver_attr,
  const int &mcmc,
  const int &burnin,
  const int &thin,
  const int &id_driver_fix,
  const int &trunc
) {
  
  int total_iter = mcmc + burnin;
  std::vector<std::vector<arma::mat>> save_lambda;
  std::vector<std::vector<arma::vec>> save_lambda_mean;
  std::vector<std::vector<arma::mat>> save_sigma;
  for (int iter = 0; iter < total_iter; ++iter) {
    // update parameters 
    hdyRank_gibbs(
      lambda, lambda_mean, sigma, c_mk,
      dat, race_attr, driver_attr, id_driver_fix, trunc
    );
    
    // save
    if (iter > burnin && (iter % thin == 0)) {
      save_lambda.push_back(lambda);
      save_sigma.push_back(sigma);
      save_lambda_mean.push_back(lambda_mean);      
    }

    // allow user interruption 
    if (iter % 5 == 0) {
      // Rcpp::Rcout << "Iter = " << iter << std::endl; 
      Rcpp::checkUserInterrupt();    
    }    
  }
  
  return Rcpp::List::create(
    Rcpp::Named("sigma")  = save_sigma, 
    Rcpp::Named("lambda") = save_lambda,
    Rcpp::Named("lambda_mean") = save_lambda_mean
  );  
}
