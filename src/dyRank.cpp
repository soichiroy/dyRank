#include <RcppArmadillo.h>
#include "update_count.h"
#include "update_lambda.h"

// single iteration 
// [[Rcpp::export]]
void dyRank_gibbs(
        std::vector<arma::mat>  &lambda,
        std::vector<arma::mat>  &c_mk,
  const std::vector<arma::imat> &dat, 
  const arma::imat              &race_attr,  
  const std::vector<arma::ivec> &driver_attr,
  const int                     &id_driver_fix,
  const int                     &trunc              
) {
  
  // info 
  int n_driver = dat.size();
  
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
  
    // update lambda
    // Rcpp::Rcout << "Updating λ mean ..." << std::endl; 
    lambda[i] = FFBSsingle_cpp(moments["Z"], moments["Omega"], is_fix);
        
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
Rcpp::List dyRank_cpp(
        std::vector<arma::mat>  &lambda,
        std::vector<arma::mat>  &c_mk,
  const std::vector<arma::imat> &dat, 
  const arma::imat              &race_attr,  
  const std::vector<arma::ivec> &driver_attr,
  const int                     &mcmc,
  const int                     &burnin,
  const int                     &thin,
  const int                     &id_driver_fix,
  const int                     &trunc
) {
  
  int total_iter = mcmc + burnin;
  std::vector<std::vector<arma::mat>> save_lambda;
  for (int iter = 0; iter < total_iter; ++iter) {
    // update parameters 
    dyRank_gibbs(
      lambda, c_mk, 
      dat, race_attr, driver_attr, id_driver_fix, trunc
    );
    
    // save
    if (iter > burnin && (iter % thin == 0)) {
      save_lambda.push_back(lambda);
    }

    // allow user interruption 
    if (iter % 5 == 0) {
      // Rcpp::Rcout << "Iter = " << iter << std::endl; 
      Rcpp::checkUserInterrupt();    
    }    
  }
  

  return Rcpp::List::create(
    Rcpp::Named("lambda") = save_lambda
  );
}
