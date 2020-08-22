
#include <RcppArmadillo.h>
#include "pgdraw.h"


//' Update moments for a driver 
//' @keywords internal 
//' @param dat A matrix of one driver's information.
// [[Rcpp::export]]
Rcpp::List update_moments(
  const arma::imat             &dat,
  const arma::imat             &race_attr,
  const arma::ivec             &driver_attr,
  const arma::mat              &lambda,
        std::vector<arma::mat> &c_mk,
        int                     trunc = 5
) {
  
  // number of observations for this driver 
  int n_obs = dat.n_rows; 
  int n_rank_types = race_attr.n_cols;
  
  // extract driver's info 
  int n_tenure = driver_attr(0);
  int min_year = driver_attr(1);
  // int max_year = driver_attr(2);
  
  // loop over observations (i.e. races x rank types)
  arma::mat Z     = arma::zeros(n_tenure, n_rank_types);
  arma::mat Omega = arma::zeros(n_tenure, n_rank_types);
  
  for (int j = 0; j < n_obs; ++j) {
    // extract infor 
    int obs_rank = dat(j,0); // observed rank (i.e., outcome)
    int id_race  = dat(j,1); // race ID 
    int id_year  = dat(j,2) - min_year; // time ID (zero index)
    int id_rank  = dat(j,3); // ranking type (rank ID)
    
    // get the number of finishes 
    int n_finish = race_attr(id_race, id_rank);
    
    // loop over all ranks up to obs_rank
    int end_of_loop = std::min(obs_rank, n_finish-trunc);
    for (int k = 0; k <= end_of_loop; ++k) {
      // create outcome 
      double Y = (obs_rank == k) ? 1.0 : 0.0;
      
      // update counts 
      c_mk[id_race](k, id_rank) -= exp(lambda(id_year, id_rank));
      
      // PG augmentation
      double tmp = lambda(id_year, id_rank) - log(c_mk[id_race](k, id_rank));
      double omega = samplepg(tmp); 
      double kappa = Y - 0.5;
      
      // compute mean and variance 
      // Z(id_year, id_rank) += kappa / omega + log(c_mk[id_race](k, id_rank));
      Z(id_year, id_rank)     += kappa + omega * log(c_mk[id_race](k, id_rank));
      Omega(id_year, id_rank) += omega; 
    }
  } // end of observations 
  
  return Rcpp::List::create(
    Rcpp::Named("Z")     = Z, 
    Rcpp::Named("Omega") = Omega
  );
}


// Update counts (c_mk) based on the new values of lambda 
// @return Updated c_mk.
// @param dat A matrix 
// @param lambda A matrix of new values of λ
// @param race_attr A matrix of race attributes.
// [[Rcpp::export]]
void update_counts(
  const arma::imat             &dat, 
  const arma::mat              &lambda, 
  const arma::imat             &race_attr,  
  const arma::ivec             &driver_attr,
        std::vector<arma::mat> &c_mk,
        int                     trunc = 5
) {

  // number of observations for this driver 
  int n_obs = dat.n_rows; 
  int min_year = driver_attr(1);
  
  for (int j = 0; j < n_obs; ++j) {    
    // extract infor 
    int obs_rank = dat(j,0); // observed rank (i.e., outcome)
    int id_race  = dat(j,1); // race ID 
    int id_year  = dat(j,2) - min_year; // time ID (zero index)
    int id_rank  = dat(j,3); // ranking type (rank ID)
    
    // get the number of finishes 
    int n_finish = race_attr(id_race, id_rank);
        
    // loop over all ranks up to obs_rank
    int end_of_loop = std::min(obs_rank, n_finish-trunc);
    for (int k = 0; k <= end_of_loop; ++k) {
      // update counts 
      c_mk[id_race](k, id_rank) += exp(lambda(id_year, id_rank));
    }
  }
}


//' Initialize c_mk 
//' @return Updated c_mk.
//' @param dat A matrix 
//' @param lambda A matrix of new values of λ
//' @param race_attr A matrix of race attributes.
//' @keywords internal
// [[Rcpp::export]]
std::vector<arma::mat> initialize_counts(
  const std::vector<arma::imat> &dat, 
  const std::vector<arma::mat>  &lambda, 
  const arma::imat              &race_attr,  
  const std::vector<arma::ivec> &driver_attr,
  const int                     &n_race,
        int                      trunc = 5
) {
  
  // initailize c_mk 
  std::vector<arma::mat> c_mk;
  int n_rank_types = race_attr.n_cols;
  
  for (int i = 0; i < n_race; ++i) {
    int n_finish = arma::max(race_attr.row(i));
    arma::mat tmp = arma::zeros(n_finish, n_rank_types); 
    c_mk.push_back(tmp);
  }

  // number of observations for this driver 
  // loop over drivers 
  for (int i = 0; i < dat.size(); ++i) {
    int n_obs   = dat[i].n_rows; 
    int min_year = driver_attr[i](1);
    for (int j = 0; j < n_obs; ++j) {
      // extract infor 
      int obs_rank = dat[i](j,0); // observed rank (i.e., outcome)
      int id_race  = dat[i](j,1); // race ID 
      int id_year  = dat[i](j,2) - min_year; // time ID (zero index)
      int id_rank  = dat[i](j,3); // ranking type (rank ID)
  
      // get the number of finishes 
      int n_finish = race_attr(id_race, id_rank);
  
      // loop over all ranks up to obs_rank
      int end_of_loop = std::min(obs_rank, n_finish-trunc);
      for (int k = 0; k <= end_of_loop; ++k) {
        // update counts 
        c_mk[id_race](k, id_rank) += exp(lambda[i](id_year, id_rank));
      }
    }
  }
  return c_mk;
}
