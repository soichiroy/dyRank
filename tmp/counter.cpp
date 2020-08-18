// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]

//' C++ implementation of count updates 
//' @keywords internal
// [[Rcpp::export]]
std::vector<arma::vec> update_counts_cpp(
  const std::vector<arma::vec>  &c_mk,
  const std::vector<arma::ivec> &dat_GP,
  const std::vector<arma::ivec> &time_GP,
  const std::vector<arma::ivec> &delta,
  const arma::vec               &lambda, 
  const int                     &tr   
) {


  std::vector<arma::vec> c_mk_new = c_mk; 
  int n_GP = dat_GP.size(); 

  for (int i = 0; i < n_GP; ++i) {
    int index_GP = dat_GP[i](0)-1;
    int index_tenure = time_GP[i](0)-1;
    int n_finish = c_mk[index_GP].n_elem;
 
    for (int k = 0; k < (n_finish - tr); ++k) {
      if (delta[i](k) == 1) {
        c_mk_new[index_GP](k) += exp(lambda(index_tenure));
      }
    }
 }
 
 return c_mk_new;
}
