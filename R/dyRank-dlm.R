

##
## Sampling dynamic rating 
## 


#' Sample Global Mean Parameter via FFBS
#'
#' @keywords internal 
#' @param lambda_mat A list of time-specific mean parameter. 
#' @param tau A variance parameter of the emission model. 
#' @param m0 Mean parameter of the initial condition. 
#' @param s0 Variance parameter of the initial condition.
#' @param delta An innovation parameter (variance) for the state equation.
#' @return A vector of dynamic mean value of length T (the length of the career)
#'
#' @examples 
#' # generate data 
#' set.seed(1234)
#' n_tenure <- 20
#' lambda   <- rep(NA, n_tenure)
#' lambda_list <- vector('list', length = n_tenure)
#' tau       <- runif(1, 0.5, 1.5)
#' delta     <- 0.5
#' for (i in 1:n_tenure) {
#'   if (i == 1) {
#'     # initial condition 
#'     lambda[1] <- rnorm(1, 0, 0.5)
#'   } else {
#'     lambda[i] <- rnorm(1, mean = lambda[i-1], sd = sqrt(delta))    
#'   }
#' 
#'   lambda_list[[i]] <- rnorm(5, mean = lambda[i], sd = tau)
#' }
#' 
#' # fit 
#' lambda_fit <- FFBS(do.call(cbind, lambda_list), tau = tau, m0 = 0, s0 = 0.5^2, delta = delta)
#' 
#' # plot 
#' plot(lambda, type = 'o', pch = 16, ylim = c(-3, 4))
#' lines(lambda_fit, type = 'o', pch = 21, col = 'red')
#' lines(sapply(lambda_list, mean), type = 'o', pch = 17)
FFBS <- function(lambda_mat, tau, m0 = 0, s0 = 0.25, delta = 0.5) {
  
  ## get info 
  time_len <- ncol(lambda_mat)
  lambda_mean <- rep(NA, time_len)
  n_rank_types <- nrow(lambda_mat)
  
  ## save obj 
  mu_tt <- mu_onestep <- rep(NA, time_len)
  var_tt <- var_onestep <- rep(NA, time_len)
  mu_back <- var_back <- rep(NA, time_len)
  
  ##
  ## forward-filtering stage 
  ## 
  ## - Use Kalman Filter to compute moments of p(λ[t] | Y^t)
  ## - Output is μ[t|t] and σ2[t|t]
  for (tt in 1:time_len) {
    if (tt == 1) {
      ## initial condition 
      mu_onestep[tt]  <- m0
      var_onestep[tt] <- s0 + delta  
    } else {
      ## compute μ[t|t-1] = μ[t-1]
      mu_onestep[tt] <- mu_tt[tt-1]
      var_onestep[tt] <- var_tt[tt-1] + delta 
    }
    
    ## update μ[t|t] and σ[t|t]
    var_tt[tt] <- 1 / (n_rank_types / tau + 1 / var_onestep[tt])
    mu_tt[tt]  <- var_tt[tt] * 
      (sum(lambda_mat[,tt]) / tau + mu_onestep[tt] / var_onestep[tt])
  }
  
  ##
  ## backward sampling stage 
  ## 
  ## - sample from T to 1 
  for (tt in time_len:1) {
    if (time_len == tt) {
      mu_back[tt] <- mu_tt[tt] 
      var_back[tt] <- var_tt[tt]       
    } else {
      ## Fruhwirth-Schnatter (1992)
      at <- var_tt[tt] / (var_tt[tt] + delta)
      var_back[tt] <- (1 - at) * var_tt[tt]
      mu_back[tt]  <- (1 - at) * mu_tt[tt] + at * lambda_mean[tt+1]
    }
    
    ## sample states 
    # cat("var_back[tt] = ", var_back[tt], "\n")
    # if (var_back[tt] < 0) var_back[tt] <- s0
    lambda_mean[tt] <- rnorm(1, mean = mu_back[tt], sd = sqrt(var_back[tt]))
  }
  
  return(lambda_mean)  
}
