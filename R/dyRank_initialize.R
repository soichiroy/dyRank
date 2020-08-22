

## initialize inputs 
# 

#' Function to initialize parameters
#' @param data An output of \code{dyRank_data()}.
#' @importFrom purrr map map2 
initialize_params <- function(data) {
  
  if (!("dyRank.data" %in% class(data))) stop("Invalid input")
  
  ## initialize Σ
  S0 <- diag(rep(0.25, data$n_rank_types))
  S0[S0==0] <- 0.05
  sigma <- rep(list(S0), data$n_drivers)
  
  ## initialize λ̅ (prior mean) 
  lambda_mean <- map(data$driver_attr, ~ draw_lambda0(.x[1]))
  
  ## initialize λ(t)_ih
  lambda_th <- map2(lambda_mean, sigma, ~ draw_lambda_th(.x, .y))
  
  ## return 
  init <- list(
    sigma = sigma, 
    lambda_mean = lambda_mean, 
    lambda = lambda_th
  )
  
  return(init)
}


#' Draw initial lambda
#' @importFrom stats rnorm 
#' @keywords internal
#' @param n_tenure The length of a tenure of a driver.
#' @param s0 Varianve of the initial condition. Mean of the initial condition is set to 0.
#' @param delta Variace of the innovation.
draw_lambda0 <- function(n_tenure, s0 = 0.1, delta = 0.1) {
  lambda_mean <- rep(NA, n_tenure)
  lambda_mean[1] <- rnorm(1, mean = 0, sd = s0)
  
  if (n_tenure >= 2) {
    for (tt in 2:n_tenure) {
      lambda_mean[tt] <- rnorm(1, mean = lambda_mean[tt-1], sd = delta)
    }
  }
  
  return(lambda_mean)
}

#' Draw initial lambda_type
#' @keywords internal
#' @param lambda_mean A vector of prior means with length corresponding to the length of a tenure.
#' @param sigma Variace of the emission equation.
draw_lambda_th <- function(lambda_mean, sigma) {
  n_rank_types <- ncol(sigma)
  n_tenure     <- length(lambda_mean)
  
  ## time - rank specific ability
  lambda <- matrix(NA, nrow = n_tenure, ncol = n_rank_types)
  
  for (tt in 1:n_tenure) {
    lambda[tt, ] <- MASS::mvrnorm(1, 
      mu = rep(lambda_mean[tt], n_rank_types), Sigma = sigma
    )
  }
    
  return(lambda)
}



#' parameter initialization for dyRank()
#' @keywords internal
#' @importFrom purrr map 
dyRank_initialize_params <- function(data) {
  if (!("dyRank.data" %in% class(data))) stop("Invalid input")
  
  ## initialize λ̅
  lambda_mean <- map(data$driver_attr, 
    ~ matrix(draw_lambda0(.x[1]), ncol = 1))
  
  return(lambda_mean)
}
