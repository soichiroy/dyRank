

## initialize inputs 
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
