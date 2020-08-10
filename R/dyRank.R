
#' Hierarchical Dynamic Rating
#' @export 
dyRank <- function(
  dat_GP, 
  time_GP, 
  year_GP,
  delta, 
  Y, 
  player_fix, 
  n_fs, tr,
  mcmc, burn, thin,
  verboser = TRUE) {

  n_rank_types <- length(dat_GP)
  n_driver <- length(dat_GP[[1]])

  ## initialization 
  lambda <- c_mk <- vector("list", length = n_rank_types)
  for (h in 1:n_rank_types) {
    lambda[[h]] <- init_lambda(year_GP[[h]])
    lambda[[h]][[player_fix]][1] <- 0

    ## count exp(lambda)
    c_mk[[h]]  <- create_cmk(dat_GP[[h]], time_GP[[h]], lambda[[h]], 
      delta[[h]], length(n_fs[[h]]), n_fs[[h]])    
  }

  n_tenure <- map(lambda[[1]], ~length(.x))
  
  ## initial sample 
  lambda_mean <- lambda[[1]]
  tau <- rep(0.5, n_driver)
  est <- sample_lambda(dat_GP, time_GP, delta, Y, 
    lambda_mean, lambda, tau, c_mk, n_tenure, player_fix, tr)

  ## ========================================== ##
  ## MCMC 
  ## ========================================== ##
  max_iter <- mcmc + burn
  nstore <- mcmc / thin
  save_list <- vector("list", length = nstore)

  for (iter in 1:max_iter) {
    cat('iter = ', iter, '\n')
    est <- sample_lambda(
      dat_GP     = dat_GP, 
      time_GP    = time_GP, 
      delta      = delta, 
      Y          = Y, 
      lambda_mea = est$lambda_mean, 
      lambda     = est$lambda, 
      tau        = est$tau, 
      c_mk       = est$c_mk, 
      n_tenure   = n_tenure,
      driver_fix = player_fix,
      tr         = tr 
    )
      
    if (iter > burn && (iter %% thin == 0)) {
      save_list[[(iter-burn)/thin]] <- est
    }
  }
  
  return(save_list)
}
