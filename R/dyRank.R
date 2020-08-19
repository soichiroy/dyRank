




#' Dynamic Ranking Model
#' @export
#' @importFrom dplyr %>% filter pull mutate
#' @importFrom furrr future_map future_map_dfr future_options
#' @importFrom future plan multiprocess
#' @importFrom coda as.mcmc as.mcmc.list 
#' @importFrom purrr map 
#' @param data A data frame of \code{tibble} class. 
#' @param var_rank A variable name of the outcome (ranking).
#' @param var_player A variable name of the players.
#' @param var_match A variable name of matches.
#' @param var_time A variable name of time index. 
#' @param driver_fix A name of the player used as a refenrece. 
#' @param mcmc MCMC iterations.
#' @param burnin Burnin periods. 
#' @param thin Thinning. 
#' @param truncation A truncation parameter.
dyRank <- function(
  data, var_rank, var_player, var_match, var_time, 
  driver_fix, mcmc = 100, burnin = 10, thin = 1,
  truncation = 3
) {
  
  if (truncation < 1) truncation <- 1
  
  ##
  ## create data 
  ##
  var_rank_type <- ".rank_type"
  data$.rank_type <- 1
  dd <- dyRank_data(data = data, 
    var_rank = var_rank, var_player = var_player, 
    var_match = var_match, var_time = var_time, 
    var_rank_type = var_rank_type
  )


  ##
  ## driver_fix 
  ##
  id_driver_fix <-  dd$dat_ref %>% 
    filter(drivers == driver_fix) %>% 
    pull(id_driver) %>% unique()
  id_driver_fix <- id_driver_fix - 1 ## cpp code is zero indexed 

  ## initialize parameters 
  params <- dyRank_initialize_params(dd)
  c_mk <- initialize_counts(
    dat         = dd$dat_driver, 
    lambda      = params, 
    race_attr   = dd$race_attr,
    driver_attr = dd$driver_attr,
    n_race      = dd$n_race,
    trunc       = truncation
  )

  c_mk <- map(c_mk, ~matrix(.x, ncol = dd$n_rank_types))

  ##
  ## MCMC 
  ## 
  fit <- dyRank_cpp(
    dat         = dd$dat_driver,
    race_attr   = dd$race_attr,
    driver_attr = dd$driver_attr,
    lambda      = params,
    c_mk        = c_mk,
    mcmc = mcmc, burnin = burnin, thin = thin, 
    id_driver_fix = id_driver_fix,
    trunc         = truncation
  )
  
  
  ##
  ## output 
  ## 
  out <- list(lambda = fit[['lambda']], data = dd)
  class(out) <- c(class(out), "dyRank.fit")
  return(out)
}

#' Hierarchical Dynamic Rating (Single variance)
dyRank_deprecate <- function(
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
