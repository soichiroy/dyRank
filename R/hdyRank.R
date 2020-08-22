#' Hierarchical Dynamic Rating Estimation for Ranked Data
#' 
#' \code{hdyRank()} implements the hierarhical dynamic Plackett-Luce model
#'  for estimating the rating based on rank-ordered data. 
#'  Different from \code{dyRank()}, \code{hdyRank()} can handle multiple types of ranking data.
#'  
#' @export
#' @importFrom dplyr %>% filter pull as_tibble mutate
#' @importFrom furrr future_map future_map_dfr future_options
#' @importFrom future plan multiprocess
#' @importFrom coda as.mcmc as.mcmc.list 
#' @importFrom purrr map 
#' @importFrom rlang .data
#' @inheritParams dyRank
#' @param var_rank_type A variable name of rank types.
hdyRank <- function(
 data, var_rank, var_player, var_match, var_time, var_rank_type,
  driver_fix, mcmc = 100, burnin = 10, thin = 1, 
  truncation = 3
) {
  
  
  if (truncation < 1) truncation <- 1
  
  
  ##
  ## create data 
  ##
  dd <- dyRank_data(data = data, 
    var_rank = var_rank, var_player = var_player, 
    var_match = var_match, var_time = var_time, 
    var_rank_type = var_rank_type
  )
  
  ##
  ## driver_fix 
  ##
  id_driver_fix <-  dd$dat_ref %>% 
    filter(.data$drivers == driver_fix) %>% 
    pull(.data$id_driver) %>% unique()
  id_driver_fix <- id_driver_fix - 1 ## cpp code is zero indexed 

  ##
  ## initialize and prepare inputs 
  ## 
  params <- initialize_params(dd)
  c_mk <- initialize_counts(
    dat         = dd$dat_driver, 
    lambda      = params$lambda, 
    race_attr   = dd$race_attr,
    driver_attr = dd$driver_attr,
    n_race      = dd$n_race,
    trunc       = truncation
  )
  
  ## format 
  c_mk <- map(c_mk, ~matrix(.x, ncol = dd$n_rank_types))
  
  ## 
  ## MCMC 
  ## 
  fit <- hdyRank_cpp(
    lambda      = params$lambda,
    lambda_mean = params$lambda_mean,
    sigma       = params$sigma, 
    c_mk        = c_mk,
    dat         = dd$dat_driver,
    race_attr   = dd$race_attr,
    driver_attr = dd$driver_attr,
    mcmc = mcmc, burnin = burnin, thin = thin, 
    id_driver_fix = id_driver_fix,
    trunc         = truncation
  )  
  
  ## save mcmc setting 
  param_mcmc <- list(mcmc = mcmc, burnin = burnin, thin = thin)
  
  ## return values 
  out <- list(
    lambda = fit[['lambda_mean']],
    sigma  = fit[['sigma']],
    lambda_type = fit[['lambda']],
    data = dd, 
    param_mcmc = param_mcmc
  )
  class(out) <- c(class(out), "dyRank.fit")
  return(out)
}
