#' Hierarchical Dynamic Rating
#' @export
#' @importFrom dplyr %>% filter pull as_tibble mutate
#' @importFrom furrr future_map future_map_dfr future_options
#' @importFrom future plan multiprocess
#' @importFrom coda as.mcmc as.mcmc.list 
#' @importFrom purrr map 
#' @param data A data frame of \code{tibble} class. 
#' @param var_rank A variable name of the outcome (ranking).
#' @param var_player A variable name of the players.
#' @param var_match A variable name of matches.
#' @param var_time A variable name of time index. 
#' @param var_rank_type A variable name of rank types.
#' @param driver_fix A name of the player used as a refenrece. 
#' @param mcmc MCMC iterations.
#' @param burnin Burnin periods. 
#' @param thin Thinning. 
#' @param n_chains The number of MCMC chains. 
hdyRank <- function(
 data, var_rank, var_player, var_match, var_time, var_rank_type,
  driver_fix, mcmc = 100, burnin = 10, thin = 1, n_chains = 3
) {
  
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
    filter(drivers == driver_fix) %>% 
    pull(id_driver) %>% unique()
  id_driver_fix <- id_driver_fix - 1 ## cpp code is zero indexed 
  
  ##
  ## run n_chains 
  ##
  plan(multiprocess)
  
  fit_chain <- future_map(1:n_chains, function(chain) {
    ## 
    ## initialize and prepare inputs 
    ## 
    params <- initialize_params(dd)
    c_mk <- initialize_counts(
      dat         = dd$dat_driver, 
      lambda      = params$lambda, 
      race_attr   = dd$race_attr,
      driver_attr = dd$driver_attr,
      n_race      = dd$n_race
    )
    
    ## format 
    c_mk <- map(c_mk, ~matrix(.x, ncol = dd$n_rank_types))
    
    ## 
    ## MCMC 
    ## 
    fit <- hdyRank_cpp(
      dat         = dd$dat_driver,
      race_attr   = dd$race_attr,
      driver_attr = dd$driver_attr,
      lambda      = params$lambda,
      lambda_mean = params$lambda_mean,
      sigma       = params$sigma, 
      c_mk        = c_mk,
      mcmc = mcmc, burnin = burnin, thin = thin, 
      id_driver_fix = id_driver_fix
    )
    
    return(fit)
  }, .options = future_options(seed = TRUE))
  
  ## summarise the key parameters 
  est_all <- future_map_dfr(1:dd$n_driver, function(i) {
    mm_list <- map(fit_chain, 
      ~map(.x$lambda_mean, ~.x[[i]]) %>% do.call(rbind, .) %>% as.mcmc()) %>%
      as.mcmc.list()
    mm <- do.call(rbind, mm_list)
    
    driver <- dd$dat_ref %>% filter(id_driver == i) %>% 
                pull(drivers) %>% unique()
    years  <- dd$dat_ref %>% filter(id_driver == i) %>% 
                pull(years) %>% unique() %>% 
                as.character() %>% as.numeric()
    
    years_vec <- min(years):max(years)
          
    est <- as_tibble(
      t(apply(mm, 2, quantile, prob = c(0.025, 0.05, 0.5, 0.95, 0.975)))
    ) %>% 
    mutate(driver = driver) %>% 
    mutate(year = years_vec)
    
    return(est)
  })
  
  
  ## return values 
  out <- list(rating = est_all, data = dd, 
              fit = fit_chain, reference = id_driver_fix)
  class(out) <- c(class(out), "hdyRank")
  return(out)
}