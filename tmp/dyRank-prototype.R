



## input design 
## - list of drivers data <- list()
##    data[[i]] is a matrix with {rank}, {race_id}, {year_id}, {rank_type}
## - driver_attributes is a list of driver's attributes 
##    driver_attr[[i]] is a vector of {n_tenure, min_year, max_year}
## - race_attributes 
##    matrix of {id_race} x {id_race} where each element is the number of finish


for (i in n_driver) {
  dat_use <- data[[i]]
  driver  <- driver_attr[[i]]
  lambda  <- lambda_list[[i]]  ## matrix of {year x rank_type}
  par_use <- params[[i]]
  
  lambda_mean <- lambda_bar[[i]]
  Sigma 
  
  ##
  ## compute moments 
  ## 
  n_obs <- length(dat_use$rank)
  
  for (j in 1:n_obs) {
    ## information about the jth observation 
    obs_rank <- dat_use$rank[j]        ## observed rank 
    id_race  <- dat_use$race_id[j]    ## unique race id 
    id_rank  <- dat_use$rank_type[j]  ## rank type 
    id_year  <- dat_use$year_id[j]    ## normalized id of tenure [1, 2, ...]

    ## get the number of finish in this particular race x ranking 
    n_finish <- race_attr[id_race, id_rank]
    
    for (k in 1:n_finish) {
      delta <- obs_rank >= k 
      Y     <- obs_rank == k
      if (delta == 1) {
        ## update counts 
        c_mk[[id_race]][k, id_rank] <- cmk_use <- c_mk[[id_race]][k, id_rank] -
                                        exp(lambda[id_year, id_rank])
        tmp <- lambda[id_year, id_rank] - log(cmk_use)
        
        ## PG augmentation 
        omega <- pgdraw(1, tmp)
        kappa <- Y - 1/2
        
        Z[id_year] <- Z[id_year] + kappa / omega + log(cmk_use)
        Omega[id_year] <- Omega[id_year] + omega
      }
      
    }
  } ## end of observations 
  
  
  ##
  ## Update λ
  ## 
  ## - input: Σ^{-1}, λ̄ (prior mean and variance)
  ## - input: z and Ω (augmented variables)
  ## - output: λ (T x H) matrix 
  sigma_inv <- solve(Sigma)
  n_tenure  <- driver$n_tenure
  
  for (tt in 1:n_tenure) {
    ## prior mean 
    lambda_prior <- rep(lambda_mean[tt], n_rank_types)
    
    ## variance 
    SS <- solve(diag(Omega[tt, ]) + sigma_inv)
    
    ## mean 
    mm <- SS %*% (Z[tt,] + sigma_inv %*% lambda_prior)
    
    ## sample 
    lambda[tt,] <- mvnfast::rmvn(1, mm, SS)
  } ## end of tenure 
  
  
  ##
  ## update λ̄ (lambda bar)
  ## 
  ## - input: lambda (T x H) matrix 
  ## - input: Σ^-1 (H x H)
  ## - output: lambda_bar (T)
  lambda_mean <- FFBS()
  
  
  ## 
  ## update: Σ
  ##
  ## - input: λ (T x H) -- T observations 
  ## - input: λ̄ (T x 1) -- known prior mean 
  Sigma <- update_sigma(lambda, v0, S0)
  
  
  ##
  ## Update count matrix 
  ##
  ## - input: current c_mk (count)
  ## - input: update lambda (T x H)
  ## - input: other ID's 
  ## - output: updated c_mk 
  c_mk <- update_count()
  
  ## return 
  # c_mk
} ## end of update for driver i 
