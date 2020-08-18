## sample ranking specific ability parameter 



sample_lambda_local <- function(Z, Omega, lambda_mean, tau = 0.25, is_fix) {
  n_time <- length(Z)
  lambda_vec <- rep(NA, n_time)
  ## update moments 
  for (tt in 1:n_time) {
    var_tt <- 1 / (sum(1 / Omega[[tt]]) + (1 / tau))
    mu_tt  <- var_tt * (lambda_mean[tt] / tau + sum(Z[[tt]] / Omega[[tt]]))
    lambda_vec[tt] <- rnorm(1, mu_tt, sqrt(var_tt))
  }
  
  if (isTRUE(is_fix)) lambda_vec[1] <- 0
  return(lambda_vec)
}


#' Sample Ability Parameters 
#' @keywords internal
sample_lambda <- function(
  dat_GP, time_GP, delta, Y, lambda_mean, lambda, tau, c_mk, n_tenure,
  driver_fix, tr, a0 = 1.5, b0 = 1
) {
  n_rank_types <- length(dat_GP)
  n_player <- length(dat_GP[[1]])
  for (i in 1:n_player) {
    # cat("driver = ", i, "\n")
    is_fix <- FALSE
    if (driver_fix == i) is_fix <- TRUE
    ## update moments 
    lambda_tmp <- matrix(NA, nrow = n_rank_types, ncol = n_tenure[[i]])
    
    
    for (h in 1:n_rank_types) {  
      ## update moments 
      moments <- update_moments(
        dat_GP[[h]][[i]], time_GP[[h]][[i]], delta[[h]][[i]],  
        Y[[h]][[i]], lambda[[h]][[i]], c_mk[[h]], 
        n_tenure[[i]], tr = tr[h]
      )
      
      ## update lambda for each h 
      lambda[[h]][[i]] <- sample_lambda_local(
        moments$Z, moments$Omega, lambda_mean[[i]], tau[i], is_fix
      )
      
      lambda_tmp[h,] <- lambda[[h]][[i]]
      
      ## update counts 
      c_mk[[h]] <- update_counts_cpp(moments$c_mk, 
        dat_GP[[h]][[i]], time_GP[[h]][[i]], delta[[h]][[i]], 
        lambda[[h]][[i]], tr = tr[h]
      )
    }
    
    ## updata tau 
    # demaen 
    lambda_dm <- sapply(1:n_rank_types, function(h) {
      lambda_tmp[h,] - lambda_mean[[i]]
    })
    tau[i] <- MCMCpack::rinvgamma(1, a0 + prod(dim(lambda_dm)) / 2, 
                                   b0 + sum(lambda_dm^2) / 2)
  
    ## update global mean 
    lambda_mean[[i]] <- FFBScpp(lambda_tmp, tau[i])
    if (isTRUE(is_fix)) lambda_mean[[i]][1] <- 0
  }
  
  return(list(lambda = lambda, lambda_mean = lambda_mean, tau = tau, c_mk = c_mk))
}



#' Data augmentation 
#' @keywords internal 
#' @importFrom pgdraw pgdraw
update_moments <- function(dat_GP, time_GP, delta, Y, lambda, c_mk, nt, tr = 1) {
  n_tenure <- nt
  Z <- vector("list", length = n_tenure)
  Omega <- vector('list', length = n_tenure)
  n_GP  <- length(dat_GP)
  
  ## augmentation step 
  for (j in 1:n_GP) {
    ## obtain which GP  and what time 
    index_GP <- dat_GP[[j]]
    index_tenure <- time_GP[[j]] ## this is the normalized time
    
    ## number of finished cars 
    n_finish <- length(c_mk[[index_GP]])
    truncation <- tr
    
    ## loop over all ranks 
    for (k in 1:(n_finish-truncation)) {
      ## use info if this driver is ranked k or below 
      if (delta[[j]][k] == 1) {
        ## update count 
        c_mk_save <- c_mk[[index_GP]][k]
        c_mk[[index_GP]][k]  <- c_mk[[index_GP]][k] - exp(lambda[index_tenure])
        tmp <- lambda[index_tenure] - log(c_mk[[index_GP]][k])
        if (!is.finite(tmp)) { 
          cat("c_mk = ", c_mk_save, "\n")
          cat("lambda[index_tenure] = ", lambda[index_tenure], "\n")
          cat("updated c_mk = ", c_mk[[index_GP]][k], "\n")
          cat("index_GP = ", index_GP, ": k = ", k, "\n")
          break; 
        }
        
        omega <- pgdraw(1, tmp)
        kappa <- Y[[j]][k] - 1/2
        
        Z[[index_tenure]] <- c(Z[[index_tenure]], 
                                kappa/omega + log(c_mk[[index_GP]][k])
                              )
        Omega[[index_tenure]] <- c(Omega[[index_tenure]], 1/omega)
      }
    }
  }

  return(list(Z = Z, Omega = Omega, c_mk = c_mk))
}



# update_lambda_mult <- function(moments, Sigma) {
# 
#   SigmaInv <- solve(Sigma)
# 
#   c_mk <- vector('list', length = n_rank_types)
#   for (h in 1:n_rank_types) {  
#     ## update moments 
#     moments <- update_moments(
#       dat_GP[[h]][[i]], time_GP[[h]][[i]], delta[[h]][[i]],  
#       Y[[h]][[i]], lambda[[h]][[i]], c_mk[[h]], 
#       n_tenure[[i]], tr = tr[h]
#     )
# 
#     Z[h,] <- sapply(moments$Z, sum)
#     Omega[h, ] <- sapply(moments$Omega, function(x) sum(1/x))
#     c_mk[[h]] <- moments$c_mk 
#   }
# 
#   for (tt in 1:n_tenure) {
#     SS <- solve(diag(Omega[,tt]) + SigmaInv)
#     mm <- SS %*% (Z[,tt] + SigmaInv %*% rep(lambda[[tt]], n_rank_types))
#     mvnfast::rmvn(1, mu = mm, sigma = SS)
#   }
# 
# }
