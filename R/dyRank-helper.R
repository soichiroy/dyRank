
## -------------------------------------- ##
## Functions for initialization
## -------------------------------------- ##

## initialize lambda 
init_lambda <- function(year_GP) {
  n_driver <- length(year_GP)
  lambda   <- vector("list", length = n_driver)
  for (i in 1:n_driver) {
    tenure <- length(year_GP[[i]])
    # cat("i = ", i, ": tenure = ", tenure, "\n")
    lambda[[i]] <- runif(tenure, -0.2, 0.2)
  }
  return(lambda)
}

## initialize c_mk 
create_cmk <- function(dat_GP, time_GP, lambda, delta, n_GP, n_fs) {
  n_driver <- length(dat_GP)
  c_mk <- vector('list', length = n_GP)
  for (j in 1:n_GP) {
    c_mk[[j]] <- rep(0, n_fs[[j]])
  }
  
  ## fill c_mk 
  for (i in 1:n_driver) {
    for (j in 1:length(dat_GP[[i]])) {
      GP_idx <- dat_GP[[i]][[j]]
      delta_i <- delta[[i]][[j]]
      time_i  <- time_GP[[i]][[j]]
      c_mk[[GP_idx]][delta_i == 1] <- c_mk[[GP_idx]][delta_i == 1] + exp(lambda[[i]][time_i])
    }
  }
  
  return(c_mk)
}




## -------------------------------------- ##
## Functions for updating statistics 
## -------------------------------------- ##
update_counts <- function(c_mk, dat_GP, time_GP, delta, lambda, tr) {
  for (j in 1:length(dat_GP)) {
    index_GP <- dat_GP[[j]]
    index_tenure <- time_GP[[j]]
    n_finish <- length(c_mk[[index_GP]])
    truncation <- tr
    for (k in 1:(n_finish - truncation)) {
      if (delta[[j]][k] == 1) {
        c_mk[[index_GP]][k] <- c_mk[[index_GP]][k] + exp(lambda[index_tenure])
      }
    }
  }
  
  return(c_mk)
  
}
