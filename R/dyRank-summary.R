

#' Obtain summary statistics 
#' @export
#' @param obj A fitted object from \code{dyRank()}.
#' @importFrom dplyr %>% filter pull as_tibble mutate 
#' @importFrom future plan multiprocess 
#' @importFrom purrr map 
#' @importFrom furrr future_map_dfr 
get_rating <- function(obj) {
	if (!("dyRank.fit" %in% class(obj))) stop("Not a supported input.")
	
	# register the parallel computing 
	# plan(multiprocess)
	
	# summary estimate 
	est_all <- future_map_dfr(1:obj$data$n_driver, function(i) {
	  mat <- do.call(rbind, map(obj$lambda, ~.x[[i]]))  
	  mat_summary <- apply(mat, 2, quantile, prob = c(0.025, 0.05, 0.5, 0.95, 0.975))

	  driver <- obj$data$dat_ref %>% filter(id_driver == i) %>% 
	              pull(drivers) %>% unique()
	  years  <- obj$data$dat_ref %>% filter(id_driver == i) %>% 
	              pull(years) %>% unique() %>% 
	              as.character() %>% as.numeric()
	  
	  years_vec <- min(years):max(years)  
	  est <- as_tibble(t(mat_summary)) %>% 
	    mutate(driver = driver) %>% 
	    mutate(year = years_vec) %>% 
	  	select(driver, year, everything())
	  return(est)
	})
	
	return(est_all)	
}


#' Obtain MCMC object of rating parameters
#' @importFrom coda as.mcmc 
#' @importFrom rlang set_names
#' @importFrom purrr map_chr
#' @importFrom furrr future_map
#' @importFrom future plan multiprocess
#' @export
get_mcmc <- function(obj) {
	
	# register the parallel computing 
	# plan(multiprocess)

	# get mcmc object 
	est_mcmc <- future_map(1:obj$data$n_driver, function(i) {
	  mat <- do.call(rbind, map(obj$lambda, ~.x[[i]]))  
	  
	  ## convert to the mcmc object 
	  mat_mcmc <- as.mcmc(mat)
	  return(mat_mcmc)
	})
	
	## drivers 
	drivers <- map_chr(1:obj$data$n_driver, function(i) {
	  ## get meta information 
	  driver <- obj$data$dat_ref %>% filter(id_driver == i) %>% 
	              pull(drivers) %>% unique()
	  return(driver)
	})
	
	names(est_mcmc) <- drivers
	
	return(est_mcmc)
}