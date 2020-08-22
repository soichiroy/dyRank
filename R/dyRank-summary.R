

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
	
  
  class(est_all) <- c(class(est_all), "dyRank.summary")
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

  if (!("dyRank.fit" %in% class(obj))) stop("Not a supported input.")	
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

#' Plot rating estimates 
#' @export
#' @param obj An output 
#' @importFrom dplyr %>% filter as_tibble
#' @import ggplot2
#' @importFrom purrr map
plot_rating <- function(obj, 
  facet = FALSE, driver_name = NULL,
  y_label = "", x_label = "", ncol = 3
) {
  
  ## obtain summary estimate 
  if ("dyRank.fit" %in% class(obj)) {
    est <- get_rating(obj)
  } else if ("dyRank.summary" %in% class(obj)) {
    est <- obj 
  } else {
    stop("Not a supported input!")
  }
  
  ## subset if names are provided 
  if (!is.null(driver_name)) {
    est <- as_tibble(est) %>% filter(driver %in% driver_name)
  }
  
  ## generate plot 
  if (isTRUE(facet)) {
    g <- ggplot(est, aes(x = year, y = `50%`)) + 
          geom_ribbon(aes(ymin = `5%`, ymax = `95%`), alpha = 0.3) + 
          geom_line() + 
          geom_point(size = 0.7) + 
          theme_bw() + 
          labs(y = y_label, x = x_label) + 
          facet_wrap(~driver, ncol = ncol)
  } else {
    driver_unique <- unique(est$driver)
    g <- map(driver_unique, ~ est %>% filter(driver == .x) %>% 
         ggplot(est, aes(x = year, y = `50%`)) + 
              geom_ribbon(aes(ymin = `5%`, ymax = `95%`), alpha = 0.3) + 
              geom_line() + 
              geom_point(size = 0.7) + 
              theme_bw() + 
              labs(y = y_label, x = x_label)
        )
    names(g) <- driver_unique
  }
  
  return(g)
}
