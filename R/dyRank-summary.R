

#' Obtain summary statistics 
#' @export
#' @param obj A fitted object from \code{dyRank()}.
#' @importFrom dplyr %>% filter pull as_tibble mutate 
#' @importFrom future plan multiprocess 
#' @importFrom purrr map 
#' @importFrom furrr future_map_dfr 
#' @importFrom stats quantile
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
#' @importFrom purrr map_chr map
#' @importFrom rlang .data
#' @param obj A \code{dyRank.fit} object, which is an output of \code{\link{dyRank}} or \code{\link{hdyRank}}.
#' @return A list of mcmc objects that are compatible with functions from \code{coda} package.
#' @export
get_mcmc <- function(obj) {

  if (!("dyRank.fit" %in% class(obj))) stop("Not a supported input.")	
	
	# get mcmc object 
	est_mcmc <- map(1:obj$data$n_driver, function(i) {
	  mat <- do.call(rbind, map(obj$lambda, ~.x[[i]]))  
	  
	  ## convert to the mcmc object 
	  mat_mcmc <- as.mcmc(mat)
	  return(mat_mcmc)
	})
	
	## drivers 
	drivers <- map_chr(1:obj$data$n_driver, function(i) {
	  ## get meta information 
	  driver <- obj$data$dat_ref %>% filter(.data$id_driver == i) %>% 
	              pull(.data$drivers) %>% unique()
	  return(driver)
	})
	
	names(est_mcmc) <- drivers
	
	return(est_mcmc)
}

#' Plot rating estimates 
#' @export
#' @param obj An output 
#' @param facet A boolean argument. If set \code{TRUE}, \code{facet_wrap} is added to the plot.
#' If \code{FALSE}, the function returns a list of individual plots.
#' @param driver_name A vector of player names. If left \code{NULL}, all players are used to generate a plot.
#' @param y_label A character for the y axis.
#' @param x_label A character for the x axis.
#' @param ncol The number of columns in the facet. This argument is ignored when \code{facet = FALSE}.
#' @importFrom dplyr %>% filter as_tibble
#' @importFrom rlang .data
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
    est <- as_tibble(est) %>% filter(.data$driver %in% driver_name)
  }
  
  ## generate plot 
  if (isTRUE(facet)) {
    g <- ggplot(est, aes(x = .data$year, y = .data$`50%`)) + 
          geom_ribbon(aes(ymin = .data$`5%`, ymax = .data$`95%`), alpha = 0.3) + 
          geom_line() + 
          geom_point(size = 0.7) + 
          theme_bw() + 
          labs(y = y_label, x = x_label) + 
          facet_wrap(~.data$driver, ncol = ncol)
  } else {
    driver_unique <- unique(est$driver)
    g <- map(driver_unique, ~ est %>% filter(.data$driver == .x) %>% 
         ggplot(est, aes(x = .data$year, y = .data$`50%`)) + 
              geom_ribbon(aes(ymin = .data$`5%`, ymax = .data$`95%`), alpha = 0.3) + 
              geom_line() + 
              geom_point(size = 0.7) + 
              theme_bw() + 
              labs(y = y_label, x = x_label)
        )
    names(g) <- driver_unique
  }
  
  return(g)
}


#' Bind Multiple Chains 
#' @export
#' @importFrom coda mcmc.list
#' @importFrom purrr map map_dfr
#' @importFrom dplyr %>% filter pull as_tibble mutate select everything
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @param obj A list of \code{dyRank.fit} objects, which are typically outputs from \code{\link{dyRank}} or 
#' \code{\link{hdyRank}}.
#' @param summarize A boolean argument. If \code{TRUE}, the function returns the summary of the posterior.
#' @return When \code{summarize = FALSE}, it returns a list of \code{mcmc.list} objects. 
#' When \code{summarize = TRUE}, it returns a \code{dyRank.summary} object.
bind_chains <- function(obj, summarize = FALSE) {

	if (length(obj) == 1) stop("Use get_mcmc() instead.")
	
	
	## transform estimates to a list of mcmc obj 
	obj_mcmc <- map(obj, ~get_mcmc(.x))
	n_drivers <- length(obj_mcmc[[1]])

	## get mcmc list for each driver 
	out_mcmc_list <- map(1:n_drivers, function(i) {
		mcmc.list(map(obj_mcmc, ~.x[[i]]))
	}) 
	
	
	## summarize estimates a la get_rating()
	if (isTRUE(summarize)) {
		# summary estimate 
		est_all <- map_dfr(1:n_drivers, function(i) {
		  mat <- do.call(rbind, out_mcmc_list[[i]])  
		  mat_summary <- apply(mat, 2, quantile, prob = c(0.025, 0.05, 0.5, 0.95, 0.975))
	
		  driver <- obj[[1]]$data$dat_ref %>% filter(.data$id_driver == i) %>% 
		              pull(.data$drivers) %>% unique()
		  years  <- obj[[1]]$data$dat_ref %>% filter(.data$id_driver == i) %>% 
		              pull(.data$years) %>% unique() %>% 
		              as.character() %>% as.numeric()
		  
		  years_vec <- min(years):max(years)  
		  est <- as_tibble(t(mat_summary)) %>% 
		    mutate(driver = driver) %>% 
		    mutate(year = years_vec) %>% 
		  	select(.data$driver, .data$year, everything())
		  return(est)
		})
    
    ## class 
    class(est_all) <- c(class(est_all), "dyRank.summary")  
	} else {
		est_all <- out_mcmc_list
	}
	
	return(est_all)
}
