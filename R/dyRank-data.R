


#' Data preparation for dynamic rating estimation 
#' @keywords internal
#' @inheritParams dyRank
#' @importFrom dplyr %>% pull tibble mutate mutate_all select arrange summarise group_by
#' @importFrom tidyr pivot_wider drop_na nest unite
#' @importFrom purrr map 
#' @importFrom rlang sym !! .data
dyRank_data <- function(
  data, var_rank, var_player, var_match, var_time, var_rank_type = NULL
) {
  
  ## input check 
  if (is.na(var_rank_type)) data$var_rank_type <- 1
  
  ## drop NA
  data <- drop_na(data, 
    !!sym(var_rank), !!sym(var_player), 
    !!sym(var_match), !!sym(var_time),
    !!sym(var_rank_type)
  )

  ## obtain data 
  outcome <- pull(data, !!sym(var_rank))  
  if (isFALSE(is.numeric(outcome))) stop("var_rank should be numeric (integer)")
  
  drivers   <- pull(data, !!sym(var_player))
  races     <- pull(data, !!sym(var_match))
  years     <- pull(data, !!sym(var_time))
  rank_type <- pull(data, !!sym(var_rank_type))
  
  ## convert information into numerical index 
  id_time   <- as.numeric(as.factor(as.character(years)))
  id_race   <- as.numeric(as.factor(as.character(races)))
  id_driver <- as.numeric(as.factor(as.character(drivers)))
  id_rank   <- as.numeric(as.factor(as.character(rank_type)))
  
  
  ## data for reference
  dat_ref <- tibble(years = years, races = races, 
    drivers = drivers, rank_type = rank_type, 
    id_time = id_time, id_driver = id_driver)

  ## index data to process 
  one_minus <- function(x) x - 1
  
  dat_tmp <- tibble(
    outcome   = outcome, 
    id_driver = id_driver,
    id_time   = as.character(id_time),
    id_race   = as.character(id_race),
    id_rank   = id_rank
  ) %>% 
  unite("id_match", .data$id_time, .data$id_race, remove = FALSE) %>% 
  mutate(id_match = as.numeric(as.factor(.data$id_match)), 
         id_time = as.numeric(.data$id_time)) %>% 
  select(-id_race) %>% 
  arrange(.data$id_driver) %>% 
  mutate_all(one_minus)
  
  ## get race attributes 
  ## matrix: id_match x id_rank (element: max_rank)
  race_attr <- group_by(dat_tmp, .data$id_rank, .data$id_match) %>% 
    summarise(max_rank = max(.data$outcome)) %>% 
    pivot_wider(id_cols = .data$id_match, names_from = .data$id_rank, values_from = .data$max_rank)
  
  ## process by drivers 
  dat_nest <- group_by(dat_tmp, .data$id_driver) %>% nest()
  dat_driver <- dat_nest$data
  
  
  ## compute the tenure of each driver 
  driver_attr <- map(dat_driver,
    ~ c(n_tenure = max(.x$id_time) - min(.x$id_time) + 1,
        min_year = min(.x$id_time), max_year = max(.x$id_time))
  )
  
  
  dat_return <- list(
    dat_driver   = lapply(dat_driver, data.matrix),
    race_attr    = data.matrix(select(race_attr, -id_match)),
    driver_attr  = driver_attr,
    dat_ref      = dat_ref,
    n_rank_types = length(unique(id_rank)),
    n_drivers    = length(unique(id_driver)),
    n_race       = length(unique(dat_tmp$id_match)),
    n_time       = length(unique(id_time))
  )
  
  class(dat_return) <- c(class(dat_return), "dyRank.data")
  return(dat_return)
}
