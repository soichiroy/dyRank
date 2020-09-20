




#' Dynamic Ranking Model
#' @export
#' @importFrom dplyr %>% filter pull mutate
#' @importFrom furrr future_map future_map_dfr future_options
#' @importFrom future plan multiprocess
#' @importFrom coda as.mcmc as.mcmc.list
#' @importFrom purrr map
#' @importFrom rlang .data
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
#' @return \code{dyRank()} returns an object of \code{dyRank.fit} class, which is a list
#' of the following elements:
#' \describe{
#'    \item{\code{lambda}}{A list of estimated rating parameters.
#'          Rating estimates can be accessed with \code{get_rating()} or
#'          \code{get_mcmc()} function.}
#'    \item{\code{data}}{A transformed dataset used for estimation.}
#' }
#' @examples
#' set.seed(1234)
#' # load data
#' data("f1_race", package = "dyRank")
#'
#' ## estimate via MCMC
#' fit <- dyRank(
#'     data       = f1_race,
#'     var_rank   = "Pos",
#'     var_player = "driver",
#'     var_match  = "GP",
#'     var_time   = "year",
#'     driver_fix = "Timo Glock",
#'     mcmc = 10, burnin = 10, thin = 1,
#'     truncation = 3
#' )
#'
#' ## obtain estimated ratings
#' rating <- get_rating(fit)
#' @seealso \code{\link{get_rating}}, \code{\link{get_mcmc}}
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
    filter(.data$drivers == driver_fix) %>%
    pull(.data$id_driver) %>% unique()
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
    lambda      = params,
    c_mk        = c_mk,
    dat         = dd$dat_driver,
    race_attr   = dd$race_attr,
    driver_attr = dd$driver_attr,
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
