

#' Race Results of Formula 1 Grand Prix (1984 - 2019)
#'
#' A dataset containing the final standing of drivers in all GP
#' since 1984 (through 2019). The dataset also contains team affiliation of 
#' drivers. 
#' 
#' @source \url{https://www.formula1.com/}
#' @docType data
#' @keywords data
#' @format A \code{tibble} data frame containing 14,037 observations iwth 8 variables.
#' \describe{
#'   \item{Pos}{The final status of each driver in a particular race. The top driver takes the value of 1.}
#'   \item{driver}{Names of drivers.}
#'   \item{Car}{Names of teams.}
#'   \item{year}{Year of the race.}
#'   \item{race_id}{A unique id of races.}
#'   \item{GP}{Name of the race.}
#'   \item{rank_type}{Types of the ranking. See also datasets "f1_grid" and "f1_laptime".}
#' }
"f1_race"