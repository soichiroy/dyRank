% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dyRank-data_documentation.R
\docType{data}
\name{f1_laptime}
\alias{f1_laptime}
\title{Lap-time Raking of Formula 1 Grand Prix (1984 - 2019)}
\format{
A \code{tibble} data frame containing 14,037 observations iwth 8 variables.
\describe{
  \item{Pos}{The ranking of each driver based on the best lap time in a particular race. The top driver takes the value of 1.}
  \item{driver}{Names of drivers.}
  \item{Car}{Names of teams.}
  \item{year}{Year of the race.}
  \item{race_id}{A unique id of races.}
  \item{GP}{Name of the race.}
  \item{rank_type}{Types of the ranking. See also datasets "f1_grid" and "f1_race".}
}
}
\source{
\url{https://www.formula1.com/}
}
\usage{
f1_laptime
}
\description{
A dataset containing the ranking based on the best lap time of drivers in all GP
since 1984 (through 2019). The dataset also contains team affiliation of
drivers.
}
\seealso{
\code{\link{f1_grid}} and \code{\link{f1_race}}.
}
\keyword{data}
