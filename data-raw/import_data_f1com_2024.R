library(tidyverse)

# Load data
dat_race <- read_rds("data-raw/f1_8424_race_results.rds")
dat_grid <- read_rds("data-raw/f1_8424_starting_grid.rds")
dat_lap <- read_rds("data-raw/f1_8424_fastest_laps.rds")

# Function to process data
ProcessData <- function(dat) {
  dat_list <- vector('list', length = nrow(dat))
  vars <- dat %>% select(-data)
  for (i in 1:nrow(dat)) {
    dat_list[[i]] <- dat$data[[i]] %>% 
      mutate(Pos = as.numeric(Pos)) %>% 
      select(Pos, driver = driver_name, abb_name, Car) %>% 
      mutate(vars[i,])
  }
  out <- bind_rows(dat_list) %>%
    rename(GP = race_name) %>%
    mutate(driver = stringr::str_squish(driver))
  return(out)
}

## race 
f1_race_8424 <- ProcessData(dat_race)
f1_laptime_8424 <- ProcessData(dat_lap)
f1_grid_8424 <- ProcessData(dat_grid)

usethis::use_data(f1_race_8424, overwrite = TRUE)
usethis::use_data(f1_laptime_8424, overwrite = TRUE)
usethis::use_data(f1_grid_8424, overwrite = TRUE)
