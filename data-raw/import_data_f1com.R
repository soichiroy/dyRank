
require(tidyverse)

dat <- read_rds("../data_scrape/f1_8419.rds")


##
## create rankings
##

## race 
dat_long <- dat %>% filter(rank_type == "race-result")
dat_list <- vector('list', length = nrow(dat_long))
vars <- dat_long %>% select(-res)
for (i in 1:nrow(dat_long)) {
  dat_list[[i]] <- dat_long$res[[i]] %>% 
    mutate(Pos = as.numeric(Pos)) %>% 
    select(Pos, first_name, last_name, Car) %>% 
    mutate(vars[i,])
}
f1_race <- bind_rows(dat_list) %>% 
  unite(driver, first_name:last_name, sep = " ")
  
## grid position  
dat_long <- dat %>% filter(rank_type == "starting-grid")
dat_list <- vector('list', length = nrow(dat_long))
vars <- dat_long %>% select(-res)
f1_grid <- map(1:nrow(dat_long), function(i) {
  dat_long$res[[i]] %>% 
    mutate(Pos = as.numeric(Pos)) %>% 
    select(Pos, first_name, last_name, Car) %>% 
    mutate(vars[i,])
}) %>% bind_rows() %>% 
  unite(driver, first_name:last_name, sep = " ")


## fastest lap 
dat_long <- dat %>% filter(rank_type == "fastest-laps")
vars <- dat_long %>% select(-res)
f1_laptime <- map(1:nrow(dat_long), function(i) {
  dat_long$res[[i]] %>% 
    # mutate(Pos = as.numeric(Pos)) %>% 
    select(Pos, first_name, last_name, Car) %>% 
    mutate(vars[i,])
}) %>% bind_rows() %>% 
  unite(driver, first_name:last_name, sep = " ")



##
## save data
##
usethis::use_data(f1_race, overwrite = TRUE)
usethis::use_data(f1_grid, overwrite = TRUE)
usethis::use_data(f1_laptime, overwrite = TRUE)
