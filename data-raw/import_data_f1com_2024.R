library(tidyverse)
dat <- read_rds("data-raw/f1_8424_race_results.rds")

## race 
dat_long <- dat %>% filter(rank_type == "race-result")
dat_list <- vector('list', length = nrow(dat_long))
vars <- dat_long %>% select(-data)
for (i in 1:nrow(dat_long)) {
  dat_list[[i]] <- dat_long$data[[i]] %>% 
    mutate(Pos = as.numeric(Pos)) %>% 
    select(Pos, driver = driver_name, abb_name, Car) %>% 
    mutate(vars[i,])
}
f1_race_8424 <- bind_rows(dat_list) %>%
  rename(GP = race_name) %>%
  mutate(driver = stringr::str_squish(driver))

usethis::use_data(f1_race_8424, overwrite = TRUE)
