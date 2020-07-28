

##
## import data into packages 
## 

path <- "~/Dropbox/research/projects/formula_one/data_clean"

## package
require(tidyverse)

## load data 
f1_driver_long <- read_rds(paste(path, "driver_long.rds", sep = "/"))
f1_driver <- read_rds(paste(path, "driver_wide.rds", sep = "/"))


## export 
usethis::use_data(f1_driver_long, overwrite = TRUE)
usethis::use_data(f1_driver, overwrite = TRUE)
