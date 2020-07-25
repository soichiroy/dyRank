

##
## import data into packages 
## 

path <- "~/Dropbox/research/projects/formula_one/data_clean"


require(tidyverse)

driver_long <- read_rds(paste(path, "driver_long.rds", sep = "/"))
driver_wide <- read_rds(paste(path, "driver_wide.rds", sep = "/"))


## export 
usethis::use_data(driver_long, overwrite = TRUE)
usethis::use_data(driver_wide, overwrite = TRUE)
