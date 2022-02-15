rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(readr)
library(aopdata) # devtools::install_github("ipeaGIT/aopdata", subdir = "r-package")

# read

walk_all <- aopdata::read_access(city = 'all', mode = 'walk', peak = TRUE, year = 2019, geometry = TRUE)

cities_code <- c("for", "rec", "bho","rio", "spo", "cur", "poa")
ubus_all <- lapply(cities_code, function(i){
  tmp_dt <- aopdata::read_access(city = i, mode = 'public_transport', peak = TRUE, year = 2019, geometry = TRUE)
  tmp_dt <- setDT(tmp_dt)
}) %>% data.table::rbindlist()

bici_all <- aopdata::read_access(city = 'all', mode = 'bicycle', peak = TRUE, year = 2019, geometry = TRUE)

# add data
setDT(walk_all)[,mode := "walk"]
setDT(ubus_all)[,mode := "public_transport"]
setDT(bici_all)[,mode := "bicycle"]


# rbind all
unique_dt <- rbind(walk_all,ubus_all)
unique_dt <- rbind(unique_dt,bici_all)

# save
readr::write_rds(unique_dt,"data/socio_acc-all.rds",compress = "gz")