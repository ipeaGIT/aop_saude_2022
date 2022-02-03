

# load packages ------

rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
library(sf)
library(patchwork)
library(mapview)
library(aopdata) # devtools::install_github("ipeaGIT/aopdata", subdir = "r-package")


# read files ------
filepath_acess <- "L:\\Proj_acess_oport\\data\\acesso_oport\\output_access"

files_acess <- list.files(path = filepath_acess#paste0(filepath_acess,"\\2017")
                          ,recursive = TRUE
                          ,full.names = TRUE)
files_acess <- files_acess[!(files_acess %like% "_old")]
files_acess <- files_acess[files_acess %like% "_all"]

files_acess_car <- list.files(path = filepath_acess#paste0(filepath_acess,"\\2017")
                              ,pattern = "car"
                              ,recursive = TRUE
                              ,full.names = TRUE)

filepath_socio <- "L:\\Proj_acess_oport\\data\\acesso_oport\\hex_agregados\\"
files_socio <- list.files(path = filepath_socio#paste0(filepath_socio,"\\2017")
                          ,recursive = TRUE
                          ,pattern = "09"
                          ,full.names = TRUE)
files_socio <- files_socio[!(files_socio %like% "_old")]

# 

tmp_acc <- lapply(files_acess,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)
tmp_soc <- lapply(files_socio,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)
tmp_car <- lapply(files_acess_car,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)

# joint dataset ----
# add socio into car
tmp_join_car <- data.table::setDT(tmp_car)[
  data.table::setDT(tmp_soc), on = c("origin"="id_hex"
                                     ,"city" = "sigla_muni"
                                     ,"ano")]
# add socio into acc
tmp_join_acc <- data.table::setDT(tmp_acc)[
  data.table::setDT(tmp_soc), on = c("origin"="id_hex"
                                     ,"city" = "sigla_muni"
                                     ,"ano")]

# save
dir.create("data/")

readr::write_rds(tmp_join_car,"data/socio_acc-car.rds",compress = "gz")
readr::write_rds(tmp_join_acc,"data/socio_acc-all.rds",compress = "gz")
