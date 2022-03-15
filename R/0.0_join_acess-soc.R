

# load packages ------

rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
library(sf)
library(patchwork)
library(aopdata) # devtools::install_github("ipeaGIT/aopdata", subdir = "r-package")


# 1) List files ------
filepath_acess <- "L:\\Proj_acess_oport\\data\\acesso_oport\\output_access"

files_acess <- list.files(path = filepath_acess#paste0(filepath_acess,"\\2017")
                          ,recursive = TRUE
                          ,full.names = TRUE)
files_acess <- files_acess[!(files_acess %like% "_old")]
files_acess <- files_acess[!(files_acess %like% "_car")]
files_acess <- files_acess[(files_acess %like% "2019")]


files_acess_car <- list.files(path = filepath_acess#paste0(filepath_acess,"\\2017")
                              ,pattern = "car"
                              ,recursive = TRUE
                              ,full.names = TRUE)
files_acess_car <- files_acess_car[(files_acess_car %like% "2019")]

filepath_socio <- "L:\\Proj_acess_oport\\data\\acesso_oport\\hex_agregados\\"
files_socio <- list.files(path = filepath_socio#paste0(filepath_socio,"\\2017")
                          ,recursive = TRUE
                          ,pattern = "09"
                          ,full.names = TRUE)
files_socio <- files_socio[!(files_socio %like% "_old")]
files_socio <- files_socio[(files_socio %like% "2019")]

# 2) Read and rbind ----

future::plan(strategy = "multisession",workers = 19)
tmp_acc <- future.apply::future_lapply(files_acess,readr::read_rds) %>%
  data.table::rbindlist(fill=TRUE)
tmp_acc <- tmp_acc[,c("origin","city","mode","pico","ano",
                      "CMAST15","CMASB15","CMASM15","CMASA15",
                      "CMAST30","CMASB30","CMASM30","CMASA30",
                      "CMAST45","CMASB45","CMASM45","CMASA45",
                      "TMIST","TMISB","TMISM","TMISA","geometry")]

tmp_soc <- future.apply::future_lapply(files_socio,readr::read_rds) %>% 
  data.table::rbindlist(fill=TRUE)
tmp_soc <- tmp_soc[,c("id_hex","h3_resolution","sigla_muni","ano"
                      ,"cor_branca","cor_amarela","cor_indigena","cor_negra",
                      "pop_total","renda_total","renda_capita","quintil",
                      "decil","saude_total","saude_baixa","saude_media",
                      "saude_alta")]

tmp_car <- future.apply::future_lapply(files_acess_car,readr::read_rds) %>%
  data.table::rbindlist(fill=TRUE) 
tmp_car <- tmp_car[,c("origin","city","mode","pico","ano",
                      "CMAST30","CMASB30","CMASM30","CMASA30",
                      "CMAST60","CMASB60","CMASM60","CMASA60",
                      "CMAST90","CMASB90","CMASM90","CMASA90",
                      "TMIST","TMISB","TMISM","TMISA","geometry")]


# 3) Joint dataset ----
# add socio into car
tmp_join_car <- data.table::setDT(tmp_car)[
  data.table::setDT(tmp_soc), on = c("origin"="id_hex"
                                     ,"city" = "sigla_muni"
                                     ,"ano")]
# add socio into acc
tmp_join_acc <- data.table::copy(tmp_acc)[
  data.table::copy(tmp_soc), on = c("origin"="id_hex"
                                    ,"city" = "sigla_muni"
                                    ,"ano" = "ano")]

nrow(tmp_join_acc)
# 3) Filter files -----
tmp_join_car <- tmp_join_car[!is.na(mode),]
tmp_join_acc <- tmp_join_acc[!is.na(mode),]

# save
dir.create("data/")

readr::write_rds(tmp_join_car,"data/socio_acc-car_2019.rds",compress = "gz")
readr::write_rds(tmp_join_acc,"data/socio_acc-all_2019.rds",compress = "gz")
