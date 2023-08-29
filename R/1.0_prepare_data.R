

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



# 1) Download_files -----


est_saude_walk <- aopdata::read_access(
  city = "all"
  ,year =  2019
  ,mode = "walk"
  ,geometry = T)
names(est_saude_walk)
setDT(est_saude_walk)


est_saude_pt <- aopdata::read_access(
  city = "all"
  ,year =  2019
  ,mode = "public_transport"
  ,geometry = T)


est_saude_car <- aopdata::read_access(
  city = "all"
  ,year =  2019
  ,mode = "car"
  ,geometry = T)


est_saude_bic <- aopdata::read_access(
  city = "all"
  ,year =  2019
  ,mode = "bicycle"
  ,geometry = T)


names(est_saude_bic)  %>% length()
names(est_saude_car)  %>% length()
names(est_saude_walk)  %>% length()
names(est_saude_pt)  %>% length()

all_vars <- c(
  "id_hex","abbrev_muni","name_muni","code_muni","year"
  ,"P001","P002","P003","P004","P005"
  ,"R001","R002","R003"
  ,"S001","S002","S003","S004"
  ,"mode","peak"
  ,"CMAST15","CMASB15","CMASM15","CMASA15"
  ,"CMAST30","CMASB30","CMASM30","CMASA30"
  ,"TMIST","TMISB","TMISM","TMISA"
  ,"geometry"
)

tmp_join_acc <- list( setDT(est_saude_bic)[,.SD,.SDcols = all_vars]
                      ,setDT(est_saude_walk)[,.SD,.SDcols = all_vars]
                      ,setDT(est_saude_pt)[,.SD,.SDcols = all_vars]
                      ,setDT(est_saude_car)[,.SD,.SDcols = all_vars]
) %>% 
  data.table::rbindlist()

# setnames

setnames(tmp_join_acc
         ,old = c(  "abbrev_muni","year"
                    ,"P001","P002","P003","P004","P005"
                    ,"R001","R002","R003"
                    ,"S001","S002","S003","S004"
                    )
         ,new = c("sigla_muni","ano"
                  , "pop_total","cor_branca","cor_negra","cor_indigena","cor_amarela"
                  , "renda_capita","quintil", "decil"
                  , "saude_total","saude_baixa","saude_media","saude_alta"
                  ))

# save
readr::write_rds(tmp_join_acc,"data/socio_acc-all_2019.rds",compress = "gz")


# 2) Download tiles ------

rio_url <- "https://github.com/ipeaGIT/aop_saude_2022/releases/download/v.1.0/maptile_crop_mapbox_rio_2019.rds"
for_url <- "https://github.com/ipeaGIT/aop_saude_2022/releases/download/v.1.0/maptile_crop_mapbox_for_2019.rds"
cur_url <- "https://github.com/ipeaGIT/aop_saude_2022/releases/download/v.1.0/maptile_crop_mapbox_cur_2019.rds"

dir.create("data-raw")
download.file(url = rio_url,destfile = "data-raw/maptile_crop_mapbox_rio_2019.rds",mode = "wb")
download.file(url = for_url,destfile = "data-raw/maptile_crop_mapbox_for_2019.rds",mode = "wb")
download.file(url = cur_url,destfile = "data-raw/maptile_crop_mapbox_cur_2019.rds",mode = "wb")
