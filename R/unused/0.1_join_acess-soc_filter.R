


# load packages ------

rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(readr)


# read files ------

tmp_join_car <- readr::read_rds("data/socio_acc-car.rds")
tmp_join_acc <- readr::read_rds("data/socio_acc-all.rds")


tmp_join_acc[1]
tmp_join_car[1]
# joint dataset ----
# add socio into car
tmp_join_car <- tmp_join_car[,.(origin,city,mode
                                ,pico
                                ,CMASA30,CMASB30
                                ,TMISB,TMISA
                                ,geometry
                                ,ano
                                ,cor_branca
                                ,cor_amarela
                                ,cor_indigena
                                ,cor_negra
                                ,pop_total
                                ,decil
                                ,renda_total 
                                ,renda_capita)]

# read files ------
filepath_acess <- "L:\\Proj_acess_oport\\data\\acesso_oport\\output_access"

files_acess <- list.files(path = filepath_acess#paste0(filepath_acess,"\\2017")
                          ,recursive = TRUE
                          ,full.names = TRUE)
files_acess <- files_acess[!(files_acess %like% "_old")]
files_acess <- files_acess[!(files_acess %like% "_car")]

# add socio into acc
tmp_join_acc <- tmp_join_acc[,.(origin,city,mode
                                ,pico
                                ,CMASA30,CMASB30
                                ,TMISB,TMISA
                                ,geometry
                                ,ano
                                ,cor_branca
                                ,cor_amarela
                                ,cor_indigena
                                ,cor_negra
                                ,pop_total
                                ,decil
                                ,renda_total 
                                ,renda_capita)]


# rbind
tmp_join <- rbind(tmp_join_car,tmp_join_acc)


tmp_join %>% 
  .[mode == "walk" & ano == 2019 & CMASA30 > 0,] %>% 
  .[,.SD[1],by = city] %>% 
  .[,city]



readr::write_rds(x = tmp_join,"data/acess_merged.rds")
