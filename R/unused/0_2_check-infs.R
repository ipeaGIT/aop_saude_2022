# Load packages ------

rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
library(sf)
library(patchwork)
# add style
source('R/colours.R')

# Read files ------

tmp_join_bus_raw <- readr::read_rds("data/socio_acc-all_2019.rds") 
tmp_join_car_raw <- readr::read_rds("data/socio_acc-car_2019.rds") 

# Analysis -----

tmp_join <- list(tmp_join_bus_raw,tmp_join_car_raw) %>% 
  data.table::rbindlist(use.names = TRUE,fill = TRUE)


# join
tmp_inf_check <- data.table::copy(tmp_join) %>% 
  .[pop_total > 0,] %>% 
  .[pico == 1,]

tmp_inf_check[,
              {
                # TMISB
                TMISB_inf = sum(is.infinite(TMISB))
                TMISB_na  = sum(is.na(TMISB) | is.nan(TMISB))
                
                # TMISA
                TMISA_inf = sum(is.infinite(TMISA))
                TMISA_na  = sum(is.na(TMISA) | is.nan(TMISA))
                
                # CMASB30
                CMASB30_inf = sum(is.infinite(CMASB30))
                CMASB30_na  = sum(is.na(CMASB30) | is.nan(CMASB30))
                
                # CMASA30
                CMASA30_inf = sum(is.infinite(CMASA30))
                CMASA30_na  = sum(is.na(CMASA30) | is.nan(CMASA30))
                
                # soma
                total_hexagono = .N
                
                # export
                list(
                   `TMISB_na(%)`      = round( 100 * TMISB_na / total_hexagono, 2)
                  ,`TMISA_na(%)`      = round( 100 * TMISA_na / total_hexagono, 2)
                  ,`TMISB_inf(%)`     = round( 100 * TMISB_inf / total_hexagono, 2)
                  ,`TMISA_inf(%)`     = round( 100 * TMISA_inf / total_hexagono, 2)
                  ,`CMASB30_na(%)`    = round( 100 * CMASB30_na / total_hexagono, 2)
                  ,`CMASA30_na(%)`    = round( 100 * CMASA30_na / total_hexagono, 2)
                  ,`CMASB30_inf(%)`   = round( 100 * CMASB30_inf / total_hexagono, 2)
                  ,`CMASA30_inf(%)`   = round( 100 * CMASA30_inf / total_hexagono, 2)
                )
                }
              ,by = .(mode,ano,pico)]


tmp_inf_check[city == "spo"]
# End-----