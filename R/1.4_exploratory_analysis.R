
# setup -------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(sf)
library(aopdata)
library(patchwork)
library(mapview)
library(Hmisc)
library(hrbrthemes)
library(lemon)
library(rgeoda)
source("R/style.R")
source("R/colours.R")


# read data ---------------------------------------------------------------

df_all <- readRDS("data/socio_acc-all_2019.rds")
#df_car <- readRDS("data/socio_acc-car_2019.rds")

#df_all[, geometry := NULL]
#df_car[, geometry := NULL]


# * df prop negra -------------------------------------------------------------

# DUVIDA: 
# o que quer dizer quando hexagono tem pessoas mas CMASA30, por exemplo, é NA?
# alguns casos assim em Manaus

df_prop <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1 & mode == "walk",
  .(origin, city, cor_indigena, cor_negra, pop_total)
]

df_prop <- unique(df_prop, by = "origin")


df_hex_prop_black <- readr::read_rds("data/hex_prop_black_income.rds")


# * df renda -------------------------------------------------------

df_renda <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1 & mode == "walk",
  .(origin, city, renda_total, renda_capita, quintil, decil)
]

df_renda <- unique(df_renda, by = "origin")


# * df cma ----------------------------------------------------------------

df_cma <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1,
  .(origin, city, mode, CMASA30, CMASB30)
]


# join data ---------------------------------------------------------------
df_prop <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1 & mode == "walk",
  .(origin, city, cor_indigena, cor_negra, pop_total)
]
# remove duplicated origins
df_prop <- unique(df_prop, by = "origin")


df_final <- dplyr::left_join(
  df_prop,
  df_hex_prop_black,
  by = c("origin" = "origin", "city" = "city")
) %>% 
  dplyr::left_join(
    df_renda,
    by = c("origin" = "origin", "city" = "city")
  ) %>% 
  dplyr::left_join(
    df_cma,
    by = c("origin" = "origin", "city" = "city")
  )

df_final <- subset(df_final, quintil != 0)


# spatial weights ---------------------------------------------------------


# LISA --------------------------------------------------------------------


# spatial clustering ------------------------------------------------------


# plot --------------------------------------------------------------------


