
# description -------------------------------------------------------------
# this script classifies hexagons based on the proportion of people of colour
# and on income per capita (20% richer and 20% poorer)

# setup -------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(sf)
library(aopdata)


# read data ---------------------------------------------------------------

df_all <- readRDS("data/socio_acc-all_2019.rds")
df_car <- readRDS("data/socio_acc-car_2019.rds")

#df_all[, geometry := NULL]
#df_car[, geometry := NULL]


# df prop negra -------------------------------------------------------------

# DUVIDA: 
# o que quer dizer quando hexagono tem pessoas mas CMASA30, por exemplo, é NA?
# alguns casos assim em Manaus

df_prop <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1 ,
  .(origin, city, mode, cor_indigena, cor_negra, pop_total)
]

#df_prop <- unique(df_prop, by = "origin")
df_prop <- unique(df_prop, by = c("origin", "city"))
666666
# hexagonos duplicados nos pares gua-spo e duq-rio -> O QUE FAZER?
#identificar duplicados:
df_prop[duplicated(origin) | duplicated(origin, fromLast=T)] #%>% View()  

# check any na
any(is.na(df_prop))
# check any nan -> necessary create function
is.nan.df <- function(x){
  do.call(cbind, lapply(x, is.nan))
}
any(is.nan.df(df_prop))

# create column prop negra + indigena
df_prop[
  , prop_negra_indigena := (cor_indigena + cor_negra) / pop_total, 
  by = .(origin, city)
  ]

# alguns hexagonos possuem prop_negra_indigena > 1. Ou seja, indigena + negra > pop_total
# O QUE FAZER?
# PROVISORIO ABAIXO
df_prop[prop_negra_indigena > 1]$prop_negra_indigena <- 1

# * classify hex based on % race -----------------------------------------------
df_prop[
  ,
  class_race := cut(
    prop_negra_indigena,
    seq(0, 1, 0.25),
    c("<25%", "25-49%", "50-75%", "75%>"),
    include.lowest = T
  )
]


# df renda -------------------------------------------------------

df_renda <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1,
  .(origin, city, pop_total, renda_total, renda_capita, quintil, decil)
]

df_renda <- unique(df_renda, by = c("origin", "city"))

# check any na
any(is.na(df_renda))
# check any nan -> necessary create function
any(is.nan.df(df_renda))

# CHECK NUMBER OF quintis and decils
df_renda[, logical(1), by = decil]$decil %>% sort()
df_renda[, logical(1), by = quintil]$quintil %>% sort()
# quintil and decil == 0 -> CHECAR O QUE SIGNIFICA 
# remove quintil and decil == 0
df_renda <- subset(df_renda, quintil != 0)
df_renda <- subset(df_renda, decil != 0)

df_renda[duplicated(origin) | duplicated(origin, fromLast=T)] # %>% View()

df_renda[
  ,
  extremos := data.table::fcase(
    decil <= 2, "20% mais pobres",
    decil >= 9, "20% mais ricos",
    default = "Intermediário"
  )
]


# join data ---------------------------------------------------------------
df_final <- dplyr::left_join(
  df_prop %>% select(origin, city, prop_negra_indigena, class_race),
  df_renda %>% select(origin, city, extremos), 
  by = c("origin" = "origin", "city" = "city")
)

# CHECK FOR DUPLICATED ORIGINS
any(duplicated(df_final$origin))

# save data ---------------------------------------------------------------
saveRDS(
  df_final,
  file = "data/hex_prop_black_income.rds",
  compress = "xz"
)

