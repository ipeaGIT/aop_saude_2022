
# setup -------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(sf)
library(aopdata)
library(patchwork)
library(mapview)


# read data ---------------------------------------------------------------

# MODIFICAR DEPOIS PARA BASE BAIXADA AOPDATA
df_all <- readRDS("E:/JPParga/socio_acc-all.rds")
df_car <- readRDS("E:/JPParga/socio_acc-car.rds")

df_all[, geometry := NULL]
df_all[, i.geometry := NULL]
df_car[, geometry := NULL]
df_car[, i.geometry := NULL]

# DUVIDA: 
# o que quer dizer quando hexagono tem pessoas mas CMASA30, por exemplo, é NA?
# alguns casos assim em Manaus

df_pop <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1 & mode == "walk",
  .(origin, city, cor_indigena, cor_negra, pop_total)
]

# check any na
any(is.na(df_pop))
# check any nan -> necessary create function
is.nan.df <- function(x){
  do.call(cbind, lapply(x, is.nan))
}
any(is.nan.df(df_pop))

# create column prop negra + indigena
df_pop[, prop_negra_indigena := (cor_indigena + cor_negra) / pop_total]

# origin == "898af6bac2fffff" possui prop > 1 -> soma indigena e negro > pop total
# PROVISORIO ABAIXO
df_pop[prop_negra_indigena > 1]$prop_negra_indigena <- 1

df_renda <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1 & mode == "walk",
  .(origin, city, renda_total, renda_capita, quintil, decil)
]

df_pop_renda <- dplyr::left_join(df_pop, df_renda)


df_t <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1,
  .(origin, city, mode, pico, CMASA30, CMASB30)
]

a <- left_join(
  df_pop_renda, df_t, 
  by = c("origin" = "origin", "city" = "city")
  )




66666666666 ATUALIZAR

df_all_hosp <- df_all[
  ano == 2019 
]

df_temp_all <- df_all[
  ano == 2019,
  .(origin, city, mode, pico, CMASA30, CMASB30)
]

df_temp_car <- df_car[
  ano == 2019,
  .(origin, city, mode, pico, CMASA30, CMASB30)
]

a <- dplyr::left_join(df_temp_all, df_temp_car, by = "origin")
df <- data.table::merge.data.table(df_temp_all, df_temp_car, by = "origin")

# proportion negro + indigena ---------------------------------------------



df_pop <- df_all[
  ano == 2019, 
  .(origin, cor_indigena, cor_negra, pop_total)
]

# excluir hexagonos que nao tem populacoa

df_pop <- subset(df_pop, pop_total >= 1)



df_pop[is.nan.df(df_pop)] <- 0
df_pop[is.na(df_pop)] <- 0

df_pop[, prop_negra_indigena := (cor_indigena + cor_negra) / pop_total]

df_pop[is.nan.df(df_pop)] <- 0
df_pop[is.na(df_pop)] <- 0


# select vars -------------------------------------------------------------

df_temp_all <- df_all[
  ano == 2019,
  .(origin, city, mode, pico, CMASA30, CMASB30, renda_total, renda_capita)
]

df_temp_car <- df_car[
  ano == 2019,
  .(origin, city, mode, pico, CMASA30, CMASB30)
]

####### CONSERTAR JOIN -> ENTENDER PORQUE LEFT JOIN COM DF POP GERA DUPLICADAS
df_temp <- dplyr::left_join(df_temp_all, df_temp_car)
#df_temp <- data.table::merge.data.table(df_temp_all, df_temp_car)

a <- dplyr::left_join(df_temp, df_pop, by = c("origin" = "origin"))

df_temp_all[
  df_pop,
  on = "origin",
  prop_negra_indigena := i.prop_negra_indigena
]



# add mean ----------------------------------------------------------------


# * CMA -------------------------------------------------------------------


# * prop ------------------------------------------------------------------


# grafico -----------------------------------------------------------------

# boxplot
# discretizar a proporcao de pessoas negras dentro do hexagono (25% por 25% cada grupo)



# boxplot -----------------------------------------------------------------

# discretizar a pop do hexagono de acordo com a renda
# quartil (25% em 25%)
# quintil (20 em 20%)
# VER ANOTACOES final 29/03

# classificar o hexagono de acordo com o percentual de negros

# * scatterplot -----------------------------------------------------------



# testar colorir por quintil de renda

df_temp_all[mode=="walk" & city=="bho" & prop_negra_indigena != 0 & CMASA30 != 0] %>% 
  ggplot(aes(x = prop_negra_indigena, y = CMASA30)) + 
  geom_point() + 
  #geom_smooth() + 
  theme_minimal()

df_temp_all[mode=="walk"] %>% 
  ggplot(aes(x = prop_negra_indigena, y = CMASB30, colour = renda_capita)) + 
  geom_point() +
  facet_wrap(~city, nrow = 5, ncol = 2, scales = "free_y") +
  scale_colour_viridis_c(option = "inferno"
                         , trans = "pseudo_log"
  ) + 
  #geom_smooth() + 
  theme_minimal()


df_temp_all[mode=="transit" & prop_negra_indigena != 0 & CMASA30 != 0] %>% 
  ggplot(aes(x = prop_negra_indigena, y = CMASA30, colour = renda_capita)) + 
  geom_point() +
  facet_wrap(~city, nrow = 5, ncol = 2) +
  scale_colour_viridis_c(option = "inferno"
                         #, trans = "log"
  ) + 
  #geom_smooth() + 
  theme_minimal()


df_temp_all[mode=="bike" & prop_negra_indigena != 0 & CMASB30 != 0] %>% 
  ggplot(aes(x = prop_negra_indigena, y = CMASA30, colour = renda_capita)) + 
  geom_point() +
  facet_wrap(~city, nrow = 5, ncol = 2) +
  scale_colour_viridis_c(option = "inferno") + 
  #geom_smooth() + 
  theme_minimal()


# boxplot
# discretizar a proporcao de pessoas negras dentro do hexagono (25% por 25% cada grupo)



