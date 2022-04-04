
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
source("R/style.R")
source("R/colours.R")


# read data ---------------------------------------------------------------

# MODIFICAR DEPOIS PARA BASE BAIXADA AOPDATA
df_all <- readRDS("data/socio_acc-all_2019.rds")
df_car <- readRDS("data/socio_acc-car_2019.rds")

#df_all[, geometry := NULL]
#df_car[, geometry := NULL]


# df prop negra -------------------------------------------------------------

# DUVIDA: 
# o que quer dizer quando hexagono tem pessoas mas CMASA30, por exemplo, é NA?
# alguns casos assim em Manaus

df_prop <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1 & mode == "walk",
  .(origin, city, cor_indigena, cor_negra, pop_total)
]

df_prop <- unique(df_prop, by = "origin")

# check any na
any(is.na(df_prop))
# check any nan -> necessary create function
is.nan.df <- function(x){
  do.call(cbind, lapply(x, is.nan))
}
any(is.nan.df(df_prop))

# create column prop negra + indigena
df_prop[, prop_negra_indigena := (cor_indigena + cor_negra) / pop_total]

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
  ano == 2019 & pop_total > 0 & pico == 1 & mode == "walk",
  .(origin, city, pop_total, renda_total, renda_capita, quintil, decil)
]

df_renda <- unique(df_renda, by = "origin")

# check any na
any(is.na(df_renda))
# check any nan -> necessary create function
any(is.nan.df(df_renda))


df_renda[
  ,
  extremos := data.table::fcase(
    decil <= 2, "20% mais pobres",
    decil >= 9, "20% mais ricos",
    default = "Intermediário"
  )
]

# * df cma ----------------------------------------------------------------

df_cma <- df_all[
  ano == 2019 & pop_total > 0 & pico == 1,
  .(origin, city, mode, CMASA30, CMASB30)
]


# join data ---------------------------------------------------------------
df_final <- dplyr::left_join(
  df_prop,
  df_renda %>% select(-pop_total), 
  by = c("origin" = "origin", "city" = "city")
  )

df_final <- dplyr::left_join(
  df_final, df_cma,
  by = c("origin" = "origin", "city" = "city")
  
)

# CHECK FOR DUPLICATED ORIGINS


# add mean ----------------------------------------------------------------



# grafico -----------------------------------------------------------------

# boxplot
# discretizar a proporcao de pessoas negras dentro do hexagono (25% por 25% cada grupo)

# * violin ---------------------------------------------------------------


#SAUDE BAIXA

df_final[city == "bho"& mode == "walk" & !extremos == "Intermediário"] %>%  
  ggplot(aes(x = class_race, y = CMASB30, fill = class_race, colour = class_race))+
  geom_violin(width = 2.1, size = 0.2) +
  facet_wrap(~extremos) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d()+
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "none")+
  coord_flip()

#SAUDE ALTA
vio_cur <- df_final[city == "cur"& mode == "transit" & !extremos == "Intermediário"] %>%  
  ggplot(aes(x = class_race, y = CMASA30, fill = class_race, colour = class_race))+
  geom_violin(width = 2.1, size = 0.2) +
  facet_wrap(~extremos) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d()+
  #aop_style() +
  hrbrthemes::theme_ipsum() +
  theme(
    legend.position = "none"
    , panel.spacing.x = unit(0.5, "cm")
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
  )+
  coord_flip() +
  labs(subtitle = "Curitiba")


vio_for <- df_final[city == "for"& mode == "transit" & !extremos == "Intermediário"] %>%  
  ggplot(aes(x = class_race, y = CMASA30, fill = class_race, colour = class_race))+
  geom_violin(width = 2.1, size = 0.2) +
  facet_wrap(~extremos) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d()+
  #aop_style() +
  hrbrthemes::theme_ipsum() +
  theme(
    legend.position = "none"
    , strip.text.x = element_blank()
    , panel.spacing.x = unit(0.5, "cm")
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
  )+
  coord_flip() +
  labs(subtitle = "Fortaleza")


vio_rio <- df_final[city == "rio"& mode == "transit" & !extremos == "Intermediário"] %>%  
  ggplot(aes(x = class_race, y = CMASA30, fill = class_race, colour = class_race))+
  geom_violin(width = 2.1, size = 0.2) +
  facet_wrap(~extremos) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d()+
  #aop_style() +
  hrbrthemes::theme_ipsum() +
  theme(
    legend.position = "none"
    , strip.text.x = element_blank()
    , panel.spacing.x = unit(0.5, "cm")
    , panel.grid.major.y = element_blank()
    , panel.grid.minor.x = element_blank()
    )+
  coord_flip() +
  labs(subtitle = "Rio de Janeiro")

vio_cur / vio_for / vio_rio


# discretizar a pop do hexagono de acordo com a renda
# quartil (25% em 25%)
# quintil (20 em 20%)
# VER ANOTACOES final 29/03

# classificar o hexagono de acordo com o percentual de negros


# * boxplot -----------------------------------------------------------------
df_final[mode == "transit" & extremos != "Intermediário"] %>%  
  ggplot(aes(x = class_race, y = CMASA30, fill = extremos))+
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~city, ncol = 5, scales = "free_y") +
  hrbrthemes::theme_ipsum() +
  theme(legend.position = "bottom") +
  labs(subtitle = "Alta complexidade - Transporte público")


# * scatterplot -----------------------------------------------------------

# BAIXA
df_final[mode == "walk"] %>% 
  ggplot(aes(x = prop_negra_indigena, y = CMASB30,
             colour = as.factor(quintil), fill = as.factor(quintil)
             ,size = pop_total
  ))+
  geom_point(
    shape = 21
    ,alpha = 0.25
  ) + 
  scale_size(range = c(1, 10)) +
  lemon::facet_rep_wrap(~city, ncol = 5) +
  lemon::coord_capped_cart(bottom = "both", left = "both") +
  hrbrthemes::theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(size = 0.1, color = "grey"),
    axis.line.y = element_line(size = 0.1, color = "grey"),
    legend.position = "bottom"
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(subtitle = "Baixa complexidade - A pé")

# ALTA
df_final[mode == "transit"] %>% 
  ggplot(aes(x = prop_negra_indigena, y = CMASA30,
             colour = as.factor(quintil), fill = as.factor(quintil)
             ,size = pop_total
             ))+
  geom_point(
    shape = 21
    ,alpha = 0.25
    ) + 
  scale_size(range = c(1, 10)) +
  lemon::facet_rep_wrap(~city, ncol = 5) +
  lemon::coord_capped_cart(bottom = "both", left = "both") +
  hrbrthemes::theme_ipsum() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(size = 0.1, color = "grey"),
    axis.line.y = element_line(size = 0.1, color = "grey"),
    legend.position = "bottom"
  ) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(subtitle = "Alta complexidade - Transporte público")


# * density ---------------------------------------------------------------
# ALTA
df_final[mode == "transit"] %>% 
  ggplot(aes(x = CMASA30, group = class_race, fill = class_race))+
  geom_density(adjust = 1.5, alpha = 0.3) +
  facet_wrap(~city, ncol = 5, scales = "free") +
  #lemon::coord_capped_cart(bottom = "both", left = "both") +
  hrbrthemes::theme_ipsum() +
  scale_fill_viridis_d() +
  labs(subtitle = "Alta complexidade - Transporte público")



