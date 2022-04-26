
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
  ggplot(aes(x = class_race, y = CMASA30, fill = extremos, weight = pop_total))+
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
    ,alpha = 0.1
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
  scale_colour_aop(palette = "blue_red") +
  scale_fill_aop(palette = "blue_red") +
  #scale_color_viridis_d() +
  #scale_fill_viridis_d() +
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



