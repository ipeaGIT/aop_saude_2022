
# setup -------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(sf)
library(aopdata)
library(patchwork)
#library(mapview)
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
  .(origin, city, cor_negra, pop_total)
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
  .(origin, city, cor_negra, pop_total)
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

# filter data ----------------------------------------------------------------

# filter public transport cities only
df_final <- subset(df_final, mode == "transit")

# add median
df_final[
  ,
  `:=`(
    #median_cma = median(CMASA30, na.rm = T),
    median_prop = median(prop_negra, na.rm = T)
    ), 
  by = .(city)
]

df_final[, city := factor(city, ordered = T)]
df_final[, city := reorder(city, median_prop)]
# df_final[
#   , 
#   city := factor(
#     x = city,
#     labels = c(
#       "Porto Alegre"
#       , "Curitiba"
#       , "Campinas"
#       , "São Paulo"
#       , "Goiânia"
#       , "Rio de Janeiro"
#       , "Belo Horizonte"
#       , "Recife"
#       , "Fortaleza"
#       , "Salvador"
#     )
#   )
# ]



# df_final[
#   ,
#   city := factor(
#     x = city, 
    # labels = c(
    #   "bho" = "Belo Horizonte"
    #   , "cam" = "Campinas"
    #   , "cur" = "Curitiba"
    #   , "for" = "Fortaleza"
    #   , "goi" = "Goiânia"
    #   , "poa" = "Porto Alegre"
    #   , "rec" = "Recife"
    #   , "rio" = "Rio de Janeiro"
    #   , "sal" = "Salvador"
    #   , "spo" = "São Paulo"
    # )
#       )
# ]

# df_final[
#   ,
#   city := forcats::fct_reorder(df_final$city, df_final$median_prop, na.rm = T)
# ]

df_median <- df_final[
  ,
  .(median_prop = unique(median_prop)),
  by = .(city)
]

# add labels ---------------------------------------------------------------
# df_label <- data.table(
#   city = "cur"
#   , prop_negra = 0.1698565
#   , CMASA30 = 35
#   , label = "Mediana"
# )

# set city as factor (for ordering)

df_label <- df_final[
  city == "poa",
  .(CMASA30 = 40, prop_negra = median(prop_negra), label = "Mediana"),
  by = city
]

# df_label <- data.table(
#   city = "bho"
#   , prop_negra = df_median[city=="bho", median_prop]
#   , CMASA30 = 40
#   , label = "Mediana"
# )

city_labels <- c(
  "poa" = "Porto Alegre"
  , "cur" = "Curitiba"
  , "cam" = "Campinas"
  , "spo" = "São Paulo"
  , "goi" = "Goiânia"
  , "rio" = "Rio de Janeiro"
  , "bho" = "Belo Horizonte"
  , "rec" = "Recife"
  , "for" = "Fortaleza"
  , "sal" = "Salvador"
)
  
  

# names(city_labels) <- df_final[mode == "transit",logical(1),by=city]$city %>% 
#   sort()



# grafico -----------------------------------------------------------------

# boxplot
# discretizar a proporcao de pessoas negras dentro do hexagono (25% por 25% cada grupo)


# * grafico final -------------------------------------------------------

# city := reorder(city, median_prop)

# ALTA
(
  gg_final <- 
  df_final %>% 
  ggplot(
    aes(
      x = prop_negra
      , y = CMASA30
      #, colour = as.factor(quintil)
      , fill = as.factor(quintil)
      , size = pop_total
      )
    ) +
  geom_point(
    shape = 21
    , alpha = 0.1 #0.075
    #, stroke = 0
    , colour = "#999999"#"#666666"
    ) + 
  geom_vline(
    data = df_median
    , aes(xintercept = median_prop)
    , linetype = "dashed"
    , colour = "#5d5d5d"
  ) +
  ggtext::geom_richtext(
    data = df_label, 
    aes(
      x = prop_negra
      , y = CMASA30
      , label = label
      )
    , colour = "black"
    , size = 3.75
    , inherit.aes = F
    , family = "Arial Narrow"
    ) +
  #scale_size(range = c(1, 10)) +
  lemon::facet_rep_wrap(
    ~city, ncol = 2, nrow = 5, labeller = labeller(city = city_labels),
    repeat.tick.labels = T
    ) +
  lemon::coord_capped_cart(
    bottom = "both"
    #, left = "both"
    ) +
  hrbrthemes::theme_ipsum() +
    theme(
      panel.grid.minor = element_blank()
      , panel.grid.major = element_blank()
      
      , axis.line.x = element_line(size = 0.1, color = "azure4")
      #, axis.line.y = element_line(size = 0.1, color = "grey")
      , axis.title.y = element_text(margin = margin(r = 0.2, unit = "cm")
                                    , size = 10)
      , axis.title.x = element_text(margin = margin(t = 0.2, unit = "cm")
                                    , size = 10)
      
      , legend.position = "bottom"
      , legend.box = "horizontal"
      , legend.spacing.y = unit(0, "cm")
      , legend.spacing.x = unit(0.05, "cm")
      , legend.text = element_text(margin = margin(t = 0, b = 0, unit = "cm"))
      , legend.box.margin = margin(t = -0.25, unit = "cm")
      
      , panel.spacing.y = unit(0.25, "cm")
      , panel.spacing.x = unit(1.5, "cm")
      , strip.switch.pad.wrap = unit(-5, "cm")
      , strip.background = element_blank()
      
      , plot.margin = unit(c(0.25,1,0.25,1), "cm") # t r b l
    ) +
  # scale_colour_brewer(
  #   palette = "RdYlBu"
  #   , guide = guide_legend(order = 1, reverse = T,
  #                          override.aes = list(alpha = 1, size = 4, colour = "black"))
  #   ) +
  scale_fill_brewer(
    palette = "RdYlBu"
    , guide = guide_legend(order = 1, reverse = T,
                           override.aes = list(alpha = 1, size = 4))
    ) +
  scale_size(
    range = c(1,10)
    # five-number summaries (removing median): fivenum(df_final$pop_total)[-2]
    , breaks = c(5,150,1000,9000) #fivenum(df_final$pop_total)[-2]
    , labels = c(5,150,1000,9000) #fivenum(df_final$pop_total)[-2]fivenum(df_final$pop_total)[-2]
    , guide = guide_legend(order = 2, 
                           override.aes = list(colour = "#5d5d5d", alpha = 1))
  ) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1)
    , expand = expansion(add = c(0.0, 0.05))
  ) +
  scale_y_continuous(
    expand = expansion(add = c(5,5))
  ) +
  labs(
    #subtitle = "Alta complexidade - Transporte público",
     x = "Proporção População Negra"
    , y = "Quantidade de estabelecimentos" #"Estabelecimentos Alta Complexidade\n acessíveis em até 30 min Transporte Público"
    , fill = "Quintil de renda\n (Maior-Menor)" #"Quintil de renda\n1 (menor) a 5 (maior)"
    #, colour = "Quintil de renda\n (Maior-Menor)"
    , size = "População\nHexágono"
    ) +
  guides(
    fill = guide_legend(
      order = 1, reverse = T, override.aes = list(alpha = 1, size = 4, colour = "black")
      )
    , colour = guide_legend(
      override.aes = list(alpha = 1, size = 4, colour = "black")
    )
  )
  
)
  
# * grafico s/ size -------------------------------------------------------

# city := reorder(city, median_prop)

# ALTA
(
  gg_final_s_size <- 
    df_final %>% 
    ggplot(
      aes(
        x = prop_negra
        , y = CMASA30
        , colour = as.factor(quintil)
        , fill = as.factor(quintil)
        #, size = pop_total
      )
    ) +
    geom_point(
      shape = 21
      , alpha = 0.075
    ) + 
    geom_vline(
      data = df_median
      , aes(xintercept = median_prop)
      , linetype = "dashed"
      , colour = "#5d5d5d"
    ) +
    ggtext::geom_richtext(
      data = df_label, 
      aes(
        x = prop_negra
        , y = CMASA30
        , label = label
      )
      , colour = "black"
      , size = 3.75
      , inherit.aes = F
      , family = "Arial Narrow"
    ) +
    #scale_size(range = c(1, 10)) +
    # facet_wrap(
    #   ~city, ncol = 2, nrow = 5, labeller = labeller(city = city_labels)
    # ) +
    lemon::facet_rep_wrap(
      ~city, ncol = 2, nrow = 5, labeller = labeller(city = city_labels),
      repeat.tick.labels = T
    ) +
    lemon::coord_capped_cart(
      bottom = "both"
      #, left = "both"
    ) +
    hrbrthemes::theme_ipsum() +
    theme(
      panel.grid.minor = element_blank()
      , panel.grid.major = element_blank()
      
      , axis.line.x = element_line(size = 0.1, color = "azure4")
      #, axis.line.y = element_line(size = 0.1, color = "grey")
      , axis.title.y = element_text(margin = margin(r = 0.2, unit = "cm")
                                    , size = 10)
      , axis.title.x = element_text(margin = margin(t = 0.2, unit = "cm")
                                    , size = 10)
      
      , legend.position = "bottom"
      , legend.box = "horizontal"
      , legend.spacing.y = unit(0, "cm")
      , legend.spacing.x = unit(0.05, "cm")
      , legend.text = element_text(margin = margin(t = 0, b = 0, unit = "cm"))
      , legend.box.margin = margin(t = -0.25, unit = "cm")
      
      , panel.spacing.y = unit(0.25, "cm")
      , panel.spacing.x = unit(1.5, "cm")
      , strip.switch.pad.wrap = unit(-5, "cm")
      , strip.background = element_blank()
      
      , plot.margin = unit(c(0.25,1,0.25,1), "cm") # t r b l
    ) +
    scale_colour_brewer(
      palette = "RdYlBu"
      , guide = guide_legend(order = 1, reverse = T,
                             override.aes = list(alpha = 1, size = 4, colour = "black"))
    ) +
    scale_fill_brewer(
      palette = "RdYlBu"
      , guide = guide_legend(order = 1, reverse = T,
                             override.aes = list(alpha = 1, size = 4))
    ) +
    #scale_colour_aop(palette = "blue_red") +
    #scale_fill_aop(
    #  palette = "blue_red"
    #  , guide = guide_legend(override.aes = list(alpha = 0.8, size = 4))
    #  ) +
    # scale_size(
    #   range = c(1,10)
    #   , breaks = fivenum(df_final$pop_total)[-2]
    #   , labels = fivenum(df_final$pop_total)[-2]
    #   , guide = guide_legend(order = 2, 
    #                          override.aes = list(colour = "#5d5d5d", alpha = 1))
    # ) +
    scale_x_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1)
      , expand = expansion(add = c(0.02, 0.05))
    ) +
    scale_y_continuous(
      expand = expansion(add = c(5,5))
    ) +
    #scale_color_viridis_d() +
    #scale_fill_viridis_d() +
    labs(
      #subtitle = "Alta complexidade - Transporte público",
      x = "Proporção População Negra"
      , y = "Quantidade de estabelecimentos" #"Estabelecimentos Alta Complexidade\n acessíveis em até 30 min Transporte Público"
      , fill = "Quintil de renda\n (Maior-Menor)" #"Quintil de renda\n1 (menor) a 5 (maior)"
      , colour = "Quintil de renda\n (Maior-Menor)"
      #, size = "População\nHexágono"
    ) #+
  #guides(
  #  fill = guide_legend(order = 1)
  #  , size = guide_legend(order = 2)
  #  , colour = guide_legend(order = 1)
  
  #size = guide_legend(override.aes = list(size = c(1,5,10)))
  #,fill = guide_legend(override.aes = list(size = 3, alpha = 0.8))
  #)
  
)


# * save plot -------------------------------------------------------------

  png(here::here("figures", "cma_prop_negro_alta_complex_hexagonos.png"),
      width = 16.5, height = 19, units = "cm", res = 600, type = "cairo"
  )
  
  gg_final
  
  dev.off()
  
  #
  png(here::here("figures", "cma_prop_negro_alta_complex_hexagonos_SEMSIZE.png"),
      width = 16.5, height = 19, units = "cm", res = 600, type = "cairo"
  )
  
  gg_final_s_size
  
  dev.off()
  
