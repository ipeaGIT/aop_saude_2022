
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

# create column -----------------------------------------------------------

df_all[
  , prop_negra_indigena := (cor_indigena + cor_negra) / pop_total, 
  by = .(origin, city)
]

df_all[prop_negra_indigena > 1]$prop_negra_indigena <- 1

# remover nan de prop_negra_indigena -> PODE RESULTAR EM AUTOCORRELACAO ESPACIAL?
data.table::setnafill(df_all, fill = 0, cols = "prop_negra_indigena")

# function ----------------------------------------------------------------
# filtro: pico == 1
# selecionar modo``

f_geoda <- function(df, cidade, modo, complexidade){
  
  # cidade <- "for"
  # modo <- "walk"
  
  # subset city
  df_sub <- df[
    city == cidade & mode == modo & pico == 1
  ]
  # remover duplicados
  df_sub <- unique(df_sub, by = "origin")
  
  
  # st as sf
  df_sub <- sf::st_as_sf(df_sub)
  
  # queen contiguity
  queen_w <- rgeoda::queen_weights(
    sf_obj = df_sub
    , include_lower_order = F
    , precision_threshold = 0
  )
  
  # LISA
  lisa <- rgeoda::local_moran(
    w = queen_w 
    , df = df_sub["prop_negra_indigena"]
    , permutations = 9999
    )
  
  df_sub <- df_sub %>% 
    dplyr::mutate(lisa_clst_10 = rgeoda::lisa_clusters(lisa, cutoff = 0.1))
  # check lisa labels
  #lisa_labels(lisa)


  # plot
  lisacolors <- lisa_colors(lisa)
  lisalabels <- lisa_labels(lisa)
  
  df_sub %>% 
    ggplot() + 
    geom_sf(aes(fill = factor(lisa_clst_10))) +
    scale_fill_manual(
      values = lisa_colors(lisa)
      , labels = lisa_labels(lisa)
      )
    labs
}

# spatial weights ---------------------------------------------------------

# * queen contiguity ------------------------------------------------------

queen_w <- rgeoda::queen_weights(
  df_final
)

# LISA --------------------------------------------------------------------


# spatial clustering ------------------------------------------------------


# plot --------------------------------------------------------------------


