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

# Read file ------

tmp_join_raw <- readr::read_rds("data/socio_acc-all_2019.rds")  # source("R/0.0_join_acess-soc.R")

# initial filter
tmp_join <- data.table::copy(tmp_join_raw) %>%  
  .[pop_total > 0,] %>% 
  .[mode == "walk",] %>% 
  .[pico == 1,] %>% 
  .[decil %in% c(9,10),decil_status := "Rica"] %>% 
  .[decil %in% c(1:2),decil_status := "Pobre"]


# organize data in columns
tmp_join <- data.table::melt(tmp_join
                             ,measure.vars = list(c("cor_branca","cor_negra"
                                                    ,"cor_amarela","cor_indigena"))
                             ,variable.name = "cor"
                             ,value.name = "total_by_color")

# keep only the rich and the poor
tmp_join <- subset(tmp_join, !is.na(decil_status))

# keep only white and the black
tmp_join <- subset(tmp_join, cor %in% c('cor_branca', 'cor_negra'))

# calculate average accessibility by income and color groups
df <- tmp_join[, .(access = weighted.mean(x=CMASA45, w=total_by_color)),
               by= .(city, cor, decil_status)]

head(df)

# divide access level by reference group (poor black)
df[, index := access / access[which(cor == 'cor_negra' & decil_status=='Pobre')],
   by= .(city)]



# plot ------

# find city order
# df <- data.table::copy(tmp_join) %>%
#   .[decil_status  == "Pobre" & cor == "cor_branca"] %>%
#   .[order(index),city]
# 
# tmp_city_order
# dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
#                                      ,"for","goi","gua","mac","man","nat","poa"
#                                      ,"rec","rio","sal","sgo","slz","spo"),
#                           labels = c("Belém","Belo Horizonte","Brasília","Campinas"
#                                      ,"Campo Grande","Curitiba","Duque de Caxias"
#                                      ,"Fortaleza","Goiânia","Guarulhos","Maceió"
#                                      ,"Manaus","Natal","Porto Alegre"
#                                      ,"Recife","Rio de Janeiro","Salvador"
#                                      ,"São Gonçalo"
#                                      ,"São Luís","São Paulo"))
# 
# # add as factors
# tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
#   .[1:length(tmp_city_order),labels]
# 
# tmp_join[,city_f := factor(
#    x = city
#   ,levels = tmp_city_order
#   ,labels = tmp_city_labels
# )]
# 
# add label
df[cor == "cor_branca" & decil_status == "Rica" , label := "Alta Branca" ]
df[cor == "cor_branca" & decil_status == "Pobre", label := "Baixa Branca"]
df[cor == "cor_negra"  & decil_status == "Rica" , label := "Alta Negra"  ]
df[cor == "cor_negra"  & decil_status == "Pobre", label := "Baixa Negra" ]
# 
# # verify data for spo
# tmp_join[city == "spo"]
# 

# plot
ggplot() + 
  # add points
  geom_point(data = df
             ,aes(x = index, y =  reorder(city, index)
                  ,color = label
                  )
             ,size=2
             ,alpha = .6
             ,shape = 1
             ,stroke = 2.) +
  # add pallete and x_continuous
  # scale_colour_aop(palette = "clevel") +
  scale_x_continuous(labels = paste0(breaks = seq(1
                                                  ,max(round(df$index,0))
                                                  ,by = 9), 'x'),
                       breaks = seq(1
                                   ,max(round(df$index,0))
                                   ,by = 9) ) +
  # add vertical line
  geom_vline(xintercept = 1,linetype = "dashed")+
  # labs
  labs(x = "Acessibilidade x vezes maior do que para população negra de baixa renda"
       , y = NULL
       , color = "Grupo"
       , title = "Desigualdade de acessibilidade em relação população negra de baixa renda"
      # , subtitle = "Proporção entre população negra de baixa renda sobre\npopulação por cor/raça - renda"
      )+
  # adjust legend.position 
  theme(legend.position = c(0.875,0.35))

