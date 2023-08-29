# Load packages ------

rm(list=ls())
gc(reset = TRUE)

library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
library(sf)
library(patchwork)
library(extrafontdb) 
library(fontcm)
library(extrafont) 
library(showtext)
#extrafont::font_import()
#extrafont::loadfonts(device = "win")
#windowsFonts()
#windowsFont("Times New Roman")
#extrafont::fonts()

# add style
source('R/colours.R')

# Read files ------

tmp_join_raw <- readr::read_rds("data/socio_acc-all_2019.rds")  # source("R/0.0_join_acess-soc.R")
setnames(tmp_join_raw,"peak","pico")
setnames(tmp_join_raw,"sigla_muni","city")

tmp_join <- copy(tmp_join_raw)

# decil status
tmp_join[decil %in% c(9,10),decil_status := "Rica"]
tmp_join[decil %in% c(1:2),decil_status := "Pobre"]
tmp_join <- tmp_join[decil > 0]

# format wide-to-long
tmp_melt <- data.table::melt(tmp_join
                             ,measure.vars = list(c("cor_branca","cor_negra"
                                                    ,"cor_amarela","cor_indigena"))
                             ,variable.name = "cor"
                             ,value.name = "total")


tmp_melt <- tmp_melt %>%  
  .[is.infinite(TMISB) & mode == "walk",TMISB := 60] %>% 
  .[is.infinite(TMISB) & mode != "walk",TMISB := 120] %>% 
  .[is.infinite(TMISA) & mode == "walk",TMISA := 60] %>% 
  .[is.infinite(TMISA) & mode != "walk",TMISA := 120] 

# by media por cor e decil_status
tmp_w1 <- data.table::copy(tmp_melt) %>% 
  .[!is.na(decil_status),] %>% 
  .[,
    lapply(.SD,weighted.mean,total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano,cor,decil_status)
    ,.SDcols = c("CMASB30","CMASA30","TMISB","TMISA")
  ] %>% 
  .[,tabela := "media cor-raca"] 


# by media geral
tmp_w2 <- data.table::copy(tmp_melt) %>% 
  .[,lapply(.SD,weighted.mean,total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano)
    ,.SDcols =  c("CMASB30","CMASA30","TMISB","TMISA")] %>% 
  .[,decil_status := "media populacao",] %>% 
  .[,cor := "media"] %>% 
  .[,tabela := "media cor"] 

# by media por cor
tmp_w3 <- data.table::copy(tmp_melt) %>% 
  .[,lapply(.SD,weighted.mean,total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano,cor)
    ,.SDcols =  c("CMASB30","CMASA30","TMISB","TMISA")] %>% 
  .[,decil_status := "media populacao"] %>% 
  .[,tabela := "media cor"] 

# rbind
tmp_w <- rbind(tmp_w1,tmp_w2,tmp_w3,use.names=TRUE)

# tmp_w <- tmp_w %>% 
#   .[!is.na(CMASB30) | !is.nan(CMASB30) ] %>% 
#   .[!is.na(CMASB60) | !is.nan(CMASB60) ] %>% 
#   .[CMASB30 > 0.00] %>% 
#   .[CMASB60 > 0.00] 

tmp_w[city == "sal",unique(mode)] 
tmp_w[city == "sal" & 
        mode == "public_transport" & 
        cor %in% c("cor_branca","cor_negra"
                   ,"media"),]
# Figure 1) walk ----

## 1.2) TMISB | walk ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("walk")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_branca_media populacao"
                                 ,"cor_negra_media populacao"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                     )
                     ,labels = c("White individuals with low income"
                                 ,"Black individuals with low income"
                                 ,"Average"
                                 ,"White individuals (average)"
                                 ,"Black individuals (average)"
                                 ,"White individuals with high income"
                                 ,"Black individuals with high income"))] 
tmp_plot_min <- data.table::copy(tmp_plot)[
  ,
  {
    get_id_media <- which(label == "Average")
    list(diff_time = max(TMISB) - min(TMISB)
         ,media_time = TMISB[get_id_media])
  }
  , by = "city"]
tmp_plot <- tmp_plot[tmp_plot_min, on ="city"]
tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "White individuals with high income"] %>% 
  .[order(diff_time,decreasing = FALSE),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belém","Belo Horizonte","Brasília","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiania","Guarulhos","Maceió"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"São Gonçalo"
                                     ,"São Luís","São Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]

#### tmi1-----
# plot
coord_plot_leg <-  c(0.85,0.225)
tmi1 <- ggplot()+ # tracado
  geom_segment(
    data = tmp_plot[decil_status != "media populacao"
                    ,list(min(TMISB),max(TMISB)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  ) +
  # pontos renda - cor
  geom_point(
    data = tmp_plot[decil_status != "media populacao"]
    ,aes(x = TMISB
         ,y = city_f
         ,color = label
         ,size = label
         ,stroke = label
         ,shape = label)
    ,alpha = 1
  ) +
  scale_shape_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = 1
                                  ,`White individuals with high income` = 1
                                  ,`Black individuals with low income` = 0
                                  ,`Black individuals with high income` = 0
                                  ,`Average` = 3
                                  ,`White individuals (average)` = 1
                                  ,`Black individuals (average)` = 0))+
  scale_size_manual(name = "Color/race and Income"
                    ,values = c( `White individuals with low income` = 2.0
                                 ,`White individuals with high income` = 2.0
                                 ,`Black individuals with low income` = 2.0
                                 ,`Black individuals with high income` = 2.0
                                 ,`Average` = 2
                                 ,`White individuals (average)` = 2.001
                                 ,`Black individuals (average)` = 2.001))+
  scale_discrete_manual(aesthetic = "stroke"
                        ,name = "Color/race and Income"
                        ,values = c( `White individuals with low income` = 1.00
                                     ,`White individuals with high income` = 1.00
                                     ,`Black individuals with low income` = 1.00
                                     ,`Black individuals with high income` = 1.00
                                     ,`Average` = 1.001
                                     ,`White individuals (average)` = 1.500
                                     ,`Black individuals (average)` = 1.500)
  )+
  scale_color_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = "#C29365"
                                    ,`White individuals with high income` = "#6A9BB3"
                                    ,`Black individuals with low income` = "#620C1A"
                                    ,`Black individuals with high income` = "#111F4F"
                                    ,`Average` = "grey"
                                    ,`White individuals (average)` = "black"
                                    ,`Black individuals (average)` = "black"))+
  labs(
    title = NULL
    ,subtitle = "Minimum Travel Time"
    ,x = "Minimum Travel Time (minutes)"
    , y = NULL
  )+
  aop_style_black()+
  theme(text = element_text(family = "Times New Roman")
        ,legend.position = coord_plot_leg
        ,legend.title.align = 0)+
  guides(color  = guide_legend(ncol = 1,title.position = "top"),
         size   = guide_legend(ncol = 1,title.position = "top"),
         stroke = guide_legend(ncol = 1,title.position = "top"),
         shape  = guide_legend(ncol = 1,title.position = "top")
  )

tmi1

#### tmi2-----
tmi2 <- ggplot()+ # tracado
  geom_segment(
    data = tmp_plot[decil_status == "media populacao"
                    ,list(min(TMISB),max(TMISB)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  ) +
  # pontos renda - cor
  geom_point(
    data = tmp_plot[decil_status == "media populacao"]
    ,aes(x = TMISB
         ,y = city_f
         ,color = label
         ,size = label
         ,stroke = label
         ,shape = label)
    ,alpha = 1
  ) +
  scale_shape_manual(name = "Color/race"
                     ,values = c( `White individuals with low income` = 1
                                  ,`White individuals with high income` = 1
                                  ,`Black individuals with low income` = 0
                                  ,`Black individuals with high income` = 0
                                  ,`Average` = 3
                                  ,`White individuals (average)` = 1
                                  ,`Black individuals (average)` = 0))+
  scale_size_manual(name = "Color/race"
                    ,values = c( `White individuals with low income` = 2.0
                                 ,`White individuals with high income` = 2.0
                                 ,`Black individuals with low income` = 2.0
                                 ,`Black individuals with high income` = 2.0
                                 ,`Average` = 2
                                 ,`White individuals (average)` = 2.001
                                 ,`Black individuals (average)` = 2.001))+
  scale_discrete_manual(aesthetic = "stroke"
                        ,name = "Color/race"
                        ,values = c( `White individuals with low income` = 1.00
                                     ,`White individuals with high income` = 1.00
                                     ,`Black individuals with low income` = 1.00
                                     ,`Black individuals with high income` = 1.00
                                     ,`Average` = 1.001
                                     ,`White individuals (average)` = 1.500
                                     ,`Black individuals (average)` = 1.500)
  )+
  scale_color_manual(name = "Color/race"
                     ,values = c( `White individuals with low income` = "#C29365"
                                    ,`White individuals with high income` = "#6A9BB3"
                                    ,`Black individuals with low income` = "#620C1A"
                                    ,`Black individuals with high income` = "#111F4F"
                                    ,`Average` = "grey"
                                    ,`White individuals (average)` = "black"
                                    ,`Black individuals (average)` = "black"))+
  labs(
    title = NULL
    ,subtitle = "Minimum Travel Time"
    ,x = "Minimum Travel Time (minutes)"
    , y = NULL
  )+
  aop_style_black()+
  theme(text = element_text(family = "Times New Roman")
        ,legend.position = coord_plot_leg
        ,legend.title.align = 0)+
  guides(color  = guide_legend(ncol = 1,title.position = "top"),
         size   = guide_legend(ncol = 1,title.position = "top"),
         stroke = guide_legend(ncol = 1,title.position = "top"),
         shape  = guide_legend(ncol = 1,title.position = "top")
  )

## 1.3) P1 | walk ----------

TMISB_walk_plot <- (tmi2 + theme(plot.margin = unit(c(0,2,0,0),"cm"))) / 
  (tmi1 + theme(plot.margin = unit(c(0,2,0,0),"cm")))  + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")

ggsave(filename = "figures/Fig1.png",
       width = 23,height = 35,scale = 0.6,units = "cm",bg = "white",dpi = 300)

#  Figure 2) transit ----
##  2.1) CMASA30 | transit ----
pos_nudge_y <- 0 # -0.35

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[,cor_f := factor(cor
                     ,levels = c("cor_branca"
                                 ,"cor_negra"
                                 ,"media")
                     ,labels = c("White"
                                 ,"Black"
                                 ,"Media"))] %>% 
  .[mode %in% c("public_transport")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                                 ,"cor_branca_media populacao"
                                 ,"cor_negra_media populacao"
                                 ,"media_media populacao"
                                 
                     )
                     ,labels = c("White individuals with low income"
                                 ,"Black individuals with low income"
                                 ,"White individuals with high income"
                                 ,"Black individuals with high income"
                                 ,"White individuals (average)"
                                 ,"Black individuals (average)"
                                 ,"Mean"
                     ))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "White individuals with high income"] %>% 
  .[order(CMASA30,decreasing = TRUE),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belém","Belo Horizonte","Brasília","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiânia","Guarulhos","Maceió"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"São Gonçalo"
                                     ,"São Luís","São Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]

# plot
CMASA30_transit_plot <- ggplot() + 
  # tracado
  geom_segment(
    data = tmp_plot[,list(min(CMASA30),max(CMASA30)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  ) +
  # pontos renda - cor
  geom_point(
    data = tmp_plot
    ,aes(x = CMASA30
         ,y = city_f
         ,color = label
         ,size = label
         ,stroke = label
         ,shape = label)
    ,alpha = 1
  ) +
  scale_shape_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = 1
                                  ,`White individuals with high income` = 1
                                  ,`Black individuals with low income` = 0
                                  ,`Black individuals with high income` = 0
                                  ,`Mean` = 3
                                  ,`White individuals (average)` = 1
                                  ,`Black individuals (average)` = 0))+
  scale_size_manual(name = "Color/race and Income"
                    ,values = c( `White individuals with low income` = 2.0
                                 ,`White individuals with high income` = 2.0
                                 ,`Black individuals with low income` = 2.0
                                 ,`Black individuals with high income` = 2.0
                                 ,`Mean` = 2
                                 ,`White individuals (average)` = 2.001
                                 ,`Black individuals (average)` = 2.001))+
  scale_discrete_manual(aesthetic = "stroke"
                        ,name = "Color/race and Income"
                        ,values = c( `White individuals with low income` = 1.00
                                     ,`White individuals with high income` = 1.00
                                     ,`Black individuals with low income` = 1.00
                                     ,`Black individuals with high income` = 1.00
                                     ,`Mean` = 1.001
                                     ,`White individuals (average)` = 1.500
                                     ,`Black individuals (average)` = 1.500)
  )+
  scale_color_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = "#C29365"
                                    ,`White individuals with high income` = "#6A9BB3"
                                    ,`Black individuals with low income` = "#620C1A"
                                    ,`Black individuals with high income` = "#111F4F"
                                    ,`Mean` = "grey"
                                    ,`White individuals (average)` = "black"
                                    ,`Black individuals (average)` = "black"))+
  labs(
    title = NULL
    ,subtitle = "Cumulative Accessibility Measure"
    ,x = "Number of opportunities"
    , y = NULL
  )+
  aop_style_black()+
  theme(text = element_text(family = "Times New Roman")
        ,legend.position = "bottom"
        ,legend.title.align = 0)+
  guides(color  = guide_legend(ncol = 4),
         size   = guide_legend(ncol = 4),
         stroke = guide_legend(ncol = 4),
         shape  = guide_legend(ncol = 4)
  )


## 2.2)  TMISA | transit ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("public_transport")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                                 ,"cor_branca_media populacao"
                                 ,"cor_negra_media populacao"
                                 ,"media_media populacao"
                     )
                     ,labels = c("White individuals with low income"
                                 ,"Black individuals with low income"
                                 ,"White individuals with high income"
                                 ,"Black individuals with high income"
                                 ,"White individuals (average)"
                                 ,"Black individuals (average)"
                                 ,"Mean"
                     ))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "White individuals with high income"] %>% 
  .[order(TMISA,decreasing = FALSE),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belém","Belo Horizonte","Brasília","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiânia","Guarulhos","Maceió"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"São Gonçalo"
                                     ,"São Luís","São Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]

# plot

TMISA_transit_plot <- ggplot() + 
  # tracado
  geom_segment(
    data = tmp_plot[,list(min(TMISA),max(TMISA)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  ) +
  # pontos renda - cor
  geom_point(
    data = tmp_plot
    ,aes(x = TMISA
         ,y = city_f
         ,color = label
         ,size = label
         ,stroke = label
         ,shape = label)
    ,alpha = 1
  ) +
  scale_shape_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = 1
                                  ,`White individuals with high income` = 1
                                  ,`Black individuals with low income` = 0
                                  ,`Black individuals with high income` = 0
                                  ,`Mean` = 3
                                  ,`White individuals (average)` = 1
                                  ,`Black individuals (average)` = 0))+
  scale_size_manual(name = "Color/race and Income"
                    ,values = c( `White individuals with low income` = 2.0
                                 ,`White individuals with high income` = 2.0
                                 ,`Black individuals with low income` = 2.0
                                 ,`Black individuals with high income` = 2.0
                                 ,`Mean` = 2
                                 ,`White individuals (average)` = 2.001
                                 ,`Black individuals (average)` = 2.001))+
  scale_discrete_manual(aesthetic = "stroke"
                        ,name = "Color/race and Income"
                        ,values = c( `White individuals with low income` = 1.00
                                     ,`White individuals with high income` = 1.00
                                     ,`Black individuals with low income` = 1.00
                                     ,`Black individuals with high income` = 1.00
                                     ,`Mean` = 1.001
                                     ,`White individuals (average)` = 1.500
                                     ,`Black individuals (average)` = 1.500)
  )+
  scale_color_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = "#C29365"
                                    ,`White individuals with high income` = "#6A9BB3"
                                    ,`Black individuals with low income` = "#620C1A"
                                    ,`Black individuals with high income` = "#111F4F"
                                    ,`Mean` = "grey"
                                    ,`White individuals (average)` = "black"
                                    ,`Black individuals (average)` = "black"))+
  labs(
    title = NULL
    ,subtitle = "Minimum Travel Time"
    ,x = "Minimum Travel Time (minutes)"
    , y = NULL
  )+
  aop_style_black()+
  theme(text = element_text(family = "Times New Roman")
        ,legend.position = "bottom"
        ,legend.title.align = 0)+
  guides(color  = guide_legend(ncol = 2,title.position = "top"),
         size   = guide_legend(ncol = 2,title.position = "top"),
         stroke = guide_legend(ncol = 2,title.position = "top"),
         shape  = guide_legend(ncol = 2,title.position = "top")
  )


## 2.3) P3 | transit ----------

(CMASA30_transit_plot + theme(legend.position = "none"))  / 
  (TMISA_transit_plot ) +
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")

ggsave(filename = "figures/Fig2.png",
       width = 24,height = 35,scale = 0.5
       ,units = "cm",bg = "white",dpi = 300)


#  Figure 4) car ----
## 4.1)  CMASA30 | car ----
pos_nudge_y <- 0 # -0.35

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  #.[city != "sal"] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("car")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[!(label %in% c("cor_branca_media populacao"
                   ,"cor_negra_media populacao")),] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                                 #,"cor_branca_media populacao"
                                 #,"cor_negra_media populacao"
                                 ,"media_media populacao"
                     )
                     ,labels = c("White individuals with low income"
                                 ,"Black individuals with low income"
                                 ,"White individuals with high income"
                                 ,"Black individuals with high income"
                                 #,"White individuals (average)"
                                 #,"Black individuals (average)"
                                 ,"Mean"
                     ))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "White individuals with high income"] %>% 
  .[order(CMASA30,decreasing = FALSE),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belém","Belo Horizonte","Brasília","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiânia","Guarulhos","Maceió"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"São Gonçalo"
                                     ,"São Luís","São Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]

### plot 1 - high CMA ----

pos_size_car <- 1.5

tmp_plot1 <- tmp_plot[,list(min(CMASA30),max(CMASA30)),by = .(city,city_f)]
tmp_plot1 <- tmp_plot1[city %in% tmp_city_order[11:20],]
tmp_cities <- tmp_plot1$city

CMASA30_car_plot_p1 <- ggplot() + 
  # tracado
  geom_segment(
    data = tmp_plot1
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  ) +
  # pontos renda - cor
  geom_point(
    data = tmp_plot[city %in% tmp_cities,]#[decil_status != "media"]
    ,aes(x = CMASA30
         ,y = city_f
         ,color = label
         ,size = label
         ,stroke = label
         ,shape = label)
    ,alpha = 1
  ) +
  scale_shape_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = 1
                                  ,`White individuals with high income` = 1
                                  ,`Black individuals with low income` = 0
                                  ,`Black individuals with high income` = 0
                                  ,`Mean` = 3
                                 #,`White individuals (average)` = 1
                                 #,`Black individuals (average)` = 0
                                  ))+
  scale_size_manual(name = "Color/race and Income"
                    ,values = c( `White individuals with low income` = 2.0
                                 ,`White individuals with high income` = 2.0
                                 ,`Black individuals with low income` = 2.0
                                 ,`Black individuals with high income` = 2.0
                                 ,`Mean` = 2
                                 #,`White individuals (average)` = 2.001
                                 #,`Black individuals (average)` = 2.001
                                 ))+
  scale_discrete_manual(aesthetic = "stroke"
                        ,name = "Color/race and Income"
                        ,values = c( `White individuals with low income` = 1.00
                                     ,`White individuals with high income` = 1.00
                                     ,`Black individuals with low income` = 1.00
                                     ,`Black individuals with high income` = 1.00
                                     ,`Mean` = 1.001
                                     #,`White individuals (average)` = 1.500
                                     #,`Black individuals (average)` = 1.500
                                     )
  )+
  scale_color_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = "#C29365"
                                    ,`White individuals with high income` = "#6A9BB3"
                                    ,`Black individuals with low income` = "#620C1A"
                                    ,`Black individuals with high income` = "#111F4F"
                                    ,`Mean` = "grey"
                                    #,`White individuals (average)` = "black"
                                    #,`Black individuals (average)` = "black"
                                    ))+
  labs(
    title = "Cumulative Accessibility Measure"
    ,subtitle = NULL
    ,x = "Number of opportunities"
    , y = NULL
  )+
  aop_style_black()+
  theme(text = element_text(family = "Times New Roman")
        ,legend.position = "bottom"
        ,legend.title.align = 0)+
  guides(color  = guide_legend(ncol = 2,title.position = "top"),
         size   = guide_legend(ncol = 2,title.position = "top"),
         stroke = guide_legend(ncol = 2,title.position = "top"),
         shape  = guide_legend(ncol = 2,title.position = "top")
  )

CMASA30_car_plot_p1

### plot 2 - low CMA----

pos_size_car <- 1.5
tmp_plot1 <- tmp_plot[,list(min(CMASA30),max(CMASA30)),by = .(city,city_f)]
tmp_plot1 <- tmp_plot1[city %in% tmp_city_order[1:10],]
tmp_cities <- tmp_plot1$city

CMASA30_car_plot_p2 <- ggplot() + 
  # tracado
  geom_segment(
    data = tmp_plot1
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  ) +
  # pontos renda - cor
  geom_point(
    data = tmp_plot[city %in% tmp_cities,]#[decil_status != "media"]
    ,aes(x = CMASA30
         ,y = city_f
         ,color = label
         ,size = label
         ,stroke = label
         ,shape = label)
    ,alpha = 1
  ) +
  scale_shape_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = 1
                                  ,`White individuals with high income` = 1
                                  ,`Black individuals with low income` = 0
                                  ,`Black individuals with high income` = 0
                                  ,`Mean` = 3
                                  ,`White individuals (average)` = 1
                                  ,`Black individuals (average)` = 0))+
  scale_size_manual(name = "Color/race and Income"
                    ,values = c( `White individuals with low income` = 2.0
                                 ,`White individuals with high income` = 2.0
                                 ,`Black individuals with low income` = 2.0
                                 ,`Black individuals with high income` = 2.0
                                 ,`Mean` = 2
                                 ,`White individuals (average)` = 2.001
                                 ,`Black individuals (average)` = 2.001))+
  scale_discrete_manual(aesthetic = "stroke"
                        ,name = "Color/race and Income"
                        ,values = c( `White individuals with low income` = 1.00
                                     ,`White individuals with high income` = 1.00
                                     ,`Black individuals with low income` = 1.00
                                     ,`Black individuals with high income` = 1.00
                                     ,`Mean` = 1.001
                                     ,`White individuals (average)` = 1.500
                                     ,`Black individuals (average)` = 1.500)
  )+
  scale_color_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = "#C29365"
                                    ,`White individuals with high income` = "#6A9BB3"
                                    ,`Black individuals with low income` = "#620C1A"
                                    ,`Black individuals with high income` = "#111F4F"
                                    ,`Mean` = "grey"
                                    ,`White individuals (average)` = "black"
                                    ,`Black individuals (average)` = "black"))+
  labs(
    title = NULL
    ,subtitle = NULL
    ,x = "Number of opportunities"
    , y = NULL
  )+
  aop_style_black()+
  theme(text = element_text(family = "Times New Roman")
        ,legend.position = "bottom"
        ,legend.title.align = 0)+
  guides(color  = guide_legend(ncol = 2,title.position = "top"),
         size   = guide_legend(ncol = 2,title.position = "top"),
         stroke = guide_legend(ncol = 2,title.position = "top"),
         shape  = guide_legend(ncol = 2,title.position = "top")
  )

CMASA30_car_plot_p2
## 4.2)  TMISA | car ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  #.[city != "sal"] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("car")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[!(label %in% c("cor_branca_media populacao"
                 ,"cor_negra_media populacao")),] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"  
                                 ,"cor_branca_media populacao"
                                 ,"cor_negra_media populacao"
                                 ,"media_media populacao"
                                 
                                 
                     )
                     ,labels = c("White individuals with low income"
                                 ,"Black individuals with low income"
                                 ,"White individuals with high income"
                                 ,"Black individuals with high income"
                                 ,"White individuals (average)"
                                 ,"Black individuals (average)"
                                 ,"Mean"
                     ))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "White individuals with high income"] %>% 
  .[order(TMISA,decreasing = FALSE),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belém","Belo Horizonte","Brasília","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiânia","Guarulhos","Maceió"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"São Gonçalo"
                                     ,"São Luís","São Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]

# plot

TMISA_car_plot <- ggplot() + 
  # tracado
  geom_segment(
    data = tmp_plot[,list(min(TMISA),max(TMISA)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  ) +
  # pontos renda - cor
  geom_point(
    data = tmp_plot#[decil_status != "media"]
    ,aes(x = TMISA
         ,y = city_f
         ,color = label
         ,size = label
         ,stroke = label
         ,shape = label)
    ,alpha = 1
  ) +
  scale_shape_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = 1
                                  ,`White individuals with high income` = 1
                                  ,`Black individuals with low income` = 0
                                  ,`Black individuals with high income` = 0
                                  ,`Mean` = 3
                                  ,`White individuals (average)` = 1
                                  ,`Black individuals (average)` = 0))+
  scale_size_manual(name = "Color/race and Income"
                    ,values = c( `White individuals with low income` = 2.0
                                 ,`White individuals with high income` = 2.0
                                 ,`Black individuals with low income` = 2.0
                                 ,`Black individuals with high income` = 2.0
                                 ,`Mean` = 2
                                 ,`White individuals (average)` = 2.001
                                 ,`Black individuals (average)` = 2.001))+
  scale_discrete_manual(aesthetic = "stroke"
                        ,name = "Color/race and Income"
                        ,values = c( `White individuals with low income` = 1.00
                                     ,`White individuals with high income` = 1.00
                                     ,`Black individuals with low income` = 1.00
                                     ,`Black individuals with high income` = 1.00
                                     ,`Mean` = 1.001
                                     ,`White individuals (average)` = 1.500
                                     ,`Black individuals (average)` = 1.500)
  )+
  scale_color_manual(name = "Color/race and Income"
                     ,values = c( `White individuals with low income` = "#C29365"
                                    ,`White individuals with high income` = "#6A9BB3"
                                    ,`Black individuals with low income` = "#620C1A"
                                    ,`Black individuals with high income` = "#111F4F"
                                    ,`Mean` = "grey"
                                    ,`White individuals (average)` = "black"
                                    ,`Black individuals (average)` = "black"))+
  labs(
    subtitle = NULL
    , color = "Color/race and Income"
    , title = "Minimum Travel Time"
    , x = "Minimum Travel Time (minutes)"
    , y = NULL
  )+
  aop_style_black()+
  theme(text = element_text(family = "Times New Roman")
        ,legend.position = "bottom"
        ,legend.title.align = 0)+
  guides(color  = guide_legend(ncol = 2,title.position = "top"),
         size   = guide_legend(ncol = 2,title.position = "top"),
         stroke = guide_legend(ncol = 2,title.position = "top"),
         shape  = guide_legend(ncol = 2,title.position = "top")
  )

TMISA_car_plot

## 4.3) P3 | car ----------


CMASA30_car_plot <- (CMASA30_car_plot_p1+ 
                       labs(subtitle = "10 highest CMA")+
                       theme(legend.position = "none")) |
  (CMASA30_car_plot_p2 
   + labs(subtitle = "10 lowest CMA",title=NULL) 
   + theme(legend.position = "none"))+
  plot_annotation(tag_levels = 'A',tag_prefix = "(",tag_suffix = ")")
CMASA30_car_plot

p3_car <- 
  (CMASA30_car_plot + theme(legend.position = "none")) / 
  (TMISA_car_plot + theme(plot.margin=unit(c(-0.5,1,0,1), "cm")) )   +
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")") + 
  plot_layout(widths = 3, heights = unit(c(4,6), c('cm')))
#p3_car

ggsave(filename = "figures/Fig4.png",
       width = 30,height = 36,scale = 0.5
       ,units = "cm",bg = "white",dpi = 300)




# End-----