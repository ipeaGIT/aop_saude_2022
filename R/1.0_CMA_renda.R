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

tmp_w3
# rbind
tmp_w <- rbind(tmp_w1,tmp_w2,tmp_w3,use.names=TRUE)

# tmp_w <- tmp_w %>% 
#   .[!is.na(CMASB30) | !is.nan(CMASB30) ] %>% 
#   .[!is.na(CMASB60) | !is.nan(CMASB60) ] %>% 
#   .[CMASB30 > 0.00] %>% 
#   .[CMASB60 > 0.00] 

tmp_w[city == "cam" & 
        mode == "walk" & 
        cor %in% c("cor_branca","cor_negra"
                   ,"media"),]

#  CMASB30 | walk ----


tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("walk")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                     )
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Média"] %>% 
  .[order(CMASB30),city]


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

CMASB30_walk_plot <- ggplot() + 
  geom_segment(data = tmp_plot[,list(min(CMASB30),max(CMASB30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = CMASB30,y = city_f
                  ,color = label),size=2.5,alpha = 1,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = CMASB30,y = city_f)
             ,size=2.5,shape = 3,alpha = 1,color = "grey")+
  # geom_point(data = tmp_plot[decil_status == "media populacao"]
  #            ,aes(x = CMASB30,y = city_f)
  #            ,size=2.5,shape = 16,alpha = 1,color = "grey")+
  scale_colour_aop(palette = "clevel")+
  labs(title = NULL
       ,subtitle = "Medida cumulativa ativa"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  #guides(shape = "none")+
  aop_style()

CMASB30_walk_plot

# #  TMISB | walk ----

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
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Branca"
                                 ,"Negra"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 
tmp_plot_min <- data.table::copy(tmp_plot)[
  ,
  {
    get_id_media <- which(label == "Média")
    list(diff_time = max(TMISB) - min(TMISB)
         ,media_time = TMISB[get_id_media])
  }
  , by = "city"]
tmp_plot <- tmp_plot[tmp_plot_min, on ="city"]
#tmp_plot[,TMISB := TMISB - media_time]
tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Alta Branca"] %>% 
  #.[order(TMISB,decreasing = FALSE),city]
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

# plot
tmi1 <- ggplot() + 
  # decis
  geom_segment(
    data = tmp_plot[decil_status != "media populacao"] %>%  
      .[,list("min" = min(TMISB),"max" = max(TMISB)),by = city_f]
    ,aes(x = min,xend = max,y = city_f,yend = city_f)
    ,color = "grey58"
    ,position = position_nudge(y = +0.0)
  )+
  geom_point(
    data = tmp_plot[decil_status != "media populacao"]
    ,aes(x = TMISB,y = city_f
         ,color = label)
    ,size=2.0,alpha = 1
    ,shape = 1,stroke = 1.5
    ,position = position_nudge(y = +0.0)
  )+
  scale_shape_manual(values = c(21))+
  scale_colour_aop(palette = "clevel")+
  labs(
    title = NULL
    #, subtitle = "Tempo mínimo"
    , color = "Renda e raça"
    , shape = "Média raça"
    , x = "Tempo mínimo (minutos)"
    , y = NULL
    , fill = "Cor - Modo de transporte")+
  aop_style_black()

tmi1

tmi2 <- ggplot() + 
  # media raca
  geom_segment(
    data = tmp_plot %>% 
      .[cor != "media" & decil_status == "media populacao"] %>% 
      .[,list(min(TMISB),max(TMISB)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
    ,color = "grey58"
    ,position = position_nudge(y = +0.0)
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = TMISB,y = city_f)
    ,size=2,shape = 3
    ,alpha = 1,color = "grey58"
    ,position = position_nudge(y = +0.0)
  )+
  geom_point(
    data = tmp_plot[cor != "media" & decil_status == "media populacao"]
    ,aes(x = TMISB,y = city_f, shape = label)
    ,size=2.0,stroke = 1.0
    ,alpha = 1,color = "black"
    ,position = position_nudge(y = +0.0)
  )+
  scale_shape_manual(values = c(21,22))+
  scale_colour_aop(palette = "clevel")+
  labs(
    title = NULL
    , subtitle = "Tempo mínimo"
    , color = "Renda e raça"
    , shape = "Média raça"
    , x = "Tempo mínimo (minutos)"
    , y = NULL
    , fill = "Cor - Modo de transporte")+
  aop_style_black()

tmi2
TMISB_walk_plot <- (tmi2 + theme(legend.position = c(0.875,0.15))) /
                      (tmi1 + theme(legend.position = c(0.875,0.225)))
TMISB_walk_plot

ggsave(filename = "figures/fig3.png",
       width = 20,height = 35,scale = 0.6,units = "cm",bg = "white",dpi = 300)
ggsave(filename = "figures/fig3.pdf",
       width = 20,height = 35,scale = 0.6,units = "cm",bg = "white",dpi = 300)

TMISB_walk_plot <- (tmi2 + theme(legend.position = c(0.875,0.15))) /
  (tmi1 + theme(legend.position = c(0.875,0.225)))
TMISB_walk_plot

ggsave(filename = "figures/TMI_walk_20-20rico1.png",
       width = 20,height = 35,scale = 0.6,units = "cm",bg = "white",dpi = 300)


# P1 | walk ----------


CMASB30_walk_plot + TMISB_walk_plot +
  plot_annotation(
    title = 'Acesso a oportunidades de saúde de baixa complexidade (2019)',
    subtitle = 'Modo caminhada',
    #caption = 'Disclaimer: None of these plots are insightful'
  )
ggsave(filename = "figures/CMASB30_TMI_walk_40-10rico.png",
       width = 34,height = 20,scale = 0.8,units = "cm",bg = "white",dpi = 300)


#  CMASB30 | bike ----


tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("bike")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                     )
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Média"] %>% 
  .[order(CMASB30),city]


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

CMASB30_bike_plot <- ggplot() + 
  geom_segment(data = tmp_plot[,list(min(CMASB30),max(CMASB30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = CMASB30,y = city_f
                  ,color = label),size=2.5,alpha = 1,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = CMASB30,y = city_f)
             ,size=2.5,shape = 3,alpha = 1,color = "grey")+
  scale_colour_aop(palette = "clevel")+
  labs(title = NULL
       ,subtitle = "Medida cumulativa ativa"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  #guides(shape = "none")+
  aop_style()

CMASB30_bike_plot

# #  TMISB | bike ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("bike")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_branca_Pobre"
                                 ,"cor_negra_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                     )
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Média"] %>% 
  .[order(TMISB),city]


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

TMISB_bike_plot <- ggplot() + 
  geom_segment(data = tmp_plot[,list(min(TMISB),max(TMISB)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = TMISB,y = city_f
                  ,color = label),size=2.5,alpha = 1,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = TMISB,y = city_f)
             ,size=2.5,shape = 3,alpha = 1,color = "grey")+
  # scale_color_manual(values = c("#b79f00","#dbcf80"
  #                              ,"red","#89CFF0","#0000FF"))+
  scale_colour_aop(palette = "clevel")+
  # facet_grid(rows  = vars(mode),scales = "free")+
  labs(#title = "Medida cumulativa ativa (2019)"
    ,title = NULL
    ,subtitle = "Tempo mínimo"
    ,color = "Renda e raça"
    ,x = "Tempo mínimo (minutos)"
    , y = NULL
    , fill = "Cor - Modo de transporte")+
  #guides(shape = "none")+
  aop_style()+
  theme(legend.position = c(0.875,0.15))


TMISB_bike_plot
ggsave(filename = "figures/TMI_bike_40-10rico.png",
       width = 17,height = 20,scale = 0.8,units = "cm")
# P2 | bike ----------


CMASB30_bike_plot + TMISB_bike_plot +
  plot_annotation(
    title = 'Acesso a oportunidades de saúde de baixa complexidade (2019)',
    subtitle = 'Modo bicicleta',
    #caption = 'Disclaimer: None of these plots are insightful'
  )
ggsave(filename = "figures/CMASB30_TMI_bike_40-10rico.png",
       width = 34,height = 20,scale = 0.8,units = "cm",bg = "white",dpi = 300)


#  CMASA30 | transit ----
pos_nudge_y <- 0 # -0.35

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  #.[city != "sal"] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("public_transport")] %>% 
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
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Branca"
                                 ,"Negra"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Alta Branca"] %>% 
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
  # renda e cor
  geom_segment(
    data = tmp_plot[,list(min(CMASA30),max(CMASA30)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  )+
  geom_point(
    data = tmp_plot[decil_status != "media populacao"]
    ,aes(x = CMASA30,y = city_f
         ,color = label)
    ,size=1.5,alpha = 1,shape = 1,stroke = 2.
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = CMASA30,y = city_f)
    ,size=2.5,shape = 3,alpha = 1,color = "grey"
  )+
  # cor
  geom_segment(
    data = tmp_plot %>% 
      .[cor != "media" & decil_status == "media populacao"] %>% 
      .[,list(min(CMASA30),max(CMASA30)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
    ,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = CMASA30,y = city_f)
    ,size=2.5,shape = 3
    ,alpha = 1,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor != "media" & decil_status == "media populacao"]
    ,aes(x = CMASA30,y = city_f, shape = label)
    ,size=2.0,stroke = 1.0
    ,alpha = 1,color = "black"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  # scales
  scale_shape_manual(values = c(21,22))+
  scale_colour_aop(palette = "clevel")+
  labs(
    title = NULL
    ,subtitle = "Medida cumulativa ativa"
    ,color = "Renda e raça"
    ,shape = "Média raça"
    ,x = "Nº de oportunidades"
    , y = NULL
    , fill = "Cor - Modo de transporte"
  )+
  aop_style_black()

CMASA30_transit_plot
# #  TMISA | transit ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[city != "sal"] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("public_transport")] %>% 
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
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Branca"
                                 ,"Negra"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Alta Branca"] %>% 
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
  # regular
  geom_segment(
    data = tmp_plot[,list(min(TMISA),max(TMISA)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
    ,color = "grey"
  )+
  geom_point(
    data = tmp_plot[decil_status != "media populacao"]
    ,aes(x = TMISA,y = city_f
         ,color = label)
    ,size=1.5,alpha = 1,shape = 1,stroke = 2.
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = TMISA,y = city_f)
    ,size=2.5,shape = 3,alpha = 1,color = "grey"
  )+
  # medias
  geom_segment(
    data = tmp_plot %>% 
      .[cor != "media" & decil_status == "media populacao"] %>% 
      .[,list(min(TMISA),max(TMISA)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
    ,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = TMISA,y = city_f)
    ,size=2.5,shape = 3
    ,alpha = 1,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor != "media" & decil_status == "media populacao"]
    ,aes(x = TMISA,y = city_f, shape = label)
    ,size=2.0,stroke = 1.0
    ,alpha = 1,color = "black"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  # scales
  scale_shape_manual(values = c(21,22))+
  scale_colour_aop(palette = "clevel")+
  labs(
    title = NULL
    ,subtitle = "Tempo mínimo"
    ,color = "Renda e raça"
    ,shape = "Média raça"
    ,x = "Tempo mínimo (minutos)"
    , y = NULL
    , fill = "Cor - Modo de transporte"
  )+
  aop_style_black()


TMISA_transit_plot

# P3 | transit ----------


# (TMISA_transit_plot +
#    theme(legend.position = "bottom")+
#    guides(color = guide_legend(title.position = "top",nrow = 2)
#           ,shape = guide_legend(title.position = "top",nrow = 2))) + 
#   (CMASA30_transit_plot +
#      theme(legend.position = "none")) +
#   plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")+
#   plot_layout(guides = "auto")
# 
# ggsave(filename = "figures/CMASA30_TMI_transit_20-20rico2.png",
#        width = 20,height = 17,scale = .9
#        ,units = "cm",bg = "white",dpi = 300)
 
  (CMASA30_transit_plot + theme(plot.margin=unit(c(0,1,-0.5,1), "cm")))  / 
  (TMISA_transit_plot ) +
     guides(color = guide_legend(title.position = "top",nrow = 2)
            ,shape = guide_legend(title.position = "top",nrow = 2)) +
  theme(legend.position = "bottom") +
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")+
  plot_layout(guides = "auto")



ggsave(filename = "figures/fig5.pdf",
       width = 20,height = 35,scale = 0.5
       ,units = "cm",bg = "white",dpi = 300)
ggsave(filename = "figures/fig5.png",
       width = 20,height = 35,scale = 0.5
       ,units = "cm",bg = "white",dpi = 300)


#  CMASA30 | car ----
pos_nudge_y <- 0 # -0.35

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[city != "sal"] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("car")] %>% 
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
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Branca"
                                 ,"Negra"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Alta Branca"] %>% 
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

# plot

pos_size_car <- 1.5
CMASA30_car_plot <- ggplot() + 
  # renda e cor
  geom_segment(
    data = tmp_plot[,list(min(CMASA30),max(CMASA30)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey"
  )+
  geom_point(
    data = tmp_plot[decil_status != "media populacao"]
    ,aes(x = CMASA30,y = city_f
         ,color = label)
    ,size=1.5,alpha = 1,shape = 1,stroke = 2.
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = CMASA30,y = city_f)
    ,size=2.5,shape = 3,alpha = 1,color = "grey"
  )+
  # cor
  geom_segment(
    data = tmp_plot %>% 
      .[cor != "media" & decil_status == "media populacao"] %>% 
      .[,list(min(CMASA30),max(CMASA30)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
    ,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = CMASA30,y = city_f)
    ,size=2.5,shape = 3
    ,alpha = 1,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor != "media" & decil_status == "media populacao"]
    ,aes(x = CMASA30,y = city_f, shape = label)
    ,size=pos_size_car,stroke = 1.0
    ,alpha = 1,color = "black"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  # scales
  scale_shape_manual(values = c(21,22))+
  scale_colour_aop(palette = "clevel")+
  labs(
    title = NULL
    ,subtitle = "Medida cumulativa ativa"
    ,color = "Renda e raça"
    ,shape = "Média raça"
    ,x = "Nº de oportunidades"
    , y = NULL
    , fill = "Cor - Modo de transporte"
  )+
  aop_style_black()

CMASA30_car_plot
# #  TMISA | car ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[city != "sal"] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("car")] %>% 
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
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Branca"
                                 ,"Negra"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Alta Branca"] %>% 
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
  # regular
  geom_segment(
    data = tmp_plot[,list(min(TMISA),max(TMISA)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
    ,color = "grey"
  )+
  geom_point(
    data = tmp_plot[decil_status != "media populacao"]
    ,aes(x = TMISA,y = city_f
         ,color = label)
    ,size=1.5,alpha = 1,shape = 1,stroke = 2.
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = TMISA,y = city_f)
    ,size=2.5,shape = 3,alpha = 1,color = "grey"
  )+
  # medias
  geom_segment(
    data = tmp_plot %>% 
      .[cor != "media" & decil_status == "media populacao"] %>% 
      .[,list(min(TMISA),max(TMISA)),by = city_f]
    ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
    ,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor == "media" & decil_status == "media populacao"]
    ,aes(x = TMISA,y = city_f)
    ,size=2.5,shape = 3
    ,alpha = 1,color = "grey"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  geom_point(
    data = tmp_plot[cor != "media" & decil_status == "media populacao"]
    ,aes(x = TMISA,y = city_f, shape = label)
    ,size=pos_size_car,stroke = 1.0
    ,alpha = 1,color = "black"
    ,position = position_nudge(y = pos_nudge_y)
  )+
  # scales
  scale_shape_manual(values = c(21,22))+
  scale_colour_aop(palette = "clevel")+
  labs(
    title = NULL
    ,subtitle = "Tempo mínimo"
    ,color = "Renda e raça"
    ,shape = "Média raça"
    ,x = "Tempo mínimo (minutos)"
    , y = NULL
    , fill = "Cor - Modo de transporte"
  )+
  aop_style_black()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title.position = "top",nrow = 2)
         ,shape = guide_legend(title.position = "top",nrow = 2))


TMISA_car_plot

# P3 | car ----------



(CMASA30_car_plot + theme(plot.margin=unit(c(0,1,-2,1), "cm"))) / 
  (TMISA_car_plot + theme(plot.margin=unit(c(-0.5,1,0,1), "cm")) )   +
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")+
  plot_layout(guides = "auto")



ggsave(filename = "figures/fig7.png",
       width = 20,height = 32,scale = 0.6
       ,units = "cm",bg = "white",dpi = 300)
ggsave(filename = "figures/fig7.pdf",
       width = 34,height = 20,scale = 0.6
       ,units = "cm",bg = "white",dpi = 300)



# P4 | car ----------


TMISA_car_plot + CMASA30_car_plot + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")
ggsave(filename = "figures/CMASA30_TMI_car_20-20rico.png",
       width = 34,height = 25,scale = 0.6,units = "cm",bg = "white",dpi = 300)


# End-----