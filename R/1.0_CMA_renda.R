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

tmp_join_bus_raw <- readr::read_rds("data/socio_acc-all_2019.rds") 
tmp_join_bus <- data.table::copy(tmp_join_bus_raw)

tmp_join_car_raw <- readr::read_rds("data/socio_acc-car_2019.rds") 
tmp_join_car <- data.table::copy(tmp_join_car_raw)

tmp_join <- list(tmp_join_bus,tmp_join_car) %>% 
  data.table::rbindlist(use.names = TRUE,fill = TRUE)

# decil status
tmp_join[decil %in% c(9,10),decil_status := "Rica"]
tmp_join[decil %in% c(1:2),decil_status := "Pobre"]
# tmp_join[decil %in% c(10),decil_status := "Rica"]
# tmp_join[decil %in% c(1:4),decil_status := "Pobre"]
tmp_join <- tmp_join[!is.na(decil_status)]

# format wide-to-long
tmp_melt <- data.table::melt(tmp_join
                             ,measure.vars = list(c("cor_branca","cor_negra"
                                                    ,"cor_amarela","cor_indigena"))
                             ,variable.name = "cor"
                             ,value.name = "total")

tmp_melt <- tmp_melt %>% 
  .[!is.na(total) | !is.nan(total) | total != 0,] %>% 
  .[!is.na(mode),] %>% 
  .[!is.na(pico),] %>% 
  .[pop_total > 0,] %>% 
  .[is.infinite(TMISB),TMISB := NA] %>% 
  .[is.infinite(TMISA),TMISA := NA] %>% 
  .[is.nan(CMASB30), CMASB30 := NA] %>% 
  .[is.nan(CMASA30), CMASA30 := NA] %>% 
  .[is.nan(CMASB60), CMASB60 := NA] %>% 
  .[is.nan(CMASA60), CMASA60 := NA]

# by (city,mode,pico,ano,cor)
tmp_w1 <- data.table::copy(tmp_melt) %>% 
  .[,
    lapply(.SD,weighted.mean,total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano,cor,decil_status)
    ,.SDcols = c("CMASB30","CMASA30"
                 ,"CMASB60","CMASA60","TMISB","TMISA")
  ]


# by (city,mode,pico,ano)
tmp_w2 <- data.table::copy(tmp_melt) %>% 
  .[,lapply(.SD,weighted.mean,total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano)
    ,.SDcols =  c("CMASB30","CMASA30"
                  ,"CMASB60","CMASA60","TMISB","TMISA")] %>% 
  .[,decil_status := "media populacao",] %>% 
  .[,cor := "media"]

# rbind
tmp_w <- rbind(tmp_w1,tmp_w2,use.names=TRUE)

tmp_w <- tmp_w %>% 
  .[!is.na(CMASB30) | !is.nan(CMASB30) ] %>% 
  .[!is.na(CMASB60) | !is.nan(CMASB60) ] %>% 
  .[CMASB30 > 0.00] %>% 
  .[CMASB60 > 0.00] 


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
                                 ,"cor_branca_Rica"
                                 ,"cor_negra_Rica"
                     )
                     ,labels = c("Baixa Branca"
                                 ,"Baixa Negra"
                                 ,"Média"
                                 ,"Alta Branca"
                                 ,"Alta Negra"))] 


tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Alta Branca"] %>% 
  .[order(TMISB),city]


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

TMISB_walk_plot <- ggplot() + 
  geom_segment(data = tmp_plot[,list(min(TMISB),max(TMISB)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
               ,color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = TMISB,y = city_f
                  ,color = label)
             ,size=2.5,alpha = 1
             ,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = TMISB,y = city_f)
             ,size=2.5,shape = 3
             ,alpha = 1,color = "grey")+
  scale_colour_aop(palette = "clevel")+
  labs(
    title = NULL
    , subtitle = "Tempo mínimo"
    , color = "Renda e raça"
    , x = "Tempo mínimo (minutos)"
    , y = NULL
    , fill = "Cor - Modo de transporte")+
  aop_style()+
  theme(legend.position = c(0.875,0.15))


TMISB_walk_plot
ggsave(filename = "figures/TMI_walk_20-20rico.png",
       width = 17,height = 20,scale = 0.8,units = "cm",bg = "white",dpi = 300)

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


tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("transit")] %>% 
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
  .[label == "Alta Branca"] %>% 
  .[order(CMASA30),city]


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
  geom_segment(data = tmp_plot[,list(min(CMASA30),max(CMASA30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = CMASA30,y = city_f
                  ,color = label),size=2.5,alpha = 1,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = CMASA30,y = city_f)
             ,size=2.5,shape = 3,alpha = 1,color = "grey")+
  scale_colour_aop(palette = "clevel")+
  labs(title = NULL
       ,subtitle = "Medida cumulativa ativa"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  aop_style()+
  theme(legend.position = c(0.875,0.2))

CMASA30_transit_plot

# #  TMISA | transit ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("transit")] %>% 
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
  .[label == "Alta Branca"] %>% 
  .[order(TMISA),city]


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
  geom_segment(data = tmp_plot[,list(min(TMISA),max(TMISA)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f)
               ,color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = TMISA,y = city_f
                  ,color = label),size=2.5,alpha = 1,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = TMISA,y = city_f)
             ,size=2.5,shape = 3,alpha = 1,color = "grey")+
  scale_colour_aop(palette = "clevel")+
  # facet_grid(rows  = vars(mode),scales = "free")+
  labs(#title = "Medida cumulativa ativa (2019)"
    title = NULL
    ,subtitle = "Tempo mínimo"
    ,color = "Renda e raça"
    ,x = "Tempo mínimo (minutos)"
    , y = NULL
    , fill = "Cor - Modo de transporte")+
  #guides(shape = "none")+
  aop_style()


TMISA_transit_plot

# P3 | transit ----------


TMISA_transit_plot + CMASA30_transit_plot +
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")
ggsave(filename = "figures/CMASA30_TMI_transit_20-20rico.png",
       width = 34,height = 20,scale = 0.6
       ,units = "cm",bg = "white",dpi = 300)


# #  TMISA | car ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("car")] %>% 
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
  .[label == "Alta Branca"] %>% 
  .[order(TMISA),city]


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
  geom_segment(data = tmp_plot[,list(min(TMISA),max(TMISA)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = TMISA,y = city_f
                  ,color = label),size=2.5,alpha = 1,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = TMISA,y = city_f)
             ,size=2.5,shape = 3,alpha = 1,color = "grey")+
  scale_colour_aop(palette = "clevel")+
  labs(title = NULL
       ,subtitle = "Tempo mínimo"
       ,color = "Renda e raça"
       ,x = "Tempo mínimo (minutos)"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  aop_style()

#  CMASB30 | car ----


tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico  == 1] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("car")] %>% 
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
  .[label == "Alta Branca"] %>% 
  .[order(CMASA30),city]


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

CMASA30_car_plot <- ggplot() + 
  geom_segment(data = tmp_plot[,list(min(CMASA30),max(CMASA30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(data = tmp_plot[decil_status != "media populacao"]
             ,aes(x = CMASA30,y = city_f
                  ,color = label),size=2.5,alpha = 1,shape = 1,stroke = 2.)+
  geom_point(data = tmp_plot[decil_status == "media populacao"]
             ,aes(x = CMASA30,y = city_f)
             ,size=2.5,shape = 3,alpha = 1,color = "grey")+
  scale_colour_aop(palette = "clevel")+
  labs(title = NULL
       ,subtitle = "Medida cumulativa ativa"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  aop_style()+
  theme(legend.position = c(0.835,0.175))


# P4 | car ----------


TMISA_car_plot + CMASA30_car_plot + 
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")
ggsave(filename = "figures/CMASA30_TMI_car_20-20rico.png",
       width = 34,height = 25,scale = 0.6,units = "cm",bg = "white",dpi = 300)


# End-----