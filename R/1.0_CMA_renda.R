


# load packages ------

rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
library(sf)
library(patchwork)

# read files ------

tmp_join_bus_raw <- readr::read_rds("data/socio_acc-all_2019.rds") 
tmp_join_bus <- data.table::copy(tmp_join_bus_raw)

tmp_join_car_raw <- readr::read_rds("data/socio_acc-car_2019.rds") 
tmp_join_car <- data.table::copy(tmp_join_car_raw)

tmp_join <- list(tmp_join_bus,tmp_join_car) %>% 
  data.table::rbindlist(use.names = TRUE,fill = TRUE)

# decil status
tmp_join[decil %in% c(9,10),decil_status := "Rica"]
tmp_join[decil %in% c(1:2),decil_status := "Pobre"]
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
  .[is.nan(CMASA30), CMASA30 := NA]

# by (city,mode,pico,ano,cor)
tmp_w1 <- data.table::copy(tmp_melt) %>% 
  .[,lapply(.SD,weighted.mean,total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano,cor,decil_status)
    ,.SDcols = c("CMASB30","CMASA30","TMISB","TMISA")] %>% 
  .[decil_status   != "-"]


# by (city,mode,pico,ano)
tmp_w2 <- data.table::copy(tmp_melt) %>% 
  .[,lapply(.SD,weighted.mean,total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano)
    ,.SDcols = c("CMASB30","CMASA30","TMISB","TMISA")] %>% 
  .[,decil_status := "media populacao",] %>% 
  .[,cor := "media"]

# rbind
tmp_w <- rbind(tmp_w1,tmp_w2,use.names=TRUE)

tmp_w <- tmp_w %>% 
  .[!is.na(CMASB30) | !is.nan(CMASB30) ] %>% 
  .[CMASB30 > 0.00]

#  PLOT 1 = CMASB30 | transporte ativo ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("walk")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_negra_Pobre"
                                 ,"cor_branca_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_negra_Rica"
                                 ,"cor_branca_Rica")
                     ,labels = c("Baixa Negra"
                                 ,"Baixa Branca"
                                 ,"Média"
                                 ,"Alta Negra"
                                 ,"Alta Branca"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Média"] %>% 
  .[order(CMASB30),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belem","Belo Horizonte","Brasilia","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiania","Guarulhos","Maceio"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"Sao Goncalo"
                                     ,"Sao Luis","Sao Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]


ggplot(tmp_plot ) + 
  geom_segment(data = tmp_plot[,list(min(CMASB30),max(CMASB30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(aes(x = CMASB30,y = city_f
                 ,color = label),size=2.5,shape = 1,stroke = 2.)+
  scale_color_manual(values = c("#b79f00","#dbcf80"
                               ,"red","#89CFF0","#0000FF"))+
 # facet_grid(rows  = vars(mode),scales = "free")+
  labs(title = "Medida cumulativa ativa (2019)"
       ,subtitle = "Oportunidades de sáude de baixa complexidade acessíveis em até 30 minutos a pé"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()

ggsave(filename = "figures/CMASB30_walk.png",
       width = 24,height = 20,scale = 0.8,units = "cm")

#  PLOT 2 = CMASB30 | bike ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("bike")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_negra_Pobre"
                                 ,"cor_branca_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_negra_Rica"
                                 ,"cor_branca_Rica")
                     ,labels = c("Baixa Negra"
                                 ,"Baixa Branca"
                                 ,"Média"
                                 ,"Alta Negra"
                                 ,"Alta Branca"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Média"] %>% 
  .[order(CMASB30),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belem","Belo Horizonte","Brasilia","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiania","Guarulhos","Maceio"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"Sao Goncalo"
                                     ,"Sao Luis","Sao Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]


ggplot(tmp_plot ) + 
  geom_segment(data = tmp_plot[,list(min(CMASB30),max(CMASB30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(aes(x = CMASB30,y = city_f
                 ,color = label),size=2.5,shape = 1,stroke = 2.)+
  scale_color_manual(values = c("#b79f00","#dbcf80"
                                ,"red","#89CFF0","#0000FF"))+
  # facet_grid(rows  = vars(mode),scales = "free")+
  labs(title = "Medida cumulativa ativa (2019)"
       ,subtitle = "Oportunidades de sáude de baixa complexidade acessíveis em até 30 minutos por bicicleta"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()

ggsave(filename = "figures/CMASB30_bike.png",
       width = 24,height = 20,scale = 0.8,units = "cm")

#  PLOT 3 = CMASA30 | car ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico == 1 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("car")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_negra_Pobre"
                                 ,"cor_branca_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_negra_Rica"
                                 ,"cor_branca_Rica")
                     ,labels = c("Baixa Negra"
                                 ,"Baixa Branca"
                                 ,"Média"
                                 ,"Alta Negra"
                                 ,"Alta Branca"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Média"] %>% 
  .[pico == 1 ] %>% 
  .[order(CMASA30),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belem","Belo Horizonte","Brasilia","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiania","Guarulhos","Maceio"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"Sao Goncalo"
                                     ,"Sao Luis","Sao Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]


ggplot(tmp_plot ) + 
  geom_segment(data = tmp_plot[,list(min(CMASA30),max(CMASA30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(aes(x = CMASA30,y = city_f
                 ,color = label),size=2.5,shape = 1,stroke = 2.)+
  scale_color_manual(values = c("#b79f00","#dbcf80"
                                ,"red","#89CFF0","#0000FF"))+
  # facet_grid(rows  = vars(mode),scales = "free")+
  labs(title = "Medida cumulativa ativa (2019)"
       ,subtitle = "Oportunidades de sáude de alta complexidade acessíveis em até 30 minutos por \nautomóvel"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()

ggsave(filename = "figures/CMASA30_car.png",
       width = 24,height = 20,scale = 0.8,units = "cm")

#  PLOT 4 = CMASA30 | bus ----

tmp_plot <- data.table::copy(tmp_w) %>% 
  .[ano == 2019 ] %>% 
  .[pico == 1 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra","media")] %>% 
  .[mode %in% c("transit")] %>% 
  .[,label := paste0(cor,"_",decil_status)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_negra_Pobre"
                                 ,"cor_branca_Pobre"
                                 ,"media_media populacao"
                                 ,"cor_negra_Rica"
                                 ,"cor_branca_Rica")
                     ,labels = c("Baixa Negra"
                                 ,"Baixa Branca"
                                 ,"Média"
                                 ,"Alta Negra"
                                 ,"Alta Branca"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Média"] %>% 
  .[pico == 1 ] %>% 
  .[order(CMASA30),city]


dt_tmp_city <- data.table(levels = c("bel","bho","bsb","cam","cgr","cur","duq"
                                     ,"for","goi","gua","mac","man","nat","poa"
                                     ,"rec","rio","sal","sgo","slz","spo"),
                          labels = c("Belem","Belo Horizonte","Brasilia","Campinas"
                                     ,"Campo Grande","Curitiba","Duque de Caxias"
                                     ,"Fortaleza","Goiania","Guarulhos","Maceio"
                                     ,"Manaus","Natal","Porto Alegre"
                                     ,"Recife","Rio de Janeiro","Salvador"
                                     ,"Sao Goncalo"
                                     ,"Sao Luis","Sao Paulo"))
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_plot[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]


ggplot(tmp_plot ) + 
  geom_segment(data = tmp_plot[,list(min(CMASA30),max(CMASA30)),by = city_f]
               ,aes(x = V1,xend = V2,y = city_f,yend = city_f),color = "grey")+
  geom_point(aes(x = CMASA30,y = city_f
                 ,color = label),size=2.5,shape = 1,stroke = 2.)+
  scale_color_manual(values = c("#b79f00","#dbcf80"
                                ,"red","#89CFF0","#0000FF"))+
  # facet_grid(rows  = vars(mode),scales = "free")+
  labs(title = "Medida cumulativa ativa (2019)"
       ,subtitle = "Oportunidades de sáude de alta complexidade acessíveis em até 30 minutos por \ntransporte público"
       ,color = "Renda e raça"
       ,x = "Nº de oportunidades"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()

ggsave(filename = "figures/CMASA30_transit.png",
       width = 24,height = 20,scale = 0.8,units = "cm")

