


# load packages ------

rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
library(sf)
library(patchwork)
library(mapview)
library(aopdata) # devtools::install_github("ipeaGIT/aopdata", subdir = "r-package")


# read files ------

tmp_join_car <- readr::read_rds("data/socio_acc-car.rds")
tmp_join_acc <- readr::read_rds("data/socio_acc-all.rds")

# joint dataset ----
# add socio into car
tmp_join_car <- tmp_join_car[,.(origin,city,mode
                        ,pico,CMASA30,CMASB30
                        ,geometry
                        ,ano
                        ,cor_branca
                        ,cor_amarela
                        ,cor_indigena
                        ,cor_negra)]
# add socio into acc
tmp_join_acc <- tmp_join_acc[,.(origin,city,mode
                                ,pico,CMASA30,CMASB30
                                ,geometry
                                ,ano
                                ,cor_branca
                                ,cor_amarela
                                ,cor_indigena
                                ,cor_negra)]
# rbind
tmp_join <- rbind(tmp_join_car,tmp_join_acc)


# format wide-to-long
tmp_melt <- data.table::melt(tmp_join
                             ,measure.vars = list(c("cor_branca","cor_negra"
                                                    ,"cor_amarela","cor_indigena"))
                             ,variable.name = "cor"
                             ,value.name = "total")
tmp_melt[is.na(total) | is.nan(total),total := 0]

tmp_w <- data.table::copy(tmp_melt)[,weighted.mean(x = CMASA30
                                                   ,w = total,na.rm = TRUE)
                                    ,by = .(city,mode,pico,ano,cor)]

#  plot 1 ----
tmp_plot <- data.table::copy(tmp_w) %>% 
  .[pico == 1 & ano == 2017 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra")] %>% 
  .[mode %in% c("car","transit")] %>% 
  .[,label := paste0(cor,"_",mode)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_negra_transit"
                                 ,"cor_branca_transit"
                                 ,"cor_negra_car"
                                 ,"cor_branca_car")
                     ,labels = c("Negra - Transporte Público"
                                 ,"Branca - Transporte Público"
                                 ,"Negra - Automóvel"
                                 ,"Branca - Automóvel"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Negra - Transporte Público",] %>% 
  .[order(V1),city]

dt_tmp_city <- data.table(levels = c("cam", "cur", "spo", "bho", "poa", "for"),
                          labels = c("Campinas/SP","Curitiba/PR"
                                     ,"São Paulo/SP","Belo Horizonte/MG"
                                       ,"Porto Alegre/RS","Fortaleza/CE"))
tmp_plot[,city_f := factor(city
                           ,levels = dt_tmp_city$levels
                           ,labels = dt_tmp_city$labels)]
p1 <- ggplot(tmp_plot[!is.na(city_f)]) + # ggplot(tmp_plot[mode != "car"]) +
  geom_point(aes(x = V1,y = city_f
                 ,fill = label),size=3,shape = 21)+
  scale_fill_manual(values = c("#f8766d","#fcbbb6"
                                ,"#b79f00","#dbcf80"
                                ,"#00bfc4","#80dfe2"
                                ,"#f46ce3","#7a3672"))+
  labs(title = "Acessibilidade acumulada (2017)"
       ,subtitle = "Oportunidades de sáude de alta complexidade acessíveis em até 30 minutos"
       #,x = "Acessibilidade acumulada em 30 minutos"
       ,x = NULL
       , y = NULL
       #,y = "Município"
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()

ggsave(filename = "figures/CMA30_both.png",
       width = 20,height = 10,scale = 1,units = "cm")

# plot 2-----
tmp_plot <- data.table::copy(tmp_w) %>% 
  .[pico == 1 & ano == 2017 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra")] %>% 
  .[mode %in% c("car","transit")] %>% 
  .[,label := paste0(cor,"_",mode)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_negra_transit"
                                 ,"cor_branca_transit"
                                 ,"cor_negra_car"
                                 ,"cor_branca_car")
                     ,labels = c("Negra - Transporte Público"
                                 ,"Branca - Transporte Público"
                                 ,"Negra - Automóvel"
                                 ,"Branca - Automóvel"))] 

tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[label == "Negra - Transporte Público",] %>% 
  .[order(V1),city]

dt_tmp_city <- data.table(levels = c("cam", "cur", "spo", "bho", "poa", "for"),
                          labels = c("Campinas/SP","Curitiba/PR"
                                     ,"São Paulo/SP","Belo Horizonte/MG"
                                     ,"Porto Alegre/RS","Fortaleza/CE"))
tmp_plot[,city_f := factor(city
                           ,labels = dt_tmp_city[order(match(levels,tmp_city_order)),labels]
                           ,levels = tmp_city_order)]

# ggplot(tmp_plot[!is.na(city_f)]) + # ggplot(tmp_plot[mode != "car"]) +
#   geom_point(aes(x = V1,y = city_f
#                  ,fill = label),size=3,shape = 21)+
#   facet_wrap(~mode,scales = "free",ncol= 1,)+
#   scale_fill_manual(values = c("#f8766d","#fcbbb6","#b79f00","#dbcf80"))+
#   labs(title = "Acessibilidade acumulada (2017)"
#        ,subtitle = "Oportunidades de sáude de alta complexidade acessíveis em até 30 minutos"
#        ,x = "Acessibilidade acumulada em 30 minutos"
#        ,x = NULL
#        #,y = "Município"
#        , y = NULL
#        , fill = "Cor - Modo de transporte")+
#   guides(shape = "none")+
#   theme_bw()+
#   theme(strip.text.x = element_blank(), 
#         strip.background = element_blank())


p1 <- ggplot(tmp_plot[!is.na(city_f)]) + # ggplot(tmp_plot[mode != "car"]) +
  geom_point(aes(x = V1,y = city_f,fill = label),size=3,shape = 21)+
  scale_x_continuous(breaks = 1:10,limits = c(1,6.5))+
  scale_fill_manual(values = c("#f8766d","#fcbbb6","#b79f00","#dbcf80"))+
  labs(title = "Acessibilidade acumulada (2017)"
       ,subtitle = "Oportunidades de sáude de alta complexidade acessíveis em até 30 minutos"
       #,x = "Acessibilidade acumulada em 30 minutos"
       ,x = NULL
       #,y = "Município"
       , y = NULL
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()
p1

car_order <- data.table::copy(tmp_plot) %>% 
  .[!is.na(city_f) & label == "Negra - Automóvel",] %>% 
  .[order(V1),city]

tmp_plot[,city_f := factor(city
                           ,labels = dt_tmp_city[order(match(levels,car_order)),labels]
                           ,levels = car_order)]

p2 <- ggplot(tmp_plot[!is.na(city_f) & mode == "car"]) +
  geom_point(aes(x = V1,y = city_f
                 ,fill = label),size=3,shape = 21)+
  scale_fill_manual(values = c("#b79f00","#dbcf80"))+
  labs(#title = "Acessibilidade acumulada (2017)"
       #,subtitle = "Oportunidades de sáude de alta complexidade acessíveis em até 30 minutos"
       ,x = "Acessibilidade acumulada em 30 minutos"
       #,y = "Município"
       , y = NULL
       , fill = NULL)+
  guides(shape = "none",fill = "none")+
  theme_bw()

pf <- p1 / p2 

pf
#dir.create("figures")
ggsave(filename = "figures/CMA30.png",
       width = 25,height = 20,scale = 0.8,units = "cm")
