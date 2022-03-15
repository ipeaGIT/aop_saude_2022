


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

tmp_join_bus_raw <- readr::read_rds("data/socio_acc-all_2019.rds") 
tmp_join_bus <- data.table::copy(tmp_join_bus_raw)

tmp_join_car_raw <- readr::read_rds("data/socio_acc-car_2019.rds") 
tmp_join_car <- data.table::copy(tmp_join_car_raw)

tmp_join <- list(tmp_join_bus,tmp_join_car) %>% 
  data.table::rbindlist(use.names = TRUE,fill = TRUE)

# decil status
tmp_join[decil %in% c(10),decil_status := "10p_ricos"]
tmp_join[decil %in% c(1:4),decil_status := "40p_pobres"]
tmp_join <- tmp_join[!is.na(decil_status)]
tmp_join <- tmp_join[pico  == 1]

tmp_join
# weighted average
# by (city,mode,pico,ano,cor)

# tmp_w1 <- data.table::copy(tmp_join)  %>% 
#   .[,lapply(.SD,sum,na.rm = TRUE)
#     ,by = .(city,mode,pico,ano,decil_status)
#     ,.SDcols = c("CMASB30","CMASA30","TMISB","TMISA","pop_total")] 
tmp_w1 <- data.table::copy(tmp_join) %>%
  .[,lapply(.SD,weighted.mean,pop_total,na.rm = TRUE)
    ,by = .(city,mode,pico,ano,decil_status)
    ,.SDcols = c("CMASB30","CMASA30","TMISB","TMISA")]


#  long-to-wide reshaping

tmp_w2 <- data.table::dcast(data = tmp_w1,
                            formula = city  + mode + pico + ano ~ 
                              decil_status
                            , value.var = c("CMASB30","CMASA30")) 

# palm ratio

tmp_w2[,RP_CMASB30 := CMASB30_10p_ricos/CMASB30_40p_pobres ]
tmp_w2[,RP_CMASA30 := CMASA30_10p_ricos/CMASA30_40p_pobres ]


# PLOT 1 = transit X CMASA30 ----

tmp_plot <- data.table::copy(tmp_w2) %>% 
  .[mode == "transit",]


tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[order(RP_CMASA30),city]


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

ggplot(tmp_plot)+
  geom_bar(aes(x = RP_CMASA30,y = city_f),
           stat = "identity",
           position = "dodge")+
  geom_vline(xintercept = 1,linetype = "dashed")+
  scale_x_continuous(expand = c(0,0),limits = c(0,fifelse(max(tmp_plot$RP_CMASA30)<1,1.05,max(tmp_plot$RP_CMASA30)*1.05)) ) + 
  labs(title = "Desigualdade de acesso a oportunidades de saúde de alta complexidade"
       ,subtitle = 'Razão entre a média do número de estabelecimentos acessíveis \nem 30 minutos entre os 10% mais ricos e os 40% mais pobres por transporte público',
       ,x = "Razão de Palma"
       , y = NULL)+
  geom_text(aes(x = RP_CMASA30,y = city_f,
                label = round(RP_CMASA30,1)), 
            hjust = -0.2,size = 3.25,fontface = "bold",
            position = position_dodge(width = 1))+
  theme_bw()


ggsave(filename = "figures/RP_CMASA30_transit.png",
       width = 24,height = 20,scale = 0.8,units = "cm")

# PLOT 2 = car X CMASA30 ----

tmp_plot <- data.table::copy(tmp_w2) %>% 
  .[mode == "car",]


tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[order(RP_CMASA30),city]


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

ggplot(tmp_plot)+
  geom_bar(aes(x = RP_CMASA30,y = city_f),
           stat = "identity",
           position = "dodge")+
  geom_vline(xintercept = 1,linetype = "dashed")+
  scale_x_continuous(expand = c(0,0),limits = c(0,fifelse(max(tmp_plot$RP_CMASA30)<1,1.05,max(tmp_plot$RP_CMASA30)*1.05)) ) + 
  labs(title = "Desigualdade de acesso a oportunidades de saúde de alta complexidade"
       ,subtitle = 'Razão entre a média do número de estabelecimentos acessíveis \nem 30 minutos entre os 10% mais ricos e os 40% mais pobres por automóvel',
       ,x = "Razão de Palma"
       , y = NULL)+
  geom_text(aes(x = RP_CMASA30,y = city_f,
                label = round(RP_CMASA30,1)), 
            hjust = -0.2,size = 3.25,fontface = "bold",
            position = position_dodge(width = 1))+
  theme_bw()


ggsave(filename = "figures/RP_CMASA30_car.png",
       width = 24,height = 20,scale = 0.8,units = "cm")

# PLOT 3 = RP walk X CMASB30 ----

tmp_plot <- data.table::copy(tmp_w2) %>% 
  .[mode == "walk",]


tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[order(RP_CMASB30 ),city]


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

ggplot(tmp_plot)+
  geom_bar(aes(x = RP_CMASB30,y = city_f),
           stat = "identity",
           position = "dodge")+
  geom_vline(xintercept = 1,linetype = "dashed")+
  scale_x_continuous(expand = c(0,0),limits = c(0,fifelse(max(tmp_plot$RP_CMASB30)<1,1.05,max(tmp_plot$RP_CMASB30)*1.05)) ) + 
  labs(title = "Desigualdade de acesso a oportunidades de saúde de baixa complexidade"
       ,subtitle = 'Razão entre a média do número de estabelecimentos acessíveis \nem 30 minutos entre os 10% mais ricos e os 40% mais pobres por caminhada',
       ,x = "Razão de Palma"
       , y = NULL)+
  geom_text(aes(x = RP_CMASB30,y = city_f,
                label = round(RP_CMASB30,1)), 
            hjust = -0.2,size = 3.25,fontface = "bold",
            position = position_dodge(width = 1))+
  theme_bw()


ggsave(filename = "figures/RP_CMASB30_walk.png",
       width = 24,height = 20,scale = 0.8,units = "cm")

# PLOT 4 = RP bike X CMASB30 ----

tmp_plot <- data.table::copy(tmp_w2) %>% 
  .[mode == "bike",]


tmp_city_order <- data.table::copy(tmp_plot) %>% 
  .[order(RP_CMASB30 ),city]


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

ggplot(tmp_plot)+
  geom_bar(aes(x = RP_CMASB30,y = city_f),
           stat = "identity",
           position = "dodge")+
  geom_vline(xintercept = 1,linetype = "dashed")+
  scale_x_continuous(expand = c(0,0),limits = c(0,fifelse(max(tmp_plot$RP_CMASB30)<1,1.05,max(tmp_plot$RP_CMASB30)*1.05)) ) + 
  labs(title = "Desigualdade de acesso a oportunidades de saúde de baixa complexidade"
       ,subtitle = 'Razão entre a média do número de estabelecimentos acessíveis \nem 30 minutos entre os 10% mais ricos e os 40% mais pobres por bicicleta',
       ,x = "Razão de Palma"
       , y = NULL)+
  geom_text(aes(x = RP_CMASB30,y = city_f,
                label = round(RP_CMASB30,1)), 
            hjust = -0.2,size = 3.25,fontface = "bold",
            position = position_dodge(width = 1))+
  theme_bw()


ggsave(filename = "figures/RP_CMASB30_bike.png",
       width = 24,height = 20,scale = 0.8,units = "cm")
