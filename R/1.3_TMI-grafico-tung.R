


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
                                ,ano
                                ,pop_total)]
# add socio into acc
tmp_join_acc <- tmp_join_acc[,.(origin,city,mode
                                ,pico,CMASA30,CMASB30
                                ,ano
                                ,pop_total)]
# rbind
tmp_join <- rbind(tmp_join_car,tmp_join_acc)

# weighted mean
tmp_w <- data.table::copy(tmp_join)[,weighted.mean(x = CMASA30
                                                   ,w = pop_total,na.rm = TRUE)
                                    ,by = .(city,mode,pico,ano)]
tmp_w <- tmp_w %>% 
  .[!is.na(mode) | !is.nan(mode)] %>% 
  .[!is.nan(V1)] %>% 
  .[pico == 1]

# prepare to plot

dt_tmp_city <- data.table(levels = c("bsb","cgr","gua","cam"
                                     ,"cur","spo","man","bho"
                                     ,"poa","goi","duq","mac"
                                     ,"sgo","slz","rio","for"
                                     ,"sal","rec","nat","bel"),
                          labels = c("Brasília/DF","Campo Grande/MS"
                                     ,"Guarulhos/SP","Campinas/SP"
                                     ,"Curitiba/PR","São Paulo/SP"
                                     ,"Manaus/AM","Belo Horizonte/MG"
                                     ,"Porto Alegre/RS","Goiânia/GO"
                                     ,"Duque de Caxias/RJ","Maceió/AL"
                                     ,"São Gonçalo/RJ","São Luís/MA"
                                     ,"Rio de Janeiro/RJ","Fortaleza/CE"
                                     ,"Salvador/BA","Recife/PE"
                                     ,"Natal/RN","Belém/PA"))

#  plot 1 ----
tmp_w1 <- data.table::copy(tmp_w) 

tmp_city_order <- data.table::copy(tmp_w1) %>% 
  .[mode == "transit" &  ano == 2017] %>% 
  .[order(V1),city]

tmp_w1[dt_tmp_city,on = c("city" = "levels"),city_name := i.labels]
tmp_w1[,city_f := factor(city
                         ,levels = tmp_city_order 
                         ,labels = dt_tmp_city[which(dt_tmp_city$levels %in% tmp_city_order),labels])]
tmp_w1[,N := .N,by = .(city,mode)]


to_string <- as_labeller(c(`walk` = "A pé",
                           `bike` = "Bicicleta",
                           `transit` = "Transporte Público",
                           `car` = "Automóvel"))
tmp_w1[,mode_f := factor(mode,levels = c("walk","bike","transit","car"))]


ggplot(tmp_w1[!is.na(city_f) & V1 >0 &
                N == 3 & (mode == "car" | mode == "transit")]) + # ggplot(tmp_plot[mode != "car"]) +
  geom_point(aes(x = V1,y = city_f,fill = as.factor(ano)),size=3,shape = 21)+
  scale_fill_manual(values = c("#f8766d"
                               ,"#b79f00"
                               ,"#00bfc4"
                               ,"#f46ce3"))+
  facet_wrap(~mode_f,scales = "free_x", labeller = to_string)+
  labs(title = "Acessibilidade a estabelecimentos de sáude de alta complexidade"
       ,subtitle = "Oportunidades acessíveis em até 30 minutos (2017, 2018 e 2019)"
       ,x = "Acessibilidade acumulada em 30 minutos"
       #,x = NULL
       , y = NULL
       #,y = "Município"
       , fill = "Ano")+
  guides(shape = "none")+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0))

ggsave(filename = "figures/CMA30_historico.png",
       width = 18,height = 7.5,scale = 1.35,units = "cm")

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
