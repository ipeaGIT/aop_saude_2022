


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
filepath_acess <- "L:\\Proj_acess_oport\\data\\acesso_oport\\output_access"

files_acess <- list.files(path = filepath_acess#paste0(filepath_acess,"\\2017")
                          ,recursive = TRUE
                          ,full.names = TRUE)
files_acess <- files_acess[!(files_acess %like% "_old")]
files_acess <- files_acess[files_acess %like% "_all"]

files_acess_car <- list.files(path = filepath_acess#paste0(filepath_acess,"\\2017")
                          ,pattern = "car"
                          ,recursive = TRUE
                          ,full.names = TRUE)

filepath_socio <- "L:\\Proj_acess_oport\\data\\acesso_oport\\hex_agregados\\"
files_socio <- list.files(path = filepath_socio#paste0(filepath_socio,"\\2017")
                          ,recursive = TRUE
                          ,pattern = "09"
                          ,full.names = TRUE)
files_socio <- files_socio[!(files_socio %like% "_old")]

# 

tmp_acc <- lapply(files_acess,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)
tmp_soc <- lapply(files_socio,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)
tmp_car <- lapply(files_acess_car,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)

# joint dataset ----
# add socio into car
tmp_join_car <- data.table::setDT(tmp_car)[
  data.table::setDT(tmp_soc), on = c("origin"="id_hex"
                                     ,"city" = "sigla_muni"
                                     ,"ano")
  ,`:=`(cor_branca = cor_branca
        ,cor_amarela = cor_amarela
        ,cor_indigena = cor_indigena
        ,cor_negra = cor_negra)] 
tmp_join_car <- tmp_join_car[,.(origin,city,mode
                        ,pico,CMASA30,CMASB30
                        ,geometry
                        ,ano
                        ,cor_branca
                        ,cor_amarela
                        ,cor_indigena
                        ,cor_negra)]
# add socio into acc
tmp_join_acc <- data.table::setDT(tmp_acc)[
  data.table::setDT(tmp_soc), on = c("origin"="id_hex"
                                     ,"city" = "sigla_muni"
                                     ,"ano")
  ,`:=`(cor_branca = cor_branca
        ,cor_amarela = cor_amarela
        ,cor_indigena = cor_indigena
        ,cor_negra = cor_negra)] 
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

# first plot
tmp_plot <- tmp_w %>% 
  .[pico == 1 & ano == 2017 ] %>% 
  .[cor %in% c("cor_branca" ,"cor_negra")] %>% 
  .[,label := paste0(cor,"_",mode)] %>% 
  .[,label := factor(label
                     ,levels = c("cor_negra_walk"
                                 ,"cor_branca_walk"
                                 ,"cor_negra_bike"
                                 ,"cor_branca_bike"
                                 ,"cor_negra_transit"
                                 ,"cor_branca_transit"
                                 ,"cor_negra_car"
                                 ,"cor_branca_car")
                     ,labels = c("Negra - A pé"
                                 ,"Branca - A pé"
                                 ,"Negra - Bicicleta"
                                 ,"Branca - Bicicleta"
                                 ,"Negra - Transporte Público"
                                 ,"Branca - Transporte Público"
                                 ,"Negra - Automóvel"
                                 ,"Branca - Automóvel"))] 

tmp_city_order <- data.table::copy(tmp_plot)[label == "Negra - A pé",][order(V1)]$city
dt_tmp_city <- data.table(levels = tmp_city_order,
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
tmp_plot[,city_f := factor(city
                           ,levels = dt_tmp_city$levels
                           ,labels = dt_tmp_city$labels)]
p1 <- ggplot(tmp_plot[mode != "car"]) +
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
       ,y = "Município"
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()

car_order <- data.table::copy(tmp_plot)[label == "Negra - Automóvel",][order(V1)]$city

tmp_plot[,city_f := factor(city
                           ,labels = dt_tmp_city[levels %in% car_order]$labels
                           ,levels = car_order)]

p2 <- ggplot(tmp_plot[mode == "car"]) +
  geom_point(aes(x = V1,y = city_f
                 ,fill = label),size=3,shape = 21)+
  scale_fill_manual(values = c("#f8766d","#fcbbb6"))+
  labs(#title = "Acessibilidade acumulada (2017)"
       #,subtitle = "Oportunidades de sáude de alta complexidade acessíveis em até 30 minutos"
       ,x = "Acessibilidade acumulada em 30 minutos"
       ,y = "Município"
       , fill = "Cor - Modo de transporte")+
  guides(shape = "none")+
  theme_bw()

pf <- p1 / p2 

dir.create("figures")
ggsave(filename = "figures/CMA30.png",
       width = 25,height = 20,scale = 1,units = "cm")
