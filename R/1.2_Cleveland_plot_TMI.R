####################################################################################################################
################################### SCRIPT TO GENERATE TMI MAPS - SAUDE 01/02/2022 #################################
####################################################################################################################
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(ggalt)
library(hrbrthemes)
library(ggnewscale)
library(tidyverse)

##### Cleveland plot tempo medio de acesso aos equipamentos por raça

setwd("~/data")

#all <- readRDS("socio_acc-all.rds") %>%
all <- readRDS("socio_acc-car.rds") %>%
  select(city, mode, TMISB, cor_branca, cor_negra, pop_total, decil)
  
all <- all %>%
  filter(!is.na(TMISB),
         !is.infinite(TMISB),
         decil %in% c(1,2,9,10),
         mode == "car")


all_media_geral <- all %>%
  group_by(city) %>%
  summarise(medida = sum(pop_total*TMISB/sum(pop_total), na.rm = T)) %>%
  mutate(cor = "populacao geral")


all_branca_alta <- all %>%
  filter(decil %in% c(9,10)) %>%
  group_by(city) %>%
  summarise(medida = sum(cor_branca*TMISB/sum(cor_branca), na.rm = T)) %>%
  mutate(cor= "alta branca")

all_branca_baixa <- all %>%
  filter(decil %in% c(1,2)) %>%
  group_by(city) %>%
  summarise(medida = sum(cor_branca*TMISB/sum(cor_branca), na.rm = T)) %>%
  mutate(cor= "baixa branca")

all_negra_alta <- all %>%
  filter(decil %in% c(9,10)) %>%
  group_by(city) %>%
  summarise(medida = sum(cor_negra*TMISB/sum(cor_negra), na.rm = T)) %>%
  mutate(cor= "alta negra")

all_negra_baixa <- all %>%
  filter(decil %in% c(1,2)) %>%
  group_by(city) %>%
  summarise(medida = sum(cor_negra*TMISB/sum(cor_negra), na.rm = T)) %>%
  mutate(cor= "baixa negra")


all_1 <- rbind(all_branca_alta, all_branca_baixa, all_negra_alta, all_negra_baixa, all_media_geral)

cidades <- c("Belém", "Belo Horizonte", "Brasília", "Campinas", "Campo Grande", "Curitiba", "Duque de Caxias", "Fortaleza", "Goiânia", "Guarulhos", "Maceió", "Manaus", "Natal", "Porto Alegre","Recife", "Rio de Janeiro", "Salvador","São Gonçalo", "São Luís", "São Paulo")
cidades <- as.data.frame(cidades)
cid_abrev <- c("bel", "bho", "bsb", "cam", "cgr", "cur", "duq", "for", "goi", "gua", "mac", "man", "nat", "poa","rec", "rio", "sal","sgo", "slz", "spo")
cid_abrev <- as.data.frame(cid_abrev)

de_para <- cbind(cid_abrev, cidades)

all_1_plot <- dplyr::left_join(all_1, de_para,by=c("city"="cid_abrev")) %>%
  select(-c(city))

p <- ggplot(all_1_plot, aes(y=reorder(cidades, -medida), x=medida))+
        geom_line(aes(group = cidades), size = 2, color = "grey")+
        #geom_point(aes(color = cor), size = 4)#, shape=1, stroke = 2)
        geom_point(aes(color = cor), size = 4, shape=1, stroke = 2)
  

p + scale_color_manual(name = "Renda e Raça",
                     values = c("alta branca"="#08519c","baixa branca"="#6baed6","populacao geral" = "red","alta negra"="black","baixa negra"="#636363"),
                     labels = c("Alta Branca", "Baixa Branca","População Geral","Alta Negra","Baixa Negra"))+
    #scale_x_continuous(labels = c("5 Min.", "10 Min.", "15 Min.", "20 Min.", "25 Min.", "30 Min.", "35 Min.", "40 Min."), # baixa complexidade car    
    #scale_x_continuous(labels = c("5 Min.","10 Min.", "15 Min.", "20 Min.", "25 Min."), # media complexidade transit    
    scale_x_continuous(labels = c("0 Min.","5 Min."), # baixa complexidade car    
    #scale_x_continuous(labels = c("0 Min.","5 Min.", "10 Min.", "15 Min.", "20 Min."), # alta complexidade car  
    #scale_x_continuous(labels = c("10 Min.","20 Min.", "30 Min.", "40 Min.", "50 Min.", "60 Min."), # alta complexidade transit
    #scale_x_continuous(labels = c("5 Min.","10 Min.", "15 Min.", "20 Min.", "25 Min."), # baixa complexidade transit    
    #scale_x_continuous(labels = c("5 Min.","10 Min.", "15 Min.", "20 Min.", "25 Min.", "30 Min."), # baixa complexidade transit    
    #scale_x_continuous(labels = c("10 Min.","20 Min.", "30 Min.", "40 Min.", "50 Min."), # alta complexidade a pé
    #scale_x_continuous(labels = c("10 Min.","15 Min.", "20 Min.", "25 Min.","30 Min."), # media complexidade a pé
    #scale_x_continuous(labels = c("10 Min.","15 Min.", "20 Min.", "25 Min."), # baixa complexidade a pé
                       #limits = c(10,60), # media complexidade a pé                                      
                       #limits = c(5,40), # media complexidade a pé                                      
                       #limits = c(0,10), # media complexidade car                   
                       limits = c(0,5), # baixa complexidade car                   
                       #limits = c(0,20), # alta complexidade car                   
                       #limits = c(10,60), # alta complexidade transit
                       #limits = c(5,30), # baixa complexidade transit
                       #limits = c(10,50), # alta complexidade a pé
                       #limits = c(10,25), # media complexidade a pé
                       #breaks = seq(10,60, by=10))+ # media complexidade a pé                                                                            
                       #breaks = seq(5,40, by=5))+ # media complexidade a pé
                       #breaks = seq(5,30, by=5))+ # baixa complexidade a pé                                                         
                       #breaks = seq(0,10, by=5))+ # media complexidade car                                      
                       breaks = seq(0,5, by=5))+ # media complexidade a pé                   
                       #breaks = seq(0,20, by=5))+ # media complexidade a pé                   
                       #breaks = seq(10,60, by=10))+ # media complexidade a pé                   
                       #breaks = seq(10,50, by=10))+ # media complexidade a pé
                       #breaks = seq(10,25, by=5))+ # media complexidade a pé
    scale_y_discrete(expand = c(.02,0)) +
    #labs(title = "Tempo mínimo a pé até o estabelecimento de saúde mais próximo",
    labs(title = "Tempo mínimo de carro até o estabelecimento de saúde \nmais próximo",  
    #labs(title = "Tempo mínimo por transporte público até o estabelecimento \nde saúde mais próximo",
         subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.direction = "vertical",
          text = element_text(family = "sans", face = "bold", size = 14),
          plot.title = element_text(size = 16, margin = margin(b=10)),
          plot.subtitle = element_text(size=14, color = "darkslategrey", margin = margin(b = 25)),
          plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0))


ggsave("Cleveland_plot_TMISB_renda_vazada_car.png", width = 22, height = 22, units = "cm", dpi = 300)


rm(list = ls())













############################################################################################################
######### Cleveland plot de pessoas que demoram mais de 20 minutos para acessar os equipamentos ############
############################################################################################################


setwd("~/data")


#all <- readRDS("socio_acc-all.rds") %>%
all <- readRDS("socio_acc-car.rds") %>%
  select(city, mode, TMISB, cor_branca, cor_negra, pop_total,decil)

all <- all %>%
  filter(!is.na(TMISB),
         !is.infinite(TMISB),
         decil %in% c(1,2,9,10),
         #TMISA >20,
         mode == "car")

all_media_geral <- all %>%
  group_by(city) %>%
  summarise(proporcao = round((sum(pop_total[which(TMISB>20)]/sum(pop_total), na.rm = TRUE)*100),0)) %>%
  mutate(cor = "populacao geral")

all_branca_alta <- all %>%
  filter(decil %in% c(9,10)) %>%
  group_by(city) %>%
  summarise(proporcao = round((sum(cor_branca[which(TMISB>20)]/sum(cor_branca), na.rm = TRUE)*100),0)) %>%
  mutate(cor = "alta branca")


all_branca_baixa <- all %>%
  filter(decil %in% c(1,2)) %>%
  group_by(city) %>%
  summarise(proporcao = round((sum(cor_branca[which(TMISB>20)]/sum(cor_branca), na.rm = TRUE)*100),0))%>%
  mutate(cor = "baixa branca")


all_negra_alta <- all %>%
  filter(decil %in% c(9,10)) %>%
  group_by(city) %>%
  summarise(proporcao = round((sum(cor_negra[which(TMISB>20)]/sum(cor_negra), na.rm = TRUE)*100),0))%>%
  mutate(cor = "alta negra")


all_negra_baixa <- all %>%
  filter(decil %in% c(1,2)) %>%
  group_by(city) %>%
  summarise(proporcao = round((sum(cor_negra[which(TMISB>20)]/sum(cor_negra), na.rm = TRUE)*100),0))%>%
  mutate(cor = "baixa negra")


all_1 <- rbind(all_branca_alta, all_branca_baixa, all_negra_alta, all_negra_baixa, all_media_geral)

cidades <- c("Belém", "Belo Horizonte", "Brasília", "Campinas", "Campo Grande", "Curitiba", "Duque de Caxias", "Fortaleza", "Goiânia", "Guarulhos", "Maceió", "Manaus", "Natal", "Porto Alegre","Recife", "Rio de Janeiro", "Salvador","São Gonçalo", "São Luís", "São Paulo")
cidades <- as.data.frame(cidades)
cid_abrev <- c("bel", "bho", "bsb", "cam", "cgr", "cur", "duq", "for", "goi", "gua", "mac", "man", "nat", "poa","rec", "rio", "sal","sgo", "slz", "spo")
cid_abrev <- as.data.frame(cid_abrev)

de_para <- cbind(cid_abrev, cidades)

all_1_plot <- dplyr::left_join(all_1, de_para,by=c("city"="cid_abrev")) %>%
  select(-c(city))

p <- ggplot(all_1_plot, aes(y=reorder(cidades, -proporcao), x=proporcao))+
  geom_line(aes(group = cidades), size = 2, color = "grey")+
  geom_point(aes(color = cor), size = 4, shape=1, stroke=2)


p + scale_color_manual(name = "Renda e Raça",
              values = c("alta branca"="#08519c","baixa branca"="#6baed6", "populacao geral" = "red", "alta negra"="black","baixa negra"="#636363"),
              labels = c("Alta Branca", "Baixa Branca", "População Geral","Alta Negra","Baixa Negra"))+
  scale_x_continuous(labels = c("0%","10%", "20%", "30%"),
                     #labels = c("0%","20%", "40%", "60%","80%", "100%"),
                     limits = c(0,30),
                     #limits = c(0,100),
                     breaks = seq(0,30, by=10))+
                     #breaks = seq(0,100, by=20))+
  scale_y_discrete(expand = c(.02,0)) +
  labs(#title = "Proporção da população a mais de 20 minutos de caminhada\ndo estabelecimento de saúde mais próximo",
       #title = "Proporção da população a mais de 20 minutos por transporte público\ndo estabelecimento de saúde mais próximo",
       title = "Proporção da população a mais de 20 minutos de carro\ndo estabelecimento de saúde mais próximo",
       subtitle = "Estabelecimentos de saúde de baixa complexidade\n20 maiores cidades do Brasil (2019)")+
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.direction = "vertical",
        text = element_text(family = "sans", face = "bold", size = 14),
        plot.title = element_text(size = 16, margin = margin(b=10)),
        plot.subtitle = element_text(size=14, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0))


ggsave("Cleveland_plot_mais_20min_car_baixa_complexidade_vazado.png", width = 22, height = 22, units = "cm", dpi = 300)





