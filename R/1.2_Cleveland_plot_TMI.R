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


all <- readRDS("socio_acc-all.rds") %>%
#all <- readRDS("socio_acc-car.rds") %>%
  select(city, mode, TMISA, cor_branca, cor_negra)
  
all <- all %>%
  filter(!is.na(TMISA),
         !is.infinite(TMISA),
         mode == "walk")

all_1 <- all %>%
  group_by(city) %>%
  summarise(TMISA_branco = sum(cor_branca*TMISA/sum(cor_branca), na.rm = TRUE),
            TMISA_negro = sum(cor_negra*TMISA/sum(cor_negra), na.rm = TRUE))

all_1 <- all_1 %>%
  ungroup() %>%
  mutate(city = factor(city, levels = all_1$city))

all_1_branca <- all_1 %>%
  select(city,TMISA_branco) %>%
  mutate(cor = "branca") %>%
  rename(TMISA = "TMISA_branco")

all_1_negra <- all_1 %>%
  select(city,TMISA_negro) %>%
  mutate(cor = "negra") %>%
  rename(TMISA = "TMISA_negro")

all_1_plot <- rbind(all_1_branca, all_1_negra)
  
cidades <- c("Belém", "Belo Horizonte", "Brasília", "Campinas", "Campo Grande", "Curitiba", "Duque de Caxias", "Fortaleza", "Goiânia", "Guarulhos", "Maceió", "Manaus", "Natal", "Porto Alegre","Recife", "Rio de Janeiro", "Salvador","São Gonçalo", "São Luís", "São Paulo")
cidades <- as.data.frame(cidades)
cid_abrev <- c("bel", "bho", "bsb", "cam", "cgr", "cur", "duq", "for", "goi", "gua", "mac", "man", "nat", "poa","rec", "rio", "sal","sgo", "slz", "spo")
cid_abrev <- as.data.frame(cid_abrev)

de_para <- cbind(cid_abrev, cidades)

all_1_plot <- dplyr::left_join(all_1_plot, de_para,by=c("city"="cid_abrev")) %>%
  select(-c(city))

p <- ggplot(all_1_plot, aes(y=reorder(cidades, -TMISA), x=TMISA))+
        geom_line(aes(group = cidades), size = 2, color = "grey")+
        geom_point(aes(color = cor), size = 4)
  

p + scale_color_manual(name = "Raça",
                     values = c("branca"="deepskyblue3","negra"="black"),
                     labels = c("Branca", "Negra"))+
    scale_x_continuous(labels = c("20 Min.", "25 Min.", "30 Min.","35 Min."),
                       limits = c(17,38),
                       breaks = seq(20,35, by=5))+
    scale_y_discrete(expand = c(.02,0)) +
    labs(title = "Tempo mínimo a pé até o estabelecimento de saúde mais próximo",
         subtitle = "Estabelecimentos de saúde de alta complexidade\n20 maiores cidades do Brasil (2019)")+
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_blank(),
          legend.direction = "vertical",
          text = element_text(family = "sans", face = "bold", size = 14),
          plot.title = element_text(size = 16, margin = margin(b=10)),
          plot.subtitle = element_text(size=14, color = "darkslategrey", margin = margin(b = 25)),
          plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0))


ggsave("Cleveland_plot_TMISA.png", width = 22, height = 22, units = "cm", dpi = 300)

rm(list = ls())

############################################################################################################
######### Cleveland plot de pessoas que demoram mais de 20 minutos para acessar os equipamentos ############
############################################################################################################


setwd("~/data")


all <- readRDS("socio_acc-all.rds") %>%
  #all <- readRDS("socio_acc-car.rds") %>%
  select(city, mode, TMISA, cor_branca, cor_negra)

all <- all %>%
  filter(!is.na(TMISA),
         !is.infinite(TMISA),
         #TMISA >20,
         mode == "walk")

all_1 <- all %>%
  group_by(city) %>%
  summarise(prop_branca = round((sum(cor_branca[which(TMISA>20)]/sum(cor_branca), na.rm = TRUE)*100),0),
            prop_negro = round((sum(cor_negra[which(TMISA>20)]/sum(cor_negra), na.rm = TRUE)*100),0))

all_1 <- all_1 %>%
  ungroup() %>%
  mutate(city = factor(city, levels = all_1$city))

all_1_branca <- all_1 %>%
  select(city,prop_branca) %>%
  mutate(cor = "branca") %>%
  rename(proporcao = "prop_branca")

all_1_negra <- all_1 %>%
  select(city,prop_negro) %>%
  mutate(cor = "negra") %>%
  rename(proporcao = "prop_negro")

all_1_plot <- rbind(all_1_branca, all_1_negra)

cidades <- c("Belém", "Belo Horizonte", "Brasília", "Campinas", "Campo Grande", "Curitiba", "Duque de Caxias", "Fortaleza", "Goiânia", "Guarulhos", "Maceió", "Manaus", "Natal", "Porto Alegre","Recife", "Rio de Janeiro", "Salvador","São Gonçalo", "São Luís", "São Paulo")
cidades <- as.data.frame(cidades)
cid_abrev <- c("bel", "bho", "bsb", "cam", "cgr", "cur", "duq", "for", "goi", "gua", "mac", "man", "nat", "poa","rec", "rio", "sal","sgo", "slz", "spo")
cid_abrev <- as.data.frame(cid_abrev)

de_para <- cbind(cid_abrev, cidades)

all_1_plot <- dplyr::left_join(all_1_plot, de_para,by=c("city"="cid_abrev")) %>%
  select(-c(city))

p <- ggplot(all_1_plot, aes(y=reorder(cidades, -proporcao), x=proporcao))+
  geom_line(aes(group = cidades), size = 2, color = "grey")+
  geom_point(aes(color = cor), size = 4)


p + scale_color_manual(name = "Raça",
                       values = c("branca"="deepskyblue3","negra"="black"),
                       labels = c("Branca", "Negra"))+
  scale_x_continuous(labels = c("30%", "50%", "70%","90%"),
                     limits = c(30,90),
                     breaks = seq(30,90, by=20))+
  scale_y_discrete(expand = c(.02,0)) +
  labs(title = "Proporção da população a mais de 20 minutos de caminhada\ndo estabelecimento de saúde mais próximo",
       subtitle = "Estabelecimentos de saúde de alta complexidade\n20 maiores cidades do Brasil (2019)")+
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.direction = "vertical",
        text = element_text(family = "sans", face = "bold", size = 14),
        plot.title = element_text(size = 16, margin = margin(b=10)),
        plot.subtitle = element_text(size=14, color = "darkslategrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 8, margin = margin(t=10), color = "grey70", hjust = 0))


ggsave("Cleveland_plot_mais_20min.png", width = 22, height = 22, units = "cm", dpi = 300)





