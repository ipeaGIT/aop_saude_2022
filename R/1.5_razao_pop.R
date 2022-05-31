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

# Read file ------

tmp_join_raw <- readr::read_rds("data/socio_acc-all_2019.rds")  # source("R/0.0_join_acess-soc.R")

# initial filter
tmp_join <- data.table::copy(tmp_join_raw) %>%  
  .[pop_total > 0,] %>% 
  .[mode == "walk",] %>% 
  .[pico == 1,] %>% 
  .[decil %in% c(9,10),decil_status := "Rica"] %>% 
  .[decil %in% c(1:2),decil_status := "Pobre"]


# organize data in columns
tmp_join <- data.table::melt(tmp_join
                             ,measure.vars = list(c("cor_branca","cor_negra"
                                                    ,"cor_amarela","cor_indigena"))
                             ,variable.name = "cor"
                             ,value.name = "total_by_color")

# sum population by (city,mode,pico,ano,cor)
tmp_join <- tmp_join %>% 
  .[,
    lapply(.SD,sum,na.rm = TRUE)
    ,by = .(city,decil_status,cor)
    ,.SDcols = c("total_by_color")
  ]

# remove unecessary data
tmp_join <- tmp_join %>% 
  .[!is.na(decil_status),] %>% 
  .[cor %in% c("cor_branca","cor_negra"),]

# add negra_pobre into a column
tmp_join <- tmp_join[,{
  
  tmp_id <- which(decil_status == "Pobre" & cor == "cor_negra")
  
  list(
    cor
    , decil_status
    , total_by_color
    , negra_pobre =  total_by_color[tmp_id]
  )
  
},by = city]


# verify data
tmp_join[city == "spo"]

# razao racial
tmp_join[,razao_racial := negra_pobre / total_by_color]

tmp_join[city == "cam"]

# plot ------

# find city order
tmp_city_order <- data.table::copy(tmp_join) %>% 
  .[decil_status  == "Pobre" & cor == "cor_branca"] %>% 
  .[order(razao_racial),city]

tmp_city_order
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

# add as factors
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_join[,city_f := factor(
   x = city
  ,levels = tmp_city_order
  ,labels = tmp_city_labels
)]

# add label
tmp_join[cor == "cor_branca" & decil_status == "Rica" , label := "Alta Branca" ]
tmp_join[cor == "cor_branca" & decil_status == "Pobre", label := "Baixa Branca"]
tmp_join[cor == "cor_negra"  & decil_status == "Rica" , label := "Alta Negra"  ]
tmp_join[cor == "cor_negra"  & decil_status == "Pobre", label := "Baixa Negra" ]

# verify data for spo
tmp_join[city == "spo"]


# plot
ggplot() + 
  # add points
  geom_point(data = tmp_join
             ,aes(x = razao_racial,y = city_f
                  ,color = label)
             ,size=2.5,alpha = 1
             ,shape = 1,stroke = 2.) +
  # add pallete and x_continuous
  scale_colour_aop(palette = "clevel") +
  scale_x_continuous(labels = 1:8
                     ,breaks = seq(1
                                   ,max(round(tmp_join$razao_racial,0))
                                   ,by = 1)) +
  # add vertical line
  geom_vline(xintercept = 1,linetype = "dashed")+
  # labs
  labs(x = "Razão pop. negra de baixa renda / pop. de cor/raça - renda"
       , y = NULL
       , color = "Renda - Cor/raça"
       , title = "Razão população conforme cor e renda"
       , subtitle = "Proporção entre população negra de baixa renda sobre\npopulação por cor/raça - renda")+
  # adjust legend.position 
  theme(legend.position = c(0.875,0.35))

