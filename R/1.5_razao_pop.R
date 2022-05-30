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
tmp_join <- data.table::copy(tmp_join_bus_raw) %>%  
  .[pop_total > 0,] %>% 
  .[mode == "walk",] %>% 
  .[pico == 1,] %>% 
  .[decil %in% c(9,10),decil_status := "Rica"] %>% 
  .[decil %in% c(1:2),decil_status := "Pobre"]


#tmp_join[cor_negra  > 0.5 * (pop_total),dummyCor := "Negra"]
#tmp_join[cor_branca  > 0.5 * (pop_total),dummyCor := "Branca"]
tmp_join[decil %in% c(1:2)  & cor_negra   > 0.5 * (pop_total),dummySoc := "Baixa Negra" ]
tmp_join[decil %in% c(9:10) & cor_negra   > 0.5 * (pop_total),dummySoc := "Alta Negra"  ]
tmp_join[decil %in% c(1:2)  & cor_branca  > 0.5 * (pop_total),dummySoc := "Baixa Branca"]
tmp_join[decil %in% c(9:10) & cor_branca  > 0.5 * (pop_total),dummySoc := "Alta Branca" ]

tmp_join[dummySoc == "Alta Negra" & city == "spo",]
#tmp_melt <- data.table::melt(tmp_join
#                             ,measure.vars = list(c("cor_branca","cor_negra"
#                                                    ,"cor_amarela","cor_indigena"))
#                             ,variable.name = "cor"
#                             ,value.name = "total_by_color")

tmp_join[origin == "89a88cd814fffff"]
tmp_join[dummySoc == "Alta Negra" & city == "spo",]
# by (city,mode,pico,ano,cor)
tmp_w1 <- data.table::copy(tmp_join) %>% 
  .[,
    lapply(.SD,sum,na.rm = TRUE)
    ,by = .(city,dummySoc)
    ,.SDcols = c("cor_branca","cor_negra","pop_total")
  ]

tmp_w1[city == "spo"]
#tmp_w1 <- tmp_w1[!is.na(decil_status),]
tmp_w1 <- tmp_w1[!is.na(dummySoc),]
tmp_w1[,razao_racial := cor_branca / cor_negra]
tmp_w1[,razao_racial_adj := razao_racial / min(razao_racial),by = city]

tmp_w1[city == "cam"]

# plot
tmp_city_order <- data.table::copy(tmp_w1) %>% 
  #.[decil_status  == "Pobre"] %>% 
  .[dummySoc   == "Alta Branca"] %>% 
  .[order(razao_racial_adj),city]
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
tmp_city_labels <- dt_tmp_city[order(match(levels,tmp_city_order)),] %>% 
  .[1:length(tmp_city_order),labels]

tmp_w1[,city_f := factor(city
                           ,levels = tmp_city_order
                           ,labels = tmp_city_labels)]

tmp_w1[city=="spo"]
# plot

ggplot() + 
  geom_point(data = tmp_w1
             ,aes(x = razao_racial_adj,y = city_f
                  ,color = dummySoc )
             ,size=2.5
             ,alpha = 1,shape = 1,stroke = 2.)+
  scale_colour_aop(palette = "clevel")+
  geom_vline(xintercept = 1,linetype = "dashed")+
  labs(x = "Razão cor-renda / pop. negra de baixa renda"
       , y = NULL
       #, color = "Nível de renda"
       , color = "Renda / Cor"
       , title = "Razão população conforme cor e renda"
       , subtitle = "Proporção entre população conforme cor/raça e nível de renda \najustado pela pop. negra de baixa renda")+
  theme(legend.position = c(0.875,0.2))
  
  
  ggplot() + 
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

TMISA_transit_plot + CMASA30_transit_plot +
  plot_annotation(tag_levels = 'I',tag_prefix = "(",tag_suffix = ")")
ggsave(filename = "figures/CMASA30_TMI_transit_20-20rico.png",
       width = 34,height = 20,scale = 0.6
       ,units = "cm",bg = "white",dpi = 300)
