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

# cities
tmp_join_raw$name_muni %>% unique()

tmp_join_raw[name_muni == "Sao Luis"   ,name_muni := "São Luís"]
tmp_join_raw[name_muni == "Belem"      ,name_muni := "Belém"]
tmp_join_raw[name_muni == "Maceio"     ,name_muni := "Maceió"]
tmp_join_raw[name_muni == "Sao Paulo"  ,name_muni := "São Paulo"]
tmp_join_raw[name_muni == "Sao Gonçalo",name_muni := "São Gonçalo"]
tmp_join_raw[name_muni == "Goiania"    ,name_muni := "Goiânia"]
tmp_join_raw[name_muni == "Brasilia"   ,name_muni := "Brasília"]
# initial filter
tmp_join <- data.table::copy(tmp_join_raw) %>%  
  .[pop_total > 0,] %>% 
  .[mode == "walk",] %>% 
  .[peak == 1,] %>% 
  .[decil %in% c(9,10),decil_status := "Rica"] %>% 
  .[decil %in% c(1:2),decil_status := "Pobre"]



# organize data in columns
tmp_join <- data.table::melt(tmp_join
                             ,measure.vars = list(c("cor_branca","cor_negra"
                                                    ,"cor_amarela","cor_indigena"))
                             ,variable.name = "cor"
                             ,value.name = "total_by_color")

# sum population by (name_muni,mode,pico,ano,cor)
tmp_join <- tmp_join %>% 
  .[,
    lapply(.SD,sum,na.rm = TRUE)
    ,by = .(name_muni ,decil_status,cor)
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
  
},by = name_muni]


# verify data
tmp_join[name_muni == "São Paulo"]

# razao racial
tmp_join[,razao_racial := total_by_color / negra_pobre]

tmp_join[name_muni == "cam"]

# plot ------

# find name_muni order
tmp_name_muni_order <- data.table::copy(tmp_join) %>% 
  .[decil_status  == "Rica" & cor == "cor_branca"] %>% 
  .[order(razao_racial),name_muni]

# add label
tmp_join[cor == "cor_branca" & decil_status == "Rica" , label := "Alta Branca" ]
tmp_join[cor == "cor_branca" & decil_status == "Pobre", label := "Baixa Branca"]
tmp_join[cor == "cor_negra"  & decil_status == "Rica" , label := "Alta Negra"  ]
tmp_join[cor == "cor_negra"  & decil_status == "Pobre", label := "Baixa Negra" ]

# verify data for spo
tmp_join[name_muni == "spo"]


tmp_join[,{list(min = min(razao_racial)
                ,max = max(razao_racial))},by = name_muni]
# plot
ggplot() + 
  # seguimento
  geom_segment(data = tmp_join[,{list(min = min(razao_racial)
                                      ,max = max(razao_racial))}
                               ,by = name_muni]
               ,aes(x = min,xend = max
                    ,y = name_muni,yend = name_muni),color = "grey",size = 1.0)+
  # add points
  geom_point(data = tmp_join
             ,aes(x = razao_racial,y = name_muni
                  ,color = label)
             ,size=2.5,alpha = 1
             ,shape = 21,stroke = 2.) +
  # add pallete and x_continuous
  scale_colour_aop(palette = "clevel",reverse = TRUE) +
  scale_x_continuous(
    labels = sprintf("%sx",
                     seq(0,max(round(tmp_join$razao_racial,0))
                         ,by = 0.5))
    ,breaks = seq(0
                  ,max(round(tmp_join$razao_racial,0))
                  ,by = 0.5)
  ) +
  # add vertical line
  #geom_vline(xintercept = 1,linetype = "dashed")+
  # labs
  labs(
    x = "População x vezes a população negra de baixa renda"
    , y = NULL
    , color = "Renda e Cor/raça"
    , title = "Razão populacional conforme renda e cor/raça"
    #, subtitle = "Proporção entre população negra de baixa renda sobre\npopulação por cor/raça - renda"
  )+
  # adjust legend.position 
  #aop_style()+
  theme_bw()+
  theme(legend.position = c(0.875,0.175)
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.y = element_blank()
        #, panel.border = element_blank()
        #, panel.spacing.x = unit(1, "cm")
        #, panel.spacing.y = unit(0, "cm")
        )

tmp_join

ggsave(filename = "figures/razao_populacional.jpg",
       width = 30,height = 25,scale = 0.6,units = "cm",bg = "white",dpi = 300)

