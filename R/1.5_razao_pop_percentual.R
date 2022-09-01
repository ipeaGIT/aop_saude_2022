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
setnames(tmp_join_raw,"peak","pico")
setnames(tmp_join_raw,"sigla_muni","city")

# initial filter
tmp_join <- data.table::copy(tmp_join_raw) %>%  
  .[pop_total > 0,] %>% 
  .[mode == "walk",] %>% 
  .[pico == 1,] %>% 
  .[decil %in% c(9,10),decil_status := "Rica"] %>% 
  .[decil %in% c(1:2),decil_status := "Pobre"]


tmp_join %>% 
  .[decil %in% c(1,2,9,10),] %>% 
  .[,.SD[1],by=.(decil,city)] %>% 
  .[,.SD,.SDcols = c("city","decil","renda_capita")] %>% 
  data.table::setorder(.,city,decil) %>% 
  .[]

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
tmp_join[,razao_racial := total_by_color / negra_pobre]
tmp_join[,razao_racial_v1 := data.table::fifelse(
  razao_racial < 1, (- 1 /razao_racial) + 1, razao_racial - 1) ]

# plot 1 ------

# find city order
tmp_city_order <- data.table::copy(tmp_join) %>% 
  .[decil_status  == "Rica" & cor == "cor_branca"] %>% 
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


tmp_join[,":="(min = min(razao_racial_v1)
                ,max = max(razao_racial_v1)),by = city_f]

breaks_x <-  max(abs(round(tmp_join$razao_racial_v1,0)))
breaks_x <- (-breaks_x):(breaks_x)

labels_x <- ifelse(breaks_x < 0,breaks_x-1,breaks_x+1)
labels_x <- ifelse(labels_x < 0,labels_x*-100,labels_x*100)
labels_x <- paste0(labels_x,"%")



# plot
ggplot() + 
  # seguimento
  geom_segment(data = tmp_join
               ,aes(x = min,xend = max
                    ,y = city_f,yend = city_f),color = "grey",size = 1.0)+
  # add points
  geom_point(data = tmp_join
             ,aes(x = razao_racial_v1,y = city_f
                  ,color = label)
             ,size=2.5,alpha = 1
             ,shape = 21,stroke = 2.) +
  # add pallete and x_continuous
  scale_colour_aop(palette = "clevel",reverse = TRUE) +
 # scale_colour_manual(
 #   values = c( "royalblue"
 #               ,"skyblue4"
 #               ,"darkorange"
 #               ,"orange3")
 # )+
  scale_x_continuous(
    labels = labels_x[c(1,3,5,8,11,13,15)]
    ,breaks = breaks_x[c(1,3,5,8,11,13,15)]
    ,limits = c(min(breaks_x),max(breaks_x))
  ) +
  # add vertical line
  geom_vline(xintercept = 0,linetype = "dashed")+
  # labs
  labs(
    #x = "População x vezes a população negra de baixa renda"
    x = "\n\n\nProporção da população ajustada\n pela pop. negra de baixa renda "
    , y = NULL
    , color = "Renda e Cor/raça"
    , title = "Razão populacional conforme renda e cor/raça"
    )+
  theme_bw()+
  theme(legend.position = c(0.875,0.2)
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.y = element_blank()
        ,plot.margin = margin(t = 0.1,r = 0.25
                              ,b = 0.75,l = .85, "cm")
  )+
  scale_y_discrete(expand = c(0.0,0.0))+
  coord_cartesian(ylim = c(0.5
                           ,uniqueN(tmp_join$city) + 0.5)
                  , clip = "off") +
  annotate("segment"
           , x = 0.5
           , y = -1
           , xend = 3
           , yend = -1
           , arrow = grid::arrow(length = unit(0.02, "npc"), type = "closed")
           ) +
  annotate("text"
           , x = 0.5
           , y = -1
           , label = bquote("População"~bold("maior")~"(em %)")
           , vjust = 1.5
           , hjust = 0
           , size = 3)+
  # negativo
  annotate("segment"
           , x = -0.5 
           , y = -1
           , xend = -3
           , yend = -1
           , arrow = grid::arrow(length = unit(0.02, "npc"), type = "closed")
  ) +
  annotate("text"
           , x = -4.15
           , y = -1
           , label = bquote("População"~bold("menor")~"(em %)")
           , vjust = 1.5
           , hjust = 0
           , size = 3)



ggsave(filename = "figures/razao_populacional_v1.jpg",
       width = 30,height = 25,scale = 0.6,units = "cm",bg = "white",dpi = 300)

# PREP 2----

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

tmp_join_raw <- readr::read_rds("data/socio_acc-all_2019.rds")  # source("R/0.0_join_acess-soc.R")
setnames(tmp_join_raw,"peak","pico")
setnames(tmp_join_raw,"sigla_muni","city")

# initial filter
tmp_join <- data.table::copy(tmp_join_raw) %>%  
  .[pop_total > 0,] %>% 
  .[mode == "walk",] %>% 
  .[decil %in% c(9,10),decil_status := "Rica"] %>% 
  .[decil %in% c(1:2),decil_status := "Pobre"] %>% 
  .[!is.na(decil_status),]



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

tmp_join <- tmp_join[
  ,{
    prop_by_status = total_by_color / sum(total_by_color)
    prop_by_status = round(100 * prop_by_status,2)
    list(cor,total_by_color,prop_by_status)
    }
  , by = .(city,decil_status)]


tmp_join
# remove unecessary data
#tmp_join <- tmp_join %>% 
#  .[cor %in% c("cor_branca","cor_negra"),]

# verify data
tmp_join[,list(sum(total_by_color),sum(prop_by_status)),by = .(city,decil_status)]


# plot 2------

# find city order
tmp_city_order <- data.table::copy(tmp_join) %>% 
  .[decil_status  == "Rica" & cor == "cor_branca"] %>% 
  .[order(prop_by_status),city]

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

tmp_join[,cor_f := cor]
tmp_join[cor_f %in% c("cor_amarela","cor_indigena")
         ,cor_f := "amarela_indigena"]
tmp_join[,cor_f := factor(cor_f
                          ,levels = c(c("cor_branca","cor_negra","amarela_indigena"))
                          ,labels = c(c("Branca","Negra","Amarela e \nindígena")))]

tmp_join[,decil_status_f := factor(decil_status
                          ,levels = c("Pobre","Rica")
                          ,labels = c("Baixa Renda","Alta Renda"))]
# plot
myplot <- ggplot(tmp_join) + 
  geom_bar(aes(y = city_f, x= prop_by_status , fill = cor_f)
           ,stat = "identity")+
  facet_wrap(~decil_status_f)+
  scale_fill_manual(values = c(c('#C29365','#6A9BB3','#515A5A')))+
  # labs
  labs(
    #x = "População x vezes a população negra de baixa renda"
    x = "Proporção da população (%)"
    , y = NULL
    , fill = "Cor/raça"
    #, title = "Razão populacional conforme renda e cor/raça"
  )+
  geom_vline(data =  data.frame(x = c(25,50,75))
             ,aes(xintercept = x)
                 ,linetype = "dashed", alpha = 0.25)+
  coord_cartesian(expand = c(0.1))+
  aop_style_black()+
  theme(legend.position = "bottom")

myplot
#### animals
## qualitativo
#`clevel1`       = '#C29365',  # red leve+
#`clevel2`       = '#620C1A',  # red forte
#`clevel3`      = '#6A9BB3',   # blue leve+
#`clevel4`       = '#111F4F',  # blue forte
#
## cleveland2
#`vclevel1`       = '#FF3A3A', # red leve
#`vclevel2`       = '#839192', # cinza leve
#`vclevel3`       = '#8F0303', # red forte
#`vclevel4`       = '#515A5A', # cinza forte

ggsave(filename = "figures/fig2.pdf",
       width = 30,height = 27,scale = 0.6,units = "cm",bg = "white",dpi = 300)
ggsave(filename = "figures/fig2.png",
       width = 30,height = 27,scale = 0.55,units = "cm",bg = "white",dpi = 300)

# PREP 3----

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

tmp_join_raw <- readr::read_rds("data/socio_acc-all_2019.rds")  # source("R/0.0_join_acess-soc.R")

# initial filter
tmp_join <- data.table::copy(tmp_join_raw) %>%  
  .[pop_total > 0,] %>% 
  .[mode == "walk",] %>% 
  .[pico == 1,] 



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
    ,by = .(city,decil,cor)
    ,.SDcols = c("total_by_color")
  ]

# remove unecessary data
tmp_join <- tmp_join %>% 
  .[cor %in% c("cor_branca","cor_negra"),]

tmp_join <- tmp_join[
  ,{
    prop_by_status = total_by_color / sum(total_by_color)
    prop_by_status = round(100 * prop_by_status,2)
    list(decil,total_by_color,prop_by_status)
  }
  , by = .(city,cor)]



tmp_join[,sum(prop_by_status),by = .(city,cor)]



# plot 3------

# find city order
tmp_city_order <- data.table::copy(tmp_join) %>% 
  .[decil  == 10 & cor == "cor_branca"] %>% 
  .[order(prop_by_status),city]

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

tmp_join <- tmp_join[decil != 0,]
tmp_join[,cor_f := factor(
  x = cor
  ,levels = c("cor_branca","cor_negra")
  ,labels = c("Branca","Negra")
)]
tmp_join[,decil_f := factor(
  x = decil
  ,levels = 10:0
)]
tmp_join[,city_f := factor(
  x = city
  ,levels = tmp_city_order
  ,labels = tmp_city_labels
)]



# plot
ggplot(tmp_join) + 
  geom_bar(aes(y = city_f, x= prop_by_status , fill = decil_f)
           ,stat = "identity")+
  facet_wrap(~cor_f)+
  scale_fill_aop(palette = "clevel") +
  # labs
  labs(
    #x = "População x vezes a população negra de baixa renda"
    x = "Proporção da população conforme faixa de renda (%)"
    , y = NULL
    , fill = "Decil \n(Maior - Menor)"
    , title = "Razão populacional conforme renda e cor/raça"
  )+
  coord_cartesian(expand = c(0.1))+
  theme_bw()



ggsave(filename = "figures/razao_populacional_v3.jpg",
       width = 30,height = 25,scale = 0.6,units = "cm",bg = "white",dpi = 300)
#end -----