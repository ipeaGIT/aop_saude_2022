


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

tmp_join_acc <- readr::read_rds("data/socio_acc-all.rds")


names(tmp_join_acc)
data.table::setnames(x = tmp_join_acc
                     ,old = c("P001","P002","P003","P004","P005","R001","R002","R003")
                     ,new = c("total_pop","cor_branca","cor_negra"
                              ,"cor_indigena","cor_amarela","renda","quintil","decil"))
tmp_join_acc[,ano := 2019]
tmp_join_acc <- tmp_join_acc[!is.na(total_pop)]

# joint dataset ----
# add socio into car
tmp_join_acc <- tmp_join_acc[,.(id_hex,abbrev_muni,name_muni,mode
                                ,CMASA30,CMASM30,CMASB30
                                ,geometry
                                ,ano
                                ,cor_branca
                                ,cor_amarela
                                ,cor_indigena
                                ,cor_negra
                                ,renda
                                ,quintil
                                ,decil)]

# format wide-to-long
tmp_melt <- data.table::melt(data.table::copy(tmp_join_acc)
                             ,measure.vars = list(c("cor_branca","cor_negra"
                                                    ,"cor_amarela","cor_indigena"))
                             ,variable.name = "cor"
                             ,value.name = "total")
tmp_melt[is.na(total) | is.nan(total),total := 0]

# decil status
tmp_melt[decil %in% 10,decil_status := "10p_ricos"]
tmp_melt[decil %in% c(1:4),decil_status := "40p_pobres"]

# weighted average
tmp_w <- data.table::copy(tmp_melt)[,weighted.mean(x = CMASA30
                                                   ,w = total,na.rm = TRUE)
                                    ,by = .(name_muni,mode,cor,decil_status)]

# melt
tmp_w1 <- data.table::dcast(data = tmp_w[!is.na(decil_status)],
                            formula = name_muni + mode + cor ~ decil_status
                            , value.var = "V1") 
tmp_w1[,r_palma := `10p_ricos`/`40p_pobres`]

# tmp_city_order <- data.table::copy(tmp_w1) %>% 
#   .[mode == "car" & ano == 2019] %>% 
#   .[order(r_palma),city]


ggplot(tmp_w1[cor %in% c("cor_branca","cor_negra")])+
  geom_bar(aes(x = r_palma,y = name_muni,fill = cor)
           ,stat = "identity",position = "dodge")+
  facet_grid(cols = vars(mode),scales = "free")+
  labs(x = "razao de palma"
       ,title = "Acesso a Saúde de Alta Complexidade (2019)"
       ,y = "municipios",fill = "Cor/Raça")
