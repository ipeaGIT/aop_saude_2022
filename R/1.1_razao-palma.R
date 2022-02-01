


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


# select files ------
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

# read files
tmp_acc <- lapply(files_acess,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)
tmp_soc <- lapply(files_socio,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)
tmp_car <- lapply(files_acess_car,readr::read_rds) %>% data.table::rbindlist(fill=TRUE)

tmp_soc[1,]
# joint dataset ----
# add socio into car
tmp_join_car <- data.table::setDT(tmp_car)[
  data.table::setDT(tmp_soc), on = c("origin"="id_hex"
                                     ,"city" = "sigla_muni"
                                     ,"ano")
  ,`:=`(decil  = decil 
        ,quintil  = quintil
        ,pop_total = pop_total )] 
tmp_join_car <- tmp_join_car[,.(origin,city,mode
                                ,pico,CMASA30,CMASB30
                                ,geometry
                                ,ano
                                ,decil
                                ,quintil
                                ,pop_total )]
# add socio into acc
tmp_join_acc <- data.table::setDT(tmp_acc)[
  data.table::setDT(tmp_soc), on = c("origin"="id_hex"
                                     ,"city" = "sigla_muni"
                                     ,"ano")
  ,`:=`(decil  = decil 
        ,quintil  = quintil
        ,pop_total = pop_total )] 
tmp_join_acc <- tmp_join_acc[,.(origin,city,mode
                                ,pico,CMASA30,CMASB30
                                ,geometry
                                ,ano
                                ,decil
                                ,quintil
                                ,pop_total )]
# rbind
tmp_join <- rbind(tmp_join_car,tmp_join_acc)
# format wide-to-long
tmp_melt <- data.table::melt(tmp_join
                             ,measure.vars = list(c("decil","quintil"))
                             ,variable.name = "codigo"
                             ,value.name = "nivel_renda")
tmp_melt <- tmp_melt[!(is.na(nivel_renda) | is.nan(nivel_renda)),]

tmp_w <- data.table::copy(tmp_melt)[,weighted.mean(x = CMASA30
                                                   ,w = pop_total,na.rm = TRUE)
                                    ,by = .(city,mode,pico,ano,codigo,nivel_renda)]

tmp_w <- tmp_w[codigo == "decil"] %>% 
  .[nivel_renda == 9 | nivel_renda == 4,] %>% 
  .[, nivel_renda := paste0("q",nivel_renda)] %>% 
  .[pico == 1,]

tmp_w1 <- data.table::dcast(data = tmp_w,
                  formula = city + mode + pico + ano ~ nivel_renda
                  , value.var = "V1") 
tmp_w1[,r_palma := q9/q4]

tmp_city_order <- data.table::copy(tmp_w1) %>% 
  .[mode == "car" & ano == 2019] %>% 
  .[order(r_palma),city]


ggplot(tmp_w1[ano == 2019])+
  geom_bar(aes(x = r_palma,y = city),stat = "identity")+
  facet_wrap(~mode,scales = "free")

p_car <- ggplot()+
  geom_bar(data = tmp_w1[mode == "car" & ano == 2019]
           ,aes(x = r_palma,y = city),stat = "identity")
p_car

tmp_city_order <- data.table::copy(tmp_w1) %>% 
  .[mode == "transit" & ano == 2019] %>% 
  .[order(r_palma),city]

tmp_w1[,city := factor(city,tmp_city_order)]

p_bus <- ggplot()+
  geom_bar(data = tmp_w1[mode == "transit" & ano == 2019]
           ,aes(x = r_palma,y = city),stat = "identity")
p_bus










