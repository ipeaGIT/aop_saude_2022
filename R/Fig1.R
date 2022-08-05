##################################################################################################################
############################# SCRIPT PARA CRIAR MAPAS DE FORTALEZA E GOIÂNIA #####################################
##################################################################################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(ggalt)
library(hrbrthemes)
library(ggnewscale)
library(cowplot)
library(purrr)
library(ggsn)
library(BAMMtools) 
library(stringi)
library(ggspatial)
library(aopdata)
library(geobr)


#setwd("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/hex_agregados/2019")

##### mapa de distribuicao de estabelecimentos de saúde #####
distrib_saude <- function(cidade, legenda){

  est_saude <- aopdata::read_landuse(cidade, 2019,geometry = T) %>%
    st_transform(3857)
  
  baixa <- subset(est_saude, S002>0)
  baixa <- st_sample(x=est_saude, est_saude$S002, by_polygon=T)
  baixa <- st_sf(baixa)
  baixa$geometry <- st_geometry(baixa)
  baixa$baixa <- NULL
  baixa <- st_sf(baixa)
  
  alta <- subset(est_saude, S004>0)
  alta <- st_sample(x=est_saude, est_saude$S004, by_polygon=T)
  alta <- st_sf(alta)
  alta$geometry <- st_geometry(alta)
  alta$alta <- NULL
  alta <- st_sf(alta)
  
  lim <- geobr::read_municipality(code_muni = ifelse(cidade =="for", 2304400,
                                                     ifelse(cidade == "cur", 4106902,
                                                            3304557)))
  lim <- lim %>% st_transform(3857)
   
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_renda <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=lim, fill = NA, color="black", alpha=1)+
    geom_sf(data=baixa, aes(colour="B"), show.legend = "point")+
    geom_sf(data=alta, aes(colour="A"), show.legend = "point")+
    scale_colour_manual(name = "",
                        values = c("A" = "red", "B" = "#313695"),
                        labels = c("Alta Complexidade", "Atenção Básica"),
                      guide = guide_legend(override.aes = list(linetype = "blank", shape=c(16,16))))+
    annotation_scale(location = "br", width_hint = 0.2) +
    coord_sf(datum=NA, xlim = c(min(map_tiles$x), max(map_tiles$x)), ylim = c(min(map_tiles$y), max(map_tiles$y))) + 
    labs(title = "Distribuição de\nestabelecimentos de saúde",
         fill = "Quintil\nde renda  ") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 12, face="bold"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom", legend.text = element_text(size=12))
    }
  else if(legenda == "nao"){
    theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
  }
  else if(legenda == "titulo"){
    theme(legend.title = element_text(size = 12, face="bold"), plot.title = element_text(size = 16, face = "bold", hjust=0.5), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
  }
}


saude_cur <- distrib_saude("cur", "titulo")
saude_for <- distrib_saude("for", "nao")
saude_rio <- distrib_saude("rio", "sim")



##### mapa de distribuicao de renda #####

distrib_renda <- function(cidade, legenda){

  df <- aopdata::read_landuse(city=cidade, geometry = T)%>%
    st_as_sf()%>%
    st_set_crs(4326) %>%
    st_transform(3857) %>%
    mutate(quintil = as.character(R002))
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  
  mapa_renda <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=subset(df,df$quintil >0), aes(fill=quintil), color=NA, alpha=1) +
    scale_fill_manual(values = c("1"="#a50026",
                                 "2"="#f46d43",
                                 "3"="#fee090",
                                 "4"="#74add1",
                                 "5"="#313695"))+
    annotation_scale(location = "br", width_hint = 0.2) +
    coord_sf(datum=NA) + 
    labs(title = "Distribuição de renda",
         fill = "Quintil\nde renda  ") +
    if(legenda == "sim"){
    theme(legend.title = element_text(size = 12, face="bold"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom")
    }
    else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }
    else if(legenda == "titulo"){
    theme(legend.title = element_text(size = 12, face="bold"), plot.title = element_text(size = 16, face = "bold", hjust=0.5), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
  }
}


renda_cur <- distrib_renda("cur", "titulo")
renda_for <- distrib_renda("for", "nao")
renda_rio <- distrib_renda("rio", "sim")



####### Mapa de distribuicao populacao negra ######

distrib_pop_negra <- function(cidade, legenda){
  
  df <- aopdata::read_landuse(city=cidade, geometry = T)%>%
    st_as_sf()%>%
    st_set_crs(4326) %>%
    st_transform(3857)%>%
    filter(P001 > 0) %>%
    mutate(prop_pop_negra = (P003)/P001)
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_pop_negra <- ggplot() +
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=subset(df,P001>0), aes(fill=prop_pop_negra), color=NA, alpha=1) +
    viridis::scale_fill_viridis(
      direction = 1
      , option = "inferno"
      , limits = c(0, 1)
      , breaks = c(0, 0.25, 0.5, 0.75, 1)
      , labels = c("0%", "25%", "50%", "75%","100%")
    ) +
    coord_sf(datum=NA)+
    labs(title = "Distribuição da população negra",
         fill = "Proporção de\npopulação negra  ",)+
    annotation_scale(location = "br", width_hint = 0.2) +
    
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 12, face="bold"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "bottom")
    }
    else if(legenda == "nao"){
      theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
    }
    else if(legenda == "titulo"){
      theme(legend.title = element_text(size = 12, face="bold"), plot.title = element_text(size = 16, face = "bold", hjust=0.5), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(),legend.position = "none")
    }
}



pop_negra_cur <- distrib_pop_negra("cur","titulo")
pop_negra_for <- distrib_pop_negra("for", "nao")
pop_negra_rio <- distrib_pop_negra("rio","sim")


####################################################################


ggdraw(xlim = c(0,50), ylim = c(0,65))+
  draw_plot(saude_cur, x=0, y=40, width = 20, height = 25)+
  draw_plot(saude_for, x=0, y=20, width = 20, height = 20)+
  draw_plot(saude_rio, x=0, y=00, width = 20, height = 20)+
  draw_plot(renda_cur, x=15.3, y=40, width = 20, height = 25)+
  draw_plot(renda_for, x=15.3, y=20, width = 20, height = 20)+
  draw_plot(renda_rio, x=15.3, y=00, width = 20, height = 20)+
  draw_plot(pop_negra_cur, x=30, y=40, width = 20, height = 25)+
  draw_plot(pop_negra_for, x=30, y=20, width = 20, height = 20)+
  draw_plot(pop_negra_rio, x=30, y=00, width = 20, height = 20)+
  draw_label("Curitiba", size = 16, fontface = "bold", x=48, y=52,angle = 270)+
  draw_label("Fortaleza", size = 16, fontface = "bold", x=48, y=30,angle = 270)+
  draw_label("Rio de Janeiro", size = 16, fontface = "bold", x=48 ,y=12,angle = 270)


setwd("~")
ggsave2(filename="Fig1_TD_SAUDE_v2_15_06_2022.png", plot=ggplot2::last_plot(),
       dpi = 300, width = 42, height = 30, units = "cm")



