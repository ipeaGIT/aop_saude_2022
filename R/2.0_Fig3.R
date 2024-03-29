
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


for_ <- aopdata::read_access(city="for", mode = "public_transport",geometry = T)
cur_ <- aopdata::read_access(city="cur", mode = "public_transport", geometry = T)
rio_ <- aopdata::read_access(city="rio", mode = "public_transport", geometry = T)



aop <- rbind(for_,cur_,rio_)

##### map TMISA #####
TMI <- function(cidade, modo, medida, legenda, maxtempo){
  aop_filter <- aop %>%
    st_as_sf()%>%
    st_set_crs(4326) %>%
    st_transform(3857) %>%
    filter(abbrev_muni == cidade,
           mode == modo,
           peak ==1,
           !is.infinite(medida),
           !is.na(medida),
           P001 >0) %>%
    mutate(TMISA = ifelse(TMISA > maxtempo, maxtempo, TMISA)) %>%
    dplyr::select(id_hex, abbrev_muni,mode, TMISB, TMISM, TMISA, CMASA30, CMASA15, CMASA60)
  
  map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_renda <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=aop_filter, aes(fill=TMISA), color=NA, alpha=1) +
    viridis::scale_fill_viridis(
      direction = -1
      , option = "viridis"
      , limits = c(0, maxtempo)
      , breaks = c(0, 20, 40, 60)
      , labels = c("0", "20", "40","+60 min")
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA) + 
    labs(title = "Minimum travel time",
         fill = "Minimum\ntravel time") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom")
    }
  else if(legenda == "nao"){
    theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
  }
  else if(legenda == "titulo"){
    theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
  }
}


####### Acessibility map ######

CMA <- function(cidade, modo, medida, legenda, maxtempo){
  
  aop_filter <- aop %>%
    st_as_sf()%>%
    st_set_crs(4326) %>%
    st_transform(3857) %>%
    filter(abbrev_muni == cidade,
           mode == modo,
           peak ==1,
           !is.infinite(medida),
           !is.na(medida),
           P001 >0) %>%
    mutate(CMASA60 = ifelse(CMASA60 > maxtempo, maxtempo, CMASA60)) %>%
    dplyr::select(id_hex, abbrev_muni,mode, TMISB, TMISM, TMISA, CMASA30, CMASA15, CMASA60)
  
  aop_filter <- aop_filter %>%
    mutate(quintil = as.character(ntile(CMASA60, 5)))
  
  if (cidade == "for"){
    aop_filter <- aop_filter %>%
      mutate(quintil = case_when(CMASA60 == 40 ~ "5",
                                 CMASA60 != 40 ~ quintil))
  }
  
  map_tiles <- readRDS(paste0("data-raw/maptile_crop_mapbox_", cidade,"_2019.rds"))
  
  mapa_renda <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity()+
    # nova escala
    new_scale_fill() + 
    geom_sf(data=aop_filter, aes(fill=quintil), color=NA, alpha=1) +
    scale_fill_viridis_d(
      breaks = c("1","2","3","4","5")
      , direction = 1
      , option = "cividis"
    ) +
    annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0, "cm")) +
    coord_sf(datum=NA) + 
    labs(title = "Number of healthcare facilities",
         fill = "Number of healthcare \nfacilities (quintile)") +
    if(legenda == "sim"){
      theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "bottom")
    }
  else if(legenda == "nao"){
    theme(legend.title = element_blank(), plot.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
  }
  else if(legenda == "titulo"){
    theme(legend.title = element_text(size = 14, face="bold",family = "serif"), plot.title = element_text(size = 18, face = "bold", hjust=0.5,family = "serif"), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank(), legend.position = "none")
  }
}


######## Rodando mapas

TMI_cur <- TMI("cur", "public_transport","TMISA","titulo",60)
TMI_for <- TMI("for","public_transport","TMISA", "nao", 60)
TMI_rio <- TMI("rio", "public_transport","TMISA", "sim", 60)

cma_cur <- CMA("cur", "public_transport","CMASA60","titulo",40) ### maximo de oportunidades em curitiba ? de 36
cma_for <- CMA("for","public_transport","CMASA60", "nao", 40)
cma_rio <- CMA("rio", "public_transport","CMASA60", "sim", 40)




ggdraw(xlim = c(0,40), ylim = c(0,65))+
  draw_plot(TMI_cur, x=0, y=40, width = 20, height = 25)+
  draw_plot(TMI_for, x=0, y=20, width = 20, height = 20)+
  draw_plot(TMI_rio, x=0, y=00, width = 20, height = 20)+
  draw_plot(cma_cur, x=15, y=40, width = 20, height = 25)+
  draw_plot(cma_for, x=15, y=20, width = 20, height = 20)+
  draw_plot(cma_rio, x=15, y=00, width = 20, height = 20)+
  draw_label("Curitiba", size = 18, fontface = "bold", x=34, y=52,angle = 270,fontfamily = "serif")+
  draw_label("Fortaleza", size = 18, fontface = "bold", x=34, y=30,angle = 270,fontfamily = "serif")+
  draw_label("Rio de Janeiro", size = 18, fontface = "bold", x=34,y=12,angle = 270,fontfamily = "serif")


setwd("~")
ggsave2(filename="figures/Fig3.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 35, height = 30, units = "cm")


