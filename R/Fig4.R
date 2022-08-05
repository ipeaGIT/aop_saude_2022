####################################################################################################################
################################### SCRIPT PARA GERAR MAPAS TMI ####################################################
####################################################################################################################
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
library(patchwork)

######### utilizado para calcular CMASA60 para TP
for_ <- aopdata::read_access(city="for", geometry = T)
cur_ <- aopdata::read_access(city="cur", geometry = T)
rio_ <- aopdata::read_access(city="rio", geometry = T)


#####

aop <- rbind(for_,cur_,rio_)
aop <- aop %>%
  st_as_sf()%>%
  st_set_crs(4326) %>%
  st_transform(3857) %>%
  filter(abbrev_muni %in% c("for", "cur", "rio"),
         mode == "walk",
         peak ==1,
         !is.infinite("TMISB"),
         !is.na("TMISB"),
         P001 >0) %>%
  mutate(TMISB = ifelse(abbrev_muni == "rio" & TMISB > 30, 30, TMISB),
         TMISB = ifelse(abbrev_muni != "rio" & TMISB > 30, 30, TMISB)) %>%
  dplyr::select(id_hex, abbrev_muni,mode, TMISB, TMISM, TMISA, CMASA30, CMASA15, CMASA60)

map_tiles_cur <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_cur_2019.rds"))
map_tiles_for <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_for_2019.rds"))
map_tiles_rio <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_rio_2019.rds"))


######## plots ######

plot_cur <- ggplot()+
  geom_raster(data = map_tiles_cur, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = subset(aop, abbrev_muni == "cur"), aes(fill = TMISB), color = NA, alpha=.7)+
  viridis::scale_fill_viridis(direction = -1
                              , limits = c(0, 30)
                              , breaks = c(0, 10, 20, 30)
                              , labels = c("0", "10", "20", "+30min")
  ) +
  
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("black","white"))+
  
  labs(fill = "Número de \noportunidades\n",
    title = paste0("(A) Curitiba"))+
  
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())



####################

plot_for <- ggplot()+
  geom_raster(data = map_tiles_for, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = subset(aop, abbrev_muni == "for"), aes(fill = TMISB), color = NA, alpha=.7)+
  viridis::scale_fill_viridis(direction = -1
                              , limits = c(0, 30) 
                              , breaks = c(0, 10, 20, 30)
                              , labels = c("0", "10", "20", "+30min")
  ) +
  
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("black","white"))+
  
  labs(fill = "Número de \noportunidades\n",
    title = paste0("(B) Fortaleza"))+
  
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


##################

plot_rio <- ggplot()+
  geom_raster(data = map_tiles_rio, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = subset(aop, abbrev_muni == "rio"), aes(fill = TMISB), color = NA, alpha=.7)+
  viridis::scale_fill_viridis(direction = -1
                              , limits = c(0, 30) 
                              , breaks = c(0, 10, 20, 30)
                              , labels = c("0", "10", "20", "+30min")
  ) +
  
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("black","white"))+
  
  ggspatial::annotation_north_arrow(
    location="br", which_north = "true",
    pad_x=unit(0.4,"in"), pad_y = unit(0.4,"in"),
    style = ggspatial::north_arrow_fancy_orienteering()
  )+
  
  labs(fill = "Tempo mínimo \npara acesso \n(Minutos)\n",
       title = paste0("(C) Rio de Janeiro"))+
  
  theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

##################

ggdraw(xlim = c(0,28), ylim = c(0,20))+
  draw_plot(plot_cur+theme(legend.position = 'none'), x=2, y=10, width = 14, height = 10)+
  draw_plot(plot_for+theme(legend.position = 'none'), x=10, y=10, width = 14, height = 10)+
  draw_plot(plot_rio+theme(legend.position = 'right'), x=7, y=1, width = 16, height = 10)#+


ggsave2(filename="Fig4.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 32, height = 22, units = "cm")


