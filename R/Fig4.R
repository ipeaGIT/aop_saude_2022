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

setwd("~/data2")

######### utilizado para calcular CMASA60 para TP
for_ <- readRDS("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/output_access/2019/tp_active/acess_2019_for_all_access-all.rds")
cur_ <- readRDS("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/output_access/2019/tp_active/acess_2019_cur_all_access-all.rds")
rio_ <- readRDS("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/output_access/2019/tp_active/acess_2019_rio_all_access-all.rds")


######## join com os dados socioeconomicos por hexagono ########## 
for_hex <- readRDS("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/hex_agregados/2019/hex_agregado_for_09_2019.rds") %>% st_drop_geometry()
cur_hex <- readRDS("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/hex_agregados/2019/hex_agregado_cur_09_2019.rds") %>% st_drop_geometry()
rio_hex <- readRDS("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/hex_agregados/2019/hex_agregado_rio_09_2019.rds") %>% st_drop_geometry()

for_ <- dplyr::left_join(for_, for_hex, by=c("origin"="id_hex"))
cur_ <- dplyr::left_join(cur_, cur_hex, by=c("origin"="id_hex"))
rio_ <- dplyr::left_join(rio_, rio_hex, by=c("origin"="id_hex"))


#####

aop <- rbind(for_,cur_,rio_)
aop <- aop %>%
  st_as_sf()%>%
  st_set_crs(4326) %>%
  st_transform(3857) %>%
  filter(city %in% c("for", "cur", "rio"),
         mode == "walk",
         pico ==1,
         !is.infinite("TMISB"),
         !is.na("TMISB"),
         pop_total >0) %>%
  mutate(TMISB = ifelse(city == "rio" & TMISB > 30, 30, TMISB),
         TMISB = ifelse(city != "rio" & TMISB > 30, 30, TMISB)) %>%
  dplyr::select(origin, city,mode, TMISB, TMISM, TMISA, CMASA30, CMASA15, CMASA60, CMASA90)

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
  geom_sf(data = subset(aop, city == "cur"), aes(fill = TMISB), color = NA, alpha=.7)+
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
  geom_sf(data = subset(aop, city == "for"), aes(fill = TMISB), color = NA, alpha=.7)+
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
  geom_sf(data = subset(aop, city == "rio"), aes(fill = TMISB), color = NA, alpha=.7)+
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


