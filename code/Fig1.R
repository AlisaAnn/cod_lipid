library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(tidyverse)
library(cowplot)
library(ggpubr)
library(rgdal)
library(brms)
library(ggstar)

theme_set(theme_bw())

# set palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## study site -----------------------------------------------

ak <- ne_countries(scale = "large", returnclass = "sf", continent="north america")

# use this version unless the high-res version is entered!
# ak <- ne_countries(scale = "medium", returnclass = "sf", continent="north america")
world <- ne_countries(scale='medium', returnclass = "sf")


# add Cook Bay location
cook <- data.frame(lat = 57.803, long = -152.374)

box <- data.frame(long = c(-156, -156, -151.3, -151.3, -156), lat = c(55.3, 59, 59, 55.3, 55.3))

inset <- ggplot(data = world) +
  geom_sf(fill="dark grey", color=NA) +
  coord_sf(xlim = c(-179, -70), ylim = c(0, 70)) +
  geom_path(data=box, aes(long, lat), color=cb[8], size=0.5) +
  theme_classic() +
  theme(axis.line = element_line(color="black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color="black", fill="transparent"),
        panel.spacing = unit(1, 'mm'))

inset  


map.plot <- ggplot(ak) +  
  # geom_path(data=polys, aes(long, lat, color=type), lwd=1.5) +
  geom_sf(fill="darkgoldenrod3", color=NA) + 
  coord_sf(xlim = c(-156, -151.3), ylim = c(56.3, 59), expand = FALSE) +
  geom_star(data = cook, aes(long, lat), fill=cb[6], size=3.5) +
  theme(axis.title = element_blank())


map.plot

full.map <- map.plot +
  annotation_custom(
    grob = ggplotGrob(inset),
    xmin = -156,
    xmax = -153.5,
    ymin = 57.95,
    ymax = 58.95
  ) 

full.map

ggsave("./figs/study_site.png", width = 4, height = 3, units = 'in')

