library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(ggspatial)
library(dplyr)
library(here)

source(here::here("R/plot_map.R"))

# without flooding

## congo
file_path <- here("data/ga2_congo.nc")
bounding_box <- c(15,22,-3,3) # c(15,20,-2,3)  # max bb:  c(15,22,-3,3)
map_congo <- plot_map(file_path, bounding_box, show_legend = TRUE)

ggsave(here("book/images/map_congo.png"), plot = map_congo, width = 8, height = 8)

## switzerland
file_path <- here("data/ga2_switzerland.nc")
bounding_box <- c(xmin = 6, xmax = 10.5, ymin = 45.81799, ymax = 47.80838)
map_switzerland <- plot_map(file_path, bounding_box, show_legend = TRUE, show_countries = FALSE, show_inset = FALSE)

ggsave(here("book/images/map_switzerland.png"), plot = map_switzerland, width = 8, height = 5)

