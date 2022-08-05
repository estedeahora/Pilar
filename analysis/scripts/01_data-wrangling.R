
# Loading Packages -----------------------------------------------------
library(tidyverse)
library(readxl)
library(sf)


library(osmdata)

# load data -------------------------------------------
CARTO <- list()

# Pilar
CARTO$PILAR <- st_read("Analysis/data/pilar_depto.geojson")

# Callejero
CARTO$CALLE <- st_read("Analysis/data/pilar_calle.geojson") |> 
  mutate(PPAL = ifelse(tipo %in% c("AUT", "AV", "RUTA"), T, F))

# Educación
CARTO$ESC <- st_read("Analysis/data/pilar_esc.geojson")
CARTO$UNI <- st_read("Analysis/data/pilar_uni.geojson")

ggplot() +
  geom_sf(data = CARTO$PILAR, color = "grey90") +
  geom_sf(data = CARTO$CALLE, alpha = 0.3, color = "grey60") +
  geom_sf(data = CARTO$CALLE |> filter(PPAL), color = "red") +
  geom_sf(data = CARTO$ESC, aes(color = SECTOR), alpha = 0.5) +
  geom_sf(data = CARTO$UNI, shape = 4 ) +
  scale_color_discrete(type = c("darkblue", "tomato4")) +
  theme_void()

# Otros datos -------------------------------------------------------------------

# CARTO$RENABAP <- read_sf(paste0(path_CARTO,
#                                 "ARG RENABAP/ARG barrios-populares.shp")) %>%
#   st_transform(st_crs(CARTO$AGLOMERADO))
# 
# CARTO$COUNTRY <- read_sf(paste0(path_CARTO,
#                                 "ARG Barrios Cerrados/ARG Barrio cerrado.shp"))%>%
#   st_transform(st_crs(CARTO$AGLOMERADO))
# IGN: Salud 
# INFRAEST$salud <- read_sf(paste0(AUX$path_CARTO,
#                                  "ARG Salud/salud_020801.shp")) %>%
#   select(name = fna, gna)
