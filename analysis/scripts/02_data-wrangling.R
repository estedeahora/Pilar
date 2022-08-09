# 01 Data wrangling

# Loading Packages -----------------------------------------------------
library(tidyverse)
library(readxl)
library(sf)

library(tidygraph)
library(sfnetworks)

# load data -------------------------------------------

ruta <- "analysis/data/"
archivos <- list.files(ruta)
archivos <- archivos[str_detect(archivos, "\\.geojson")]

CARTO <- map(paste0(ruta, archivos), st_read)

names(CARTO) <- archivos |>
  str_remove("pilar_") |>
  str_remove("\\.geojson") |>
  str_to_upper()

rm(ruta, archivos)

# Transformar polígonos y lineas a puntos  -----------------------------------
names(CARTO)

CARTO$CALLE$highway |>
  table()

CARTO$CALLE |> #st_drop_geometry() |> count(highway, sort = T)
  ggplot() +
  geom_sf(aes(color = highway))

CARTO$CALLE |> st_drop_geometry() |> count(highway, sort = T)

CARTO$CALLE |>
  # filter(highway == c( "residential", "service", "tertiary",
  #                      "motorway", "motorway_link") ) |>
  ggplot() +
  geom_sf(aes(color = highway), alpha = 0.5) +
  # geom_sf(data = CARTO$CALLE |> filter(highway == "living_street"),
  #         color = "red", size = 1) +
  geom_sf(data =CARTO$CALLE |> filter(str_detect( name, "Ramal Pilar")), color = "red", size = 2) +
  # geom_sf(data =CARTO$CALLE |> filter(highway  ==  "motorway"), color = "green") +
  geom_sf(data =CARTO$CALLE |> filter(str_detect( name, "Ruta")), color = "blue") +
  theme_void()
