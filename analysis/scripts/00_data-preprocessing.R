library(tidyverse)
library(sf)
library(rwfs)
library(units)
library(osmdata)

# Functions ---------------------------------------------------------------

# Cargar y Cortar objetos sf
st_readandcut <- function(path, pol = PILAR_bf){
  st_read(path) |>
    st_make_valid() |>
    st_intersection(pol)
}

# Recodificar a UTF-8
iconv2 <- function(v){
  if(is.character(v)){
    res <- iconv(v , from="UTF-8", to="UTF-8")
  }else{
    res <- v
  }
  return(res)
}
# Bounding box --------------------------------------

bb <- c(xmin = -59.08579,
        ymin = -34.59813,
        xmax = -58.70734,
        ymax = -34.28126)

class(bb) <- "bbox"
bb <- st_as_sfc(bb) |> st_set_crs(value = 4326)

# INDEC: Pilar y Región --------------------------------------------

# wfs Conection
wfs <- "https://geoservicios.indec.gov.ar/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities"

fileName <- tempfile()

download.file(wfs, fileName)
request <- GMLFile$new(fileName)
client <- WFSCachingClient$new(request)

# Región
REGION <- client$getLayer(layer = "geocenso2010:departamentos_codigo") |>
  filter(codpcia == "06") |>
  st_intersection(bb)

# Pilar
PILAR <- REGION |>
  filter(link == "06638")

PILAR_bf <- PILAR |>
  st_buffer(dist = set_units(1.5, km) ) |>
  select()

rm(client, request,wfs, fileName)

# OSM ---------------------------------------------------------------------

# CALLEJERO
# value = c("motorway", "road", "trunk", "primary",
#           "secondary", "tertiary", "unclassified")

CALLE <-  bb |>
  opq() |>
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>%
  `[[`("osm_lines") %>%
  mutate(across(.fns = ~iconv2(.x)) ) |>
  filter(highway != "footway" ) |>
  st_intersection(PILAR_bf)

# Bancos y ATM
BANK <-  bb  |>
  opq() |>
  add_osm_feature(key = 'amenity',
                  value = c("bank")) %>%
  osmdata_sf() %>%
  `[[`("osm_points") %>%
  mutate(across(.fns = ~iconv2(.x)) ) |>
  st_intersection(PILAR_bf)

ATM <- bb |>
  opq() |>
  add_osm_feature(key = 'amenity',
                  value = c("atm")) %>%
  osmdata_sf() %>%
  `[[`("osm_points") %>%
  mutate(across(.fns = ~iconv2(.x)) ) |>
  st_intersection(PILAR_bf)

# Seguridad: Comisaría
POLICE <- bb %>%
  opq() |>
  add_osm_feature(key = 'amenity',
                  value = "police") %>%
  osmdata_sf() %>%
  `[[`("osm_points") %>%
  mutate(across(.fns = ~iconv2(.x)) ) |>
  st_intersection(PILAR_bf)

# Compra de alimentos
SUPER <-  bb %>%
  opq() |>
  add_osm_feature(key = 'shop',
                  value = c("butcher", "convenience", "wholesale",
                            "greengrocer", "mall", "supermarket")) %>%
  osmdata_sf() %>%
  `[[`("osm_points") %>%
  mutate(across(.fns = ~iconv2(.x)) )|>
  st_intersection(PILAR_bf)

# ME: Mapa de Educativo Nacional --------------------------------------
# http://mapa.educacion.gob.ar/
# http://mapa.educacion.gob.ar/geoservicios
#
# https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/
# https://github.com/rOpenGov/rwfs

# ESC <- st_read("Analysis/data/pilar_esc.geojson")
# UNI <- st_read("Analysis/data/pilar_uni.geojson")

fileName <- tempfile()
download.file("http://mapa.educacion.gob.ar/geoserver/ows?service=wfs&version=1.1.0&request=GetCapabilities", fileName)
request <- GMLFile$new(fileName)
client <- WFSCachingClient$new(request)

ESC <- client$getLayer("establecimiento_educativo") |>
  select(CUE = cue, name = fna, SECTOR = ges, mde, nen, geometry = the_geom) |>
  st_intersection(PILAR_bf)

UNI <- client$getLayer("universidades") |>
  select(universidad, facultad, SECTOR = sector, geometry = the_geom)|>
  st_intersection(PILAR_bf)

unlink(fileName)

rm(request, client, fileName)

# IGN ---------------------------------------------------------------------

ruta <- "Analysis/_raw/IGN/"
archivos <- list.files(path = ruta, pattern = ".json",
                       all.files = T, recursive = T)

IGN <- map(paste0(ruta, archivos), st_readandcut)

names(IGN) <- archivos |>
  (\(x){str_split(x, pattern = "\\+", simplify = T)[,2]})() |>
  str_remove(".json")

# Antenas de celulares (DATAR) ----------------------------------------------------

ruta <- "Analysis/_raw/CEL/"
archivos <- list.files(path = ruta, pattern = ".geojson",
                       all.files = T, recursive = T)

CEL <- map(paste0(ruta, archivos), st_readandcut)
names(CEL) <- archivos |>
  str_remove(".geojson")

CEL <- do.call(rbind, CEL) |>
  as_tibble(rownames = "base") |>
  mutate(base = str_split(base, "\\.", simplify = T)[ , 1 ]) |>
  st_as_sf(crs = 4326) |>
  select(base)

rm(ruta, archivos)

# Guardar CARTO -----------------------------------------------------------

# General
st_write(REGION, dsn = "Analysis/data/pilar_region.geojson", append = F)
st_write(PILAR, dsn = "Analysis/data/pilar_depto.geojson", append = F)
st_write(CALLE, dsn = "Analysis/data/pilar_calle.geojson", append = F)

# Educación
st_write(ESC, dsn = "Analysis/data/pilar_esc.geojson", append = F)
st_write(UNI, dsn = "Analysis/data/pilar_uni.geojson", append = F)

# IGN
n <- paste0("Analysis/data/pilar_", names(IGN), ".geojson")
walk2(IGN, n, \(x, y) st_write(obj = x, dsn = y) )

# Celular
st_write(CEL, dsn = "Analysis/data/pilar_cel.geojson", append = F)

# OSM
st_write(BANK, dsn = "Analysis/data/pilar_bank.geojson", append = F)
st_write(ATM, dsn = "Analysis/data/pilar_atm.geojson", append = F)
st_write(POLICE, dsn = "Analysis/data/pilar_police.geojson", append = F)
st_write(SUPER, dsn = "Analysis/data/pilar_comercio.geojson", append = F)


# Pruebas gráficas --------------------------------------------------------

REGION |>
  ggplot() +
  geom_sf() +
  geom_sf(data = PILAR_bf, color = "red", alpha = 0) +
  geom_sf(data = PILAR, fill = "green", alpha = 0.1) +
  # geom_sf_text(aes(label = departamento)) +
  geom_sf(data = CALLE, color = "grey50", alpha = 0.5) +
  # geom_sf(data = ESC, aes(color = SECTOR), alpha = 0.5) +
  # geom_sf(data = UNI, shape = 4 ) +
  # geom_sf(data = CEL, aes(color = base) ) +
  # geom_sf(data = ATM, color = "red", shape = 4 ) +
  scale_color_discrete(type = c("darkblue", "tomato4")) +
  theme_void()

library(leaflet)

leaflet() |>
  addTiles() |>
  addPolylines(data = CALLE)



