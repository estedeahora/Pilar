# 01 Descarga y pre procesamiento de bases de datos

library(tidyverse)
library(sf)
library(rwfs)
library(units)
library(osmdata)

# Functions ---------------------------------------------------------------

# iconv2: Recodificar a UTF-8
iconv2 <- function(v){
  if(is.character(v)){
    res <- iconv(v , from="UTF-8", to="UTF-8")
  }else{
    res <- v
  }
  return(res)
}

# OSM_query: Generar consulta de datos en OSM

OSM_query <- function(bb, k, v = NULL,
                      feature = "osm_points", zona = PILAR_bf){
  res <-  bb |>
    opq() |>
    add_osm_feature(key = k, value = v) %>%
    osmdata_sf() %>%
    `[[`(feature) %>%
    mutate(across(.fns = ~iconv2(.x)) ) |>
    st_intersection(zona)

  return(res)
}

# st_readandcut: Cargar y Cortar objetos sf
st_readandcut <- function(path, pol = PILAR_bf){
  st_read(path) |>
    st_make_valid() |>
    st_intersection(pol)
}

# Bounding box --------------------------------------

bb <- c(xmin = -59.08579,
        ymin = -34.59813,
        xmax = -58.70734,
        ymax = -34.28126)

class(bb) <- "bbox"
bb <- st_as_sfc(bb) |> st_set_crs(value = 4326)

# WFS --------------------------------------------------------------

  # ╠ INDEC: Pilar y Región --------------------------------------------

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

  # ╠ ME: Mapa de Educativo Nacional --------------------------------------
  # http://mapa.educacion.gob.ar/
  # http://mapa.educacion.gob.ar/geoservicios

  # options(timeout = 300)

  fileName <- tempfile()
  download.file("http://mapa.educacion.gob.ar/geoserver/ows?service=wfs&version=1.1.0&request=GetCapabilities", fileName)
  request <- GMLFile$new(fileName)
  client <- WFSCachingClient$new(request)

  ESC <- client$getLayer("establecimiento_educativo") |>
    select(CUE = cue, name = fna, SECTOR = ges, mde, nen, geometry = the_geom) |>
    st_intersection(PILAR_bf)

  UNI <- client$getLayer("universidades") |>
    select(universidad, facultad, SECTOR = sector, geometry = the_geom) |>
    st_intersection(PILAR_bf)

  unlink(fileName)

  rm(request, client, fileName)

  # ╚ urBAsig -------------------------------------------------------------

  # TODO: DESARROLLAR
  # wfs <- "http://urbasig.gob.gba.gob.ar/geoserver/urbasig/wfs"
  #
  # fileName <- tempfile()
  #
  # download.file(wfs, fileName)
  # request <- GMLFile$new(fileName)
  # client <- WFSCachingClient$new(request)
  #
  # # Zonificación
  # ZONIF <- client$getLayer(layer = "urbasig:zonificacion") |>
  #   st_intersection(PILAR_bf)

  # BC <- st_read("analysis/_draft/urBAsig/urbasig_rpuc_1.geojson") |>
  #   st_intersection(PILAR_bf)

# OSM ---------------------------------------------------------------------

  # CALLEJERO

  CALLE <- OSM_query(bb = bb, k = 'highway',
                     feature = "osm_lines"#, zona = REGION
                     )  |>
    filter(highway != "footway")

  CALLE <- do.call(rbind,
                   lapply(X = 1:nrow(CALLE),
                          FUN = function(i){st_cast(CALLE[i, ],
                                                    "LINESTRING")}))

  # Bancos y ATM
  BANK <- OSM_query(bb = bb, k = 'amenity', v = c('bank', 'atm' ) )

  # Seguridad: Comisaría
  POLICE <- OSM_query(bb = bb, k = 'amenity', v = 'police')

  # Compra de alimentos
  SUPER <- OSM_query(bb = bb, k = 'shop', v = "supermarket")

# Descarga directa --------------------------------------------------------

  # ╠ Paradas de colectivos ---------------------------------------------------
    # Datos de paradas (puntos) GCBA
    #   "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/colectivos/paradas-de-colectivo.geojson"
    # Datos de recorridos (lineas) de transporte nacional, provincial y municipal
    #   Nacional
    #   "https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/84947471-9c1e-4a23-8a2e-03a8c87c056f/download/lineasbusrmbajurisdiccionnacional.geojson"
    #   Provincial
    #   "https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/f95e25bc-a6b2-4a78-a04b-35fa437be96b/download/lineasbusrmbajurisdiccionprovincial.geojson"
    #   Municipal
    #   "https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/f0f3791a-addc-4143-bb95-ef0e8bca5bd8/download/lineasbusrmbajurisdiccionmunicipal.geojson"

  COLEC <- c("https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/84947471-9c1e-4a23-8a2e-03a8c87c056f/download/lineasbusrmbajurisdiccionnacional.geojson",
             "https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/f95e25bc-a6b2-4a78-a04b-35fa437be96b/download/lineasbusrmbajurisdiccionprovincial.geojson",
             "https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/f0f3791a-addc-4143-bb95-ef0e8bca5bd8/download/lineasbusrmbajurisdiccionmunicipal.geojson") |>
    map(st_read) |>
    map(st_intersection, PILAR_bf) |>
    set_names(paste0("colec_", c("nacional", "provincial", "municipal")))

  # ╠ Ferrocarriles -----------------------------------------------------------

  TREN <- read.csv2("https://datos.transporte.gob.ar/dataset/8a83da77-e431-4d6f-a14a-c98ebc0b900b/resource/624f34de-faa7-4657-8f0a-0aed482f2341/download/estaciones_estaciones.csv") |>
      filter(longitud != "" ) |>
      st_as_sf(coords = c("longitud", "latitud"), crs = 4326) |>
      st_intersection(PILAR_bf)

  TREN_LINEA <- st_read("https://datos.transporte.gob.ar/dataset/f87b93d4-ade2-44fc-a409-d3736ba9f3ba/resource/367a26af-c5b4-4361-b614-abd6ad743383/download/ambalineas.geojson")|>
    st_intersection(REGION)

  # ╠ RENABAP -----------------------------------------------------------------

  BP <- st_read("https://datosabiertos.desarrollosocial.gob.ar/dataset/0d022767-9390-486a-bff4-ba53b85d730e/resource/97cc7d10-ad4c-46cb-9ee4-becb402adf9f/download/2022-07-13_info_publica.geojson") |>
    st_make_valid() |>
    st_intersection(PILAR_bf)

# Archivo local -----------------------------------------------------------

  # ╠ IGN ---------------------------------------------------------------------

  ruta <- "Analysis/_rawdata/IGN/"
  archivos <- list.files(path = ruta, pattern = ".json",
                         all.files = T, recursive = T)

  IGN <- map(paste0(ruta, archivos), st_readandcut)

  names(IGN) <- archivos |>
    (\(x){str_split(x, pattern = "\\+", simplify = T)[,2]})() |>
    str_remove(".json")

  rm(ruta, archivos)

  IGN$fabrica <- IGN$fabrica_polyg |>
    st_centroid() |>
    bind_rows(IGN$fabrica)

  IGN$fabrica_polyg <- NULL

  # ╚ Antenas de celulares ----------------------------------------------------
  # dat.ar
  ruta <- "Analysis/_rawdata/CEL/"
  archivos <- list.files(path = ruta, pattern = ".geojson",
                         all.files = T, recursive = T)

  CEL <- map(paste0(ruta, archivos),
             st_readandcut) |>
    bind_rows()
  # names(CEL) <- archivos |>
  #   str_remove(".geojson")
  #
  # CEL <- do.call(rbind, CEL) |>
  #   as_tibble(rownames = "base") |>
  #   mutate(base = str_split(base, "\\.", simplify = T)[ , 1 ]) |>
  #   st_as_sf(crs = 4326) |>
  #   select(base)

  rm(ruta, archivos)

# Guardar CARTO -----------------------------------------------------------

# General
st_write(REGION, dsn = "Analysis/data/pilar_region.geojson", append = F)
st_write(PILAR, dsn = "Analysis/data/pilar_pilar.geojson", append = F)
st_write(CALLE, dsn = "Analysis/data/pilar_calle.geojson", append = F)

# Tipos de barrios
st_write(BP, dsn = "Analysis/data/pilar_renabap.geojson", append = F)

# Transporte
n <- paste0("Analysis/data/pilar_", names(COLEC), ".geojson")
walk2(COLEC, n, \(x, y) st_write(obj = x, dsn = y) )
rm(n)

st_write(TREN, dsn = "Analysis/data/pilar_tren_est.geojson", append = F)
st_write(TREN_LINEA, dsn = "Analysis/data/pilar_tren_lin.geojson", append = F)

# Educación
st_write(ESC, dsn = "Analysis/data/pilar_esc.geojson", append = F)
st_write(UNI, dsn = "Analysis/data/pilar_uni.geojson", append = F)

# IGN
n <- paste0("Analysis/data/pilar_", names(IGN), ".geojson")
walk2(IGN, n, \(x, y) st_write(obj = x, dsn = y) )
rm(n)

# Celular
st_write(CEL, dsn = "Analysis/data/pilar_cel.geojson", append = F)

# OSM
st_write(BANK, dsn = "Analysis/data/pilar_bank.geojson", append = F)
st_write(POLICE, dsn = "Analysis/data/pilar_police.geojson", append = F)
st_write(SUPER, dsn = "Analysis/data/pilar_super.geojson", append = F)
