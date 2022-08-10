# 02 Data wrangling

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

# Generar base escuela por nivel y sector ---------------------------------

# unique(CARTO$ESC$nen) |>
#   str_split(pattern = "; |;", simplify = T) |>
#   as.character() |>
#   # unique() |>
#   table()
ESC <- CARTO$ESC
CARTO$ESC <- NULL

l <- list(
       Inicial = c("Común-Escuela Infantil", "Común-Jardín de Infantes", "Común-Jardín Maternal"),
       Primario = c("Común-Primaria 6años"),
       Secundario = c("Común-Secundaria-Ambos Ciclos", "Común-Secundaria-Ciclo Básico", "Secundaria TécProf(INET)"),
       SNU = c("SNU-Ambos Tipos de Formación", "SNU-Formación Docente", "SNU-Formación Técnico Profesional", "Inst.Sup TécProf(INET)")
     )
# No considerar:
#   - Especial (ningún nivel);
#   - Servicios Complementarios
#   - Adultos (Primaria y  Secundaria)
#   - Profesional = c("Adultos-Formación Profesional", "Centro Formación Profesional(INET)")

for(i in 1:length(l)){
  patron <- paste0(l[[i]], collapse = "|")
  v <- ESC |>
    filter(str_detect(nen, patron)) |>
    split(~SECTOR) |>
    set_names(\(x) paste(names(l)[i], x, sep = "_" ) |> str_to_upper())
  CARTO <- c(CARTO, v)
}

rm(l, i, v, patron, ESC)
