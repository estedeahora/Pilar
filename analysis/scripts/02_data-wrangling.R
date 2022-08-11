# 02 Data wrangling

# Loading Packages -----------------------------------------------------
library(tidyverse)
library(readxl)
library(sf)
library(units)

library(tidygraph)
library(sfnetworks)

library(FactoMineR)
library(factoextra)

# load data -------------------------------------------
AUX <- list(db_label = read_xlsx("analysis/data/bases.xlsx"))

ruta <- "analysis/data/"
archivos <- list.files(ruta)
archivos <- archivos[str_detect(archivos, "\\.geojson") &
                       str_detect(archivos, "pilar")]

CARTO <- map(paste0(ruta, archivos), st_read)

names(CARTO) <- archivos |>
  str_remove("pilar_") |>
  str_remove("\\.geojson") |>
  str_to_upper()

rm(ruta, archivos)

# Generar base escuela por nivel y sector ---------------------------------
# No considerar:
#   - Especial (ningún nivel);
#   - Servicios Complementarios
#   - Adultos (Primaria y  Secundaria)
#   - Profesional = c("Adultos-Formación Profesional", "Centro Formación Profesional(INET)")

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

for(i in 1:length(l)){
  patron <- paste0(l[[i]], collapse = "|")
  v <- ESC |>
    filter(str_detect(nen, patron))

  if(names(l)[i] == "SNU"){
    v <- list(SNU = v)
  }else{
    v <- v |>
      split(~SECTOR) |>
      set_names(\(x) paste(names(l)[i], x, sep = "_" ) |> str_to_upper())
  }

  CARTO <- c(CARTO, v)
}

rm(l, i, v, patron, ESC)

# Generar base por culto --------------------------------------------------

CULTO <- CARTO$EDIFICIO_RELIGIOSO |>
  mutate(CATOLICO = ifelse(gna == "Parroquia", "CULTO_CATOLICO", "CULTO_OTRO") ) |>
  split(~CATOLICO)
CARTO <- c(CARTO, CULTO)

CARTO$EDIFICIO_RELIGIOSO <- NULL
rm(CULTO)
