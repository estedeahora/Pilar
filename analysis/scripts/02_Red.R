library(sfnetworks)

# Red de calles -----------------------------------------------------------------
CALLE <- st_read("Analysis/data/pilar_calle.geojson") |> 
  st_cast("LINESTRING")
  
if(F){
  # Armar red bidireccional
  CALLE <- CALLE |>
    mutate(cod_sent = ifelse(cod_sent %in% c(0, 2, 12), 0, cod_sent) )
  
  CALLE_dir <- CALLE |>
    filter(cod_sent %in% 0:1)
  
  CALLE_inv <- CALLE |>
    filter(cod_sent %in% -1:0) |>
    st_reverse()
  
  CALLE <- rbind(CALLE_dir, CALLE_inv)
  rm(CALLE_dir, CALLE_inv)
}

# Armar red de calles
net <-  CALLE |>
  as_sfnetwork(directed = F,
               length_as_weight = T)

# Limpieza de la red
net <- net |>
  # Subdivisión: Divide los ejes en función de los nodos interiores
  convert(to_spatial_subdivision) |>
  # Calcular peso
  activate("edges") |>
  mutate(weight = edge_length()) |>
  # Quita los loops y nodos que duplican path (mejorar rendimiento)
  arrange(weight) |>
  convert(to_spatial_simple) |>
  # Filtrar nodos aislados
  activate("nodes") |>
  filter(!node_is_isolated())

MAP$CALLE <- MAP$CALLE |>
  filter(red_jerarq %in% c("VÍA DISTRIBUIDORA PRINCIPAL", "VÍA TRONCAL"))
