# Limpieza y formateo de la red para el análisis

# net: Armado de red de calles -----------------------------------------

net <- CARTO$CALLE |>
  as_sfnetwork(directed = F,
               length_as_weight = T)

# Limpieza de la red
  net <- net |>
    activate("edges") |>
    # Subdivisión: Divide los ejes en función de los nodos interiores
    convert(to_spatial_subdivision) |>
    # Quita nodos intermedios sin utilidad
    convert(to_spatial_smooth) |>
    # Calcular peso de los ejes en función de su tamaño
    activate("edges") |>
    mutate(weight = edge_length()) |>
    # Quita los loops y nodos que duplican path (mejorar rendimiento)
    arrange(weight) |>
    convert(to_spatial_simple) |>
    # Filtrar nodos aislados
    activate("nodes") |>
    filter(!node_is_isolated())

# Retener primer componente de la red
  net <- net |>
    filter(group_components() == 1)

  # save(net, file = "analysis/data/net.RData")
  # load(file = "analysis/data/net.RData")

# Distancia de red a objetos puntuales ------------------------------------

  # Armar paradas de colectivo --------------------------------------------

  n <- net |>
    activate("nodes") |>
    st_as_sf()

  sel <- names(CARTO)
  sel <- sel [str_detect(sel, "COLEC")]

  COLEC <- list()

  for(i in sel){
    a <- CARTO[[i]] |>
      summarise() |>
      st_buffer(dist = set_units(0.02, km) )

    COLEC[[i]] <- st_intersection(n, a)
  }


  rm(sel, a, i, n)

  # Armar ppp -------------------------------------------------------------

    # Armar df con todos los puntos
    ppp <- CARTO[map_lgl(CARTO,
                       \(gdf){
                         clase <- gdf |>
                           st_geometry() |>
                           class()
                         if(any(str_detect(clase, "POINT"))  ) TRUE else FALSE
                       })] |>
      bind_rows(.id = "db") |>
      select(db)

    ppp <- ppp |>
      filter(!st_is_empty(ppp)) |>
      # left_join(AUX$db_label, by = "db") |>
      mutate(id = 1:n(),
             # CLASE = factor(CLASE,
             #                levels = unique(AUX$db_label$CLASE) )
             )

  # Asignar nodo a ppp ------------------------------------------------

    # if(AUX$d){
      # blend red con ppp
      cat("Blend ppp")
      ini <- Sys.time()
      net_aux <- st_network_blend(net, ppp)
      fin <- Sys.time()
      cat(": done. Tiempo de procesamiento: ", fin - ini, "\n")

      # Id del nodo más cercano al punto
      ppp$near_node <- st_nearest_feature(ppp, net_aux)

      # Distancia a nodo más cercano desde posición real del punto
      ppp$near_dist <- st_distance(ppp,
                                   st_geometry(net_aux)[ppp$near_node],
                                   by_element = T) |>
        round()

      # Quitar nodos fuera de CABA (más de 500m)
      # ppp <- ppp |> filter(near_dist < units::as_units(500, "m") )

      # save(net_aux, ppp, file = here::here("analysis/data/net.RData") )

    # }else{
      # load(here::here("analysis/data/net.RData") )
    # }

  # Calcular matriz de distancias ------------------------------------------------

    net <- net |> activate("nodes") |> mutate(id_node = 1:n())
    # net |> activate("nodes") |> st_as_sf() |> st_drop_geometry() |> select(id_node) |> simplify() |> unname()
    from <- 1:nrow(st_as_sf(activate(net, "nodes")))
    to <- ppp$near_node |> unique()

    cat("Matriz de costos")
    ini <- Sys.time()
    dis <- st_network_cost(x = net_aux, from = from, to = to)
    fin <- Sys.time()
    cat(": done. Tiempo de procesamiento: ", fin - ini, "\n")
    rm(ini, fin)

    colnames(dis) <- to
    rownames(dis) <- from

  # Distancia mínima a ppp ---------------------------------------------------

    res <- data.frame(from = from)

    cat("Calculando distancia mínima\n")
    for(i in unique(ppp$db)){
      cat(i)
      ppp_aux <- ppp |>
        filter(db == i) |>
        mutate(node = as.character(near_node),
               dist = as.numeric(near_dist) )
      dis_aux <- dis[ , ppp_aux$node]
      dis_aux <- t(apply(dis_aux, 1 , function(x) x + ppp_aux$dist))
      res[i] <- apply(dis_aux, 1, min)
      cat(": done\n")
    }

    rm(from, to, i, dis_aux, ppp_aux)

# Armado de bases espaciales ---------------------------------------------

  esquina <- net |>
    st_as_sf() |>
    # select(from = ".tidygraph_node_index") |>
    left_join(res, by = "from")

  rm(res, net_aux, dis)

# Guardar bases ----------------------------------------------------------
  save(esquina, file = here::here("analysis/data/esquina.RData") )
