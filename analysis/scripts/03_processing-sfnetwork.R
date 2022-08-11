# Limpieza y formateo de la red para el análisis

# net: Armado de red de calles -----------------------------------------

  # ╠ Definición de la red ---------------------------------------------
  net <- CARTO$CALLE |>
    as_sfnetwork(directed = F,
                 length_as_weight = T)

  # ╠ Limpieza de la red ---------------------------------------------
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

  # ╠ Retener primer componente de la red ------------------------------

  net <- net |>
    filter(group_components() == 1)

  # save(net, file = "analysis/data/net.RData")
  # load(file = "analysis/data/net.RData")

# Distancia de red a objetos puntuales (ppp) ------------------------------

  # ╠ Armar base de recursos (REC) ------------------------------------------

  REC <- CARTO[map_lgl(CARTO,
                       \(gdf){
                         clase <- gdf |>
                           st_geometry() |>
                           class()
                         if(any(str_detect(clase, "POINT"))  ) TRUE else FALSE
                       })] |>
      bind_rows(.id = "db") |>
      select(db)

  # ╠ Creación de net_aux: Blend net con REC -------------------------------

  {cat("DOING. Blend REC (creating net_aux).")
  ini <- Sys.time()
  net_aux <- st_network_blend(net, REC)
  fin <- Sys.time()
  cat(" DONE.\n")
  cat("Tiempo de procesamiento: ", format(fin - ini), "\n")
  rm(ini, fin)}

  id_pilar <- net_aux |>
    st_as_sf() |>
    st_intersects(CARTO$PILAR) |>
    as.numeric()

  net_aux <- net_aux |>
    activate("edges") |>
    mutate(weight = edge_length())|>
    activate("nodes") |>
    select(id_net = ".tidygraph_node_index") |>
    mutate(id_n = 1:n(),
           # Definir nodos de red original
           orig = !is.na(id_net),
           # Definir nodos sobre Pilar de red original
           PILAR = ifelse(!is.na(id_pilar) & orig, T, F))

  rm(id_pilar)

  # ╠ Asignar nodo de net_aux a REC ------------------------------------------------

  # Id del nodo más cercano al punto
  REC$near_node <- st_nearest_feature(REC, net_aux)

  # Distancia a nodo más cercano desde posición real del punto
  REC$near_dist <- st_distance(REC,
                               st_geometry(net_aux)[REC$near_node],
                               by_element = T) |>
    round()

  # ╠ Identificar nodos de paradas de colectivo (COLEC) ----------------------

  n <- net_aux |>
    activate("nodes") |>
    st_as_sf()

  sel <- names(CARTO) |> str_detect("COLEC")

  COLEC <- CARTO[sel]

  for(i in seq_along(COLEC)){
    a <-  COLEC[[i]] |>
      summarise( ) |>
      st_buffer(dist = set_units(0.02, km) )

    COLEC[[i]] <- st_intersection(n, a) |>
      filter(orig) |>
      mutate(x = st_coordinates(geometry)[,1],
             y = st_coordinates(geometry)[,2]) |>
      st_drop_geometry()
  }

  COLEC <- COLEC |>
    bind_rows(.id = "db") |>
    st_as_sf(coords = c("x", "y"), crs = 4326) |>
    select(db, near_node = id_n) |>
    mutate(near_dist = set_units(0, m))

  rm(sel, a, i, n)

  # ╠ Armar base ppp común (COLEC + RECS) ---------------------------------------------------------------------

  ppp <- bind_rows(REC, COLEC,
                   .id = "line"
                   )  |>
    left_join(AUX$db_label, by = "db") |>
    mutate(id = 1:n(),
           CLASE = factor(CLASE,
                          levels = unique(AUX$db_label$CLASE) ) )

  rm(REC, COLEC)

  # ╠ Calcular matriz de distancias ------------------------------------------------

    from <- seq_along(net_aux)[net_aux |> st_as_sf() |> st_drop_geometry() |> select(PILAR) |> simplify()]
    to   <- ppp$near_node |> unique()

    {cat("DOING. Matriz de costos")
    ini <- Sys.time()
    dis <- st_network_cost(x = net_aux, from = from, to = to)
    fin <- Sys.time()
    cat(" DONE.\n")
    cat("Tiempo de procesamiento: ", format(fin - ini), "\n")
    rm(ini, fin)}

    colnames(dis) <- to
    rownames(dis) <- from

  # ╠ Distancia mínima a ppp (DIS) ---------------------------------------------

    res <- data.frame(from = from)

    cat("DOING. Calculando distancia mínima\n")
    for(i in unique(ppp$db)){
      cat(i)
      ppp_aux <- ppp |>
        filter(db == i) |>
        mutate(node = as.character(near_node),
               dist = as.numeric(near_dist) )
      dis_aux <- dis[ , ppp_aux$node]
      dis_aux <- t(apply(dis_aux, 1 , function(x) x + ppp_aux$dist))
      res[i] <- apply(dis_aux, 1, min)
      cat(": DONE\n")
    }

    rm(from, to, i, dis_aux, ppp_aux)

# Armado de bases esquinas ---------------------------------------------

  esquinas <- net_aux |>
      st_as_sf() |>
      filter(PILAR) |>
      select(id_n) |>
      full_join(res, by = c("id_n" = "from")) |>
      mutate(across(.cols = -c("id_n", "geometry"),
                    .fns = ~round(.x, 0)) )

  rm(res, net, dis, net_aux)


# Guardar bases ----------------------------------------------------------

  st_write(esquinas, dsn = here::here("analysis/data/esquinas.geojson") )
  st_write(ppp, dsn = here::here("analysis/data/ppp.geojson") )

  esquinas |>
    st_drop_geometry() |>
    write.csv(file = "analysis/data/esquinas.csv")
