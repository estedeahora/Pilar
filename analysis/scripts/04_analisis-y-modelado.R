#                      Análisis y modelado

# Preparar datos ----------------------------------------------------------

  data <- esquina |>
    select(AUX$db_label$db[AUX$db_label$ANALISIS == 1])

# 01 Reducción de dimensionalidad ------------------------------------------
  res_PCA <- data |>
    st_drop_geometry() |>
    PCA(graph = F)

# 02 Armado de grupos -------------------------------------------------------
  AUX$n_cl <- 6

  res_clu <- HCPC(res_PCA, nb.clust = AUX$n_cl, graph = F )
  data$gr <- factor(res_clu$data.clust$clust)

