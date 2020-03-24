#Kapferer mine conflict process

#This does not use the data from the Pjake website

#g_dl <- load_dl_graph(files[3], directed = FALSE)

#going to a graph and back again solves a problem of multiple edges, but allows vertex attributes to be added in easily
mine_edge_list_df <- read_csv(files[2]) %>%
  mutate_all(., ~ifelse(is.na(.), 0, .)) %>%
  select(-X1) %>%
  as.matrix() %>%
  graph_from_adjacency_matrix(., mode = "undirected", weighted = "edge_type", diag = FALSE, add.colnames = "name") %>%
  as_data_frame() %>%
  mutate(temp_flow = 1,
         temp_flow2 = 1,
         edge_name = paste(from, to ,sep="-"),
         edge_type2 = ifelse(edge_type==2, Inf, 1) 
  ) 


kapferer_vertices <- tibble(name = c(mine_edge_list_df$from, mine_edge_list_df$to)) %>%
  distinct() %>%
  mutate(force1 =  case_when(
    name == "Abraham" ~1,
    name == "Donald"~-1,
    TRUE ~0
  ),
  force2 =  case_when(
    name == "Abraham" ~1,
    name == "Abel"~-1,
    TRUE ~0
  ),
  force3 =  case_when(
    name == "Benson" ~1,
    name == "Donald"~-1,
    TRUE ~0
  ),
  force4 =  case_when(
    name == "Benson" ~1,
    name == "Abel"~-1,
    TRUE ~0
  ),
  force = force1
  )




mine_combos <- expand_grid(
  accuser = c("Abraham", "Benson"),
  accused = c("Donald", "Abel"),
  range = c( seq(0, 100 * 50, by = 100))
)

Kapferer_embeddings_results <-1:nrow(mine_combos) %>%
  map_df(~{
    
    kapferer_vertices <- kapferer_vertices %>%
      mutate(force =  case_when(
        name == mine_combos$accuser[.x] ~1,
        name == mine_combos$accused[.x]~-1,
        TRUE ~0
      )
      )
    
    kapferer_mine_g <- mine_edge_list_df %>%
      graph_from_data_frame(., directed = FALSE, vertices = kapferer_vertices) %>%
      set.edge.attribute(. , "distance", value = 1) %>%
      set.edge.attribute(., "Area", value = 1) %>%
      calc_spring_youngs_modulus(., 
                                 "temp_flow", 
                                 "edge_type2", 
                                 minimum_value = 100, 
                                 stretch_range = mine_combos$range[.x]) %>%
      calc_spring_constant(., E ="E", A = "Area", distance = "distance") 
    
    
    kapferer_SETS <-auto_SETSe(kapferer_mine_g, 
                               force = "force", 
                               flow = "temp_flow", 
                               distance = "distance", 
                               capacity = "edge_type",
                               edge_name = "edge_name",
                               k = "k",
                               tstep = 0.02,
                               tol = 10e-4,
                               mass = 1,
                               verbose = FALSE)
    
    
    kapferer_mine_g <- kapferer_mine_g %>% edge_attr(., "edge_type") %>%
      {ifelse(max(.)==., min(.)*(mine_combos$range[.x]/100)+1, .)} %>%
      set_edge_attr(kapferer_mine_g, "edge_weight", value = .)
    
    cluster_results <-kapferer_SETS$node_embeddings %>%
      select(node, force, elevation) %>%
      mutate(
        SETSe_elev = as.integer(ifelse(elevation > 0, 2, 1)),
        SETSe_kmeans = kmeans(elevation, centers = 2, nstart = 30)$cluster) %>%
      left_join(., tibble(
        node = vertex_attr(kapferer_mine_g, "name"),
        clustering_fast_greedy = cluster_fast_greedy(
          kapferer_mine_g,
          weights = edge_attr(kapferer_mine_g, "edge_weight"),
          merges = T
        ) %>% as.hclust %>%
          cutree(., k = 2),
        clustering_walktrap = cluster_walktrap(
          kapferer_mine_g,
          weights = edge_attr(kapferer_mine_g, "edge_weight"),
          merges = T
        ) %>% as.hclust %>%
          cutree(., k = 2)
      )
      ) %>%
      rename_clusters(., names(.)[4:7]) %>%
      mutate(ratio = (mine_combos$range[.x] +100)/100,
             combo = paste(mine_combos$accuser[.x], mine_combos$accused[.x]  , sep = "-"))
    
  })





Kapferer_aggregated <- Kapferer_embeddings_results %>%
  select(-force,-elevation) %>%
  pivot_longer(cols = SETSe_elev:clustering_walktrap, names_to ="cluster_method") %>%
  group_by(ratio, cluster_method, value, combo) %>%
  summarise(counts = n()) %>%
  group_by(ratio, cluster_method, combo) %>%
  mutate(fract = counts/sum(counts)) %>%
  ungroup %>%
  group_by(combo, 
           ratio,
           cluster_method) %>%
  mutate(separation_success = !sum(is.na(value))>0)


kapferer_grid<-expand_grid(clusters = Kapferer_embeddings_results %>% select(contains("clustering"), contains("SETSe")) %>% names(),
                           ratio = unique(Kapferer_embeddings_results$ratio), combo = unique(Kapferer_embeddings_results$combo))

kapferer_separation <-1:nrow(kapferer_grid) %>%
  map_df( ~ {
    
    #if the beligerents are seperated they are in two different clusters
    beligerents_separated <- Kapferer_embeddings_results  %>% 
      filter(ratio == kapferer_grid$ratio[.x],  combo == kapferer_grid$combo[.x], force != 0) %>%
      select(force ,kapferer_grid$clusters[.x]) %>% pull(2) %>% 
      unique(.) %>% length %>% {
        . > 1
      }
    
    tibble(cluster_method = kapferer_grid$clusters[.x], 
           separate_succcess = beligerents_separated,
           ratio = kapferer_grid$ratio[.x],
           combo = kapferer_grid$combo[.x])
    
  })

