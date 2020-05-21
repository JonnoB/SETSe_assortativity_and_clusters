#plots the position of individual nodes in SETSe space.

#compares clustering techniques
if(file.exists(file.path(PLwd, "peel_node_position.rds"))){
  
  detected_communities <- readRDS(file.path(PLwd, "peel_node_position.rds"))
  
} else {
  
  #make a data frame of the graphs for which I want to compare against other community detection algorithms
  target_graphs <- tibble(file_path = list.files(file.path(PLwd, "peel_strain_files"))) %>%
    separate(
      col = file_path,
      into = c("drop1", "drop2", "graph_ref", "drop3", "k"),
      remove = FALSE,
      convert = TRUE,
      sep = "_"
    ) %>%
    mutate(k = k  %>% str_remove(., ".rds") %>%  as.integer) %>%
    select(-contains("drop")) %>%
    arrange(graph_ref) %>%
    filter(
      k ==0
    )
  
  
  #make a list of all the edge and node embeddings data for all 500 of the k= 0 graph data
  k0_embeddings <- 1:nrow(target_graphs)  %>% map(~{
    
    graph_id_choice <- .x
    k_choice <- 0
    
    file_path <-  target_graphs %>%
      filter(graph_ref ==graph_id_choice,
             k ==k_choice ) %>% pull(file_path) %>%
      file.path(PLwd,  "peel_strain_files",.  )
    embeddings_data <- readRDS(file_path)
    
    g_df <- as_data_frame(multi_quintet[[graph_id_choice]], what = "both")
    
    edge_data <- embeddings_data$edge_embeddings  %>%
      separate(., col= edge_name, into = c("from", "to"),sep = "_", remove = FALSE ) %>%
      left_join(g_df$vertices %>% rename_all(., ~paste0("from_", .)), by = c("from" = "from_node")) %>%
      left_join(g_df$vertices %>% rename_all(., ~paste0("to_", .)), by = c("to" = "to_node")) %>%
      mutate(graph_class = case_when(.x<101~ "A",
                                     .x <201 ~"B",
                                     .x <301 ~"C",
                                     .x <401 ~"D",
                                     TRUE~"E"),
             graph_id = .x,
             combo = paste(from_sub_class, to_sub_class))
    node_data <- embeddings_data$node_embeddings %>%
      left_join(g_df$vertices, by = "node") %>%
      mutate(graph_class =case_when(.x<101~ "A",
                                    .x <201 ~"B",
                                    .x <301 ~"C",
                                    .x <401 ~"D",
                                    TRUE~"E"),
             graph_id = .x)
    
    out_data <- list(edge_data = edge_data, node_data = node_data)
    
    return(out_data)
    
  }) %>% transpose(.) %>%
    map(~{
      reduce(., bind_rows)
      
    })
  
  
  
  #create a data frame that uses edge summary data for each node and 
  node_data2 <- bind_rows(
    k0_embeddings$edge_data %>%
      select(node = from, strain, tension, graph_class, graph_id),
    k0_embeddings$edge_data %>%
      select(node = to, strain, tension, graph_class, graph_id)
  ) %>%
    group_by(node, graph_class, graph_id) %>%
    summarise_all(., .funs = list(mean = mean, median = median)) %>%
    left_join(k0_embeddings$node_data %>% select(node, elevation, class, sub_class, graph_class, graph_id))
  
  saveRDS(node_data2 %>% ungroup, file.path(PLwd, "node_data2.rds"))
  
  # comm_df <-test$edge_data %>%
  #   filter(graph_class =="D")
  # 
  # test2 <- multi_quintet[[400]] %>% 
  #   set_edge_attr(., "weight", value = 1/comm_df$tension)
  # 
  #   test3 <-tibble(
  #   node = node_data2 %>% filter(graph_class =="D") %>% pull(node),
  #   tension = cluster_fast_greedy(test2) %>% membership(),
  #        nothing =  cluster_fast_greedy(test2, weights = NULL) %>% membership())
  # 
  groups <- expand_grid(graph_id =  1:500,
                class = c("A", "B"))
  
  #map communities using SETse walktrap and fast greedy against the true communities
  detected_communities <- 1:nrow(groups) %>%
    map_df(~{
      
      #only keep the node data from the part of the graph that will be analysed
      temp_df <-node_data2 %>%
        filter(graph_id ==groups$graph_id[.x],
               class == groups$class[.x])  %>%
        ungroup
      
      delete_class <- ifelse(groups$class[.x]=="A","B", "A") #The node class that should be deletex
      
      temp_g <- multi_quintet[[groups$graph_id[.x]]] 
      
      temp_g <- temp_g %>% delete_vertices(., v = which( vertex_attr(temp_g, "class") == delete_class) )
      
      temp_cluster <-  temp_df%>%
        select(elevation) %>%
        kmeans(., 2) 
      temp_df <-   temp_df %>%
        mutate(clustering_kmeans = temp_cluster$cluster,
               clustering_fast_greedy = cluster_fast_greedy(
                 temp_g,
                 merges = T
               ) %>% as.hclust %>%
                 cutree(., k = 2),
               clustering_walktrap = cluster_walktrap(
                 temp_g,
                 merges = T
               ) %>% as.hclust %>%
                 cutree(., k = 2),
               clustering_louvain = cluster_louvain( temp_g) %>% membership()
               
        )
      
      return(temp_df)
    }) 
  
  #Using the full graph. aka not the full information
  detected_communities_full <- 1:nrow(groups) %>%
    map_df(~{
      
      #keep the whole graph for the whoe graph test
      temp_df <-node_data2 %>%
        filter(graph_id ==groups$graph_id[.x])  %>%
        ungroup
      
      temp_g <- multi_quintet[[groups$graph_id[.x]]] 
      
      temp_cluster <-  temp_df%>%
        select(elevation) %>%
        kmeans(., 4) 
      temp_df <-  temp_df %>%
        mutate(clustering_kmeans_full = temp_cluster$cluster,
               clustering_fast_greedy_full = cluster_fast_greedy(
                 temp_g,
                 merges = T
               ) %>% as.hclust %>%
                 cutree(., k = 4),
               clustering_walktrap_full = cluster_walktrap(
                 temp_g,
                 merges = T
               ) %>% as.hclust %>%
                 cutree(., k = 4),
               clustering_louvain_full = cluster_louvain( temp_g) %>% membership()
        )
      
      return(temp_df)
      
    }) 
  
  detected_communities <-  detected_communities %>%
    left_join(., detected_communities_full %>% select(node, graph_class, graph_id,contains("clustering") )) %>%
    mutate_at( vars(contains("clustering")), ~ifelse(class =="A", ., .+2)) %>%
    group_by(graph_id) %>%
    mutate(tension_mean_range = (tension_mean-min(tension_mean))/(max(tension_mean)-min(tension_mean)),
           elevation_range =  (elevation-min(elevation))/(max(elevation)-min(elevation))) %>%
    ungroup
  
  saveRDS(detected_communities, file.path(PLwd, "peel_node_position.rds"))
  
  rm(detected_communities_full)
  rm(k0_embeddings)
  rm(target_graphs)
  rm(node_data2)
  
  
  
  
}