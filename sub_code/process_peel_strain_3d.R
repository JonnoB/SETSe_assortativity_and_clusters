#This i sthe same as process_peel_strian but for the 3d case. it just shows the system works in higher dimensions

if(file.exists(file.path(PLwd, "peels_quintet_processed","mulit_quintet_3d.rds"))){
  
  multi_quintet_3d <- readRDS(file.path(PLwd, "peels_quintet_processed","mulit_quintet_3d.rds"))
  
} else {
  
  #####
  ##
  ## Create the 3d version of the relationship between network parts
  ##
  #####
  
  C2B <- quintet %>%
    filter(class_1 != class_2) %>%
    mutate_at(., .vars = vars(sub_class_1:class_2), .funs = ~str_replace(., "A", "C"))
  
  C2A <- quintet %>%
    filter(class_1 != class_2) %>%
    mutate_at(., .vars = vars(sub_class_1:class_2), .funs = ~str_replace(., "B", "C"))
  
  C2C <-  quintet %>%
    filter(class_1 =="B", class_2 == "B") %>%
    mutate_at(., .vars = vars(sub_class_1:class_2), .funs = ~str_replace(., "B", "C"))
  
  quintet_3d <- bind_rows(quintet, C2B, C2A, C2C)
  
  
  class_data_df <- expand.grid(class = c("A", "B", "C"), sub = 1:2) %>%
    as_tibble() %>%
    mutate(sub_class = paste(class, sub, sep = "_"),
           size = 10 #replace with a vector if group sizes uneevn
    )
  
  #####
  ##
  ## 
  ##
  ##### 
  
  
  ##  
  #generate all the graphs. takes a few minutes
  ##  
  multi_quintet_3d <-c(1:200) %>% map(~{
    #set.seed so graphs are repeatable
    set.seed(.x)
    quintet_g_list <- LETTERS[1:5] %>%
      map(~create_assortivity_graph_from_subgroups(class_data_df, quintet_3d %>% rename(edges = .x), 10) %>%
            set.edge.attribute(., "type", value = .x) %>%
            set.graph.attribute(., "type", value = .x) %>%
            set.vertex.attribute(., "node", value = as.character(1:vcount(.)))
          
      ) 
    
    return(quintet_g_list)
  }) %>% transpose()  %>%
    map(~{
      
      tot_comps <-.x %>% map_dbl(~components(.x)$no)
      #checks to make sure there is only a single component
      single_comp_networks <- .x[tot_comps==1]
      
      return(single_comp_networks[1:100])    
      
    }) %>% flatten()
  
  write_rds(multi_quintet_3d,  path = file.path(PLwd, "peels_quintet_processed","mulit_quintet_3d.rds"))
}




k_options <- expand_grid(sub = c(1,0.5,0), class = c(1,0.5,0), inter = c(1,0.5,0)) %>%
  filter((sub+class+inter) ==1.5 & (sub==0 | class ==0 | inter==0)) %>%
  mutate(k_id =paste0("k_", 1:n())) %>%
  bind_rows(tibble(k_id = "k_0", sub = 1, class = 1, inter =1))

k_options2 <- k_options %>%
  pivot_longer(cols = sub:inter, names_to = "match_type", values_to = "k_fract") %>%
  mutate(k = k_fract*1000+100) %>%
  select(-k_fract) %>%
  pivot_wider(., names_from = k_id,  values_from = k)


setse_3d <-1:500 %>% map_df(~{
  start_time <- Sys.time()
  
  graph_ref <- .x
  g_out <- multi_quintet_3d[[graph_ref]]
  
  graph_type <- graph_attr(g_out, "type")
  
  g_df <- as_data_frame(g_out, what = "both")
  
  edge_df <- bind_cols(g_df$edges %>% mutate(edge_name = paste(from, to, sep = "_")), g_df$vertices %>% 
                         slice(pull(g_df$edges[1])) %>% 
                         rename(from_class = class, from_sub_class = sub_class)) %>%
    bind_cols(.,g_df$vertices %>% 
                slice(pull(g_df$edges[2])) %>% 
                rename(to_class = class, to_sub_class = sub_class) ) %>%
    mutate(match_type = case_when(
      from_sub_class== to_sub_class ~ "sub",
      from_class == to_class ~ "class",
      TRUE ~"inter"
    ),
    distance = 1,
    ) %>%
    left_join(k_options2, by = "match_type")
  
  out_3d <-LETTERS[1:3] %>% map(~{
    g_out <-graph_from_data_frame(edge_df, directed = FALSE,  g_df$vertices %>% select(node, class, sub_class) %>%
                                    mutate(force = ifelse(class==.x, 1/20, -1/40)))
    
    setse_complete <- auto_SETSe(g_out,
                                 force = "force",
                                 distance = "distance",
                                 edge_name = "edge_name",
                                 k = paste0("k_", 0),
                                 tol = sum(abs(vertex_attr(g_out, "force")))/10000,
                                 sparse = FALSE, #The networks are small so dense networks are faster approx x3 in this case
                                 verbose = F)
    
    #by comparing the average tension experienced by each node we can 
    edge_tension_per_node <- setse_complete$edge_embeddings %>%
      select(edge_name, tension) %>%
      separate(col = edge_name, into = c("from", "to"), sep = "_") %>%
      pivot_longer(cols =c(from, to), names_to = "type", values_to = "node") %>%
      group_by( node) %>%
      summarise(mean_tension_per_node = mean(tension),
                total_edges = n()) %>%
      summarise(mean_tension_per_node = mean(mean_tension_per_node)) #this takes advantage of the skew in the mean to aid differentiation
    #some nodes may have the same total tension but spread across different amount of nodes
    
    node_details <- left_join(setse_complete$node_embeddings, g_df$vertices, by = "node")
    
    
    setse_complete$graphsummary <- tibble(
      strain = mean(abs(setse_complete$edge_embeddings$strain)),
      mean_A_elevation = node_details %>% 
        filter(class=="A") %>%
        pull(elevation) %>% mean,
      mean_abs_elevation = mean(abs(node_details$elevation)),
      median_abs_elevation =  median(abs(node_details$elevation)),
      euc_elevation = sqrt(sum(node_details$elevation^2)),
      tension = mean(abs(setse_complete$edge_embeddings$tension)),
      residual_force =  mean(abs(setse_complete$node_embeddings$static_force))) %>%
      bind_cols(edge_tension_per_node )
    
    
    return(setse_complete)
  }) %>% transpose()

  euc_ten <- cbind(
    out_3d$edge_embeddings[[1]]$tension, 
    out_3d$edge_embeddings[[2]]$tension,
    out_3d$edge_embeddings[[3]]$tension) %>%
    as_tibble() %>%
    mutate(
      edge_name = out_3d$edge_embeddings[[1]]$edge_name,
      euc_ten = sqrt(V1^2+ V2^2+ V3^2)) 
  
  euc_elev <- cbind(
    out_3d$node_embeddings[[1]]$elevation, 
    out_3d$node_embeddings[[2]]$elevation,
    out_3d$node_embeddings[[3]]$elevation) %>%
    as_tibble() %>%
    mutate(
      node = out_3d$node_embeddings[[1]]$node,
      euc_elev = sqrt(V1^2+ V2^2+ V3^2)) 
  
  
  #by comparing the average tension experienced by each node we can 
  edge_tension_per_node <- euc_ten %>%
    select(edge_name, tension = euc_ten) %>%
    separate(col = edge_name, into = c("from", "to"), sep = "_") %>%
    pivot_longer(cols =c(from, to), names_to = "type", values_to = "node") %>%
    group_by( node) %>%
    summarise(mean_tension_per_node = mean(tension),
              total_edges = n()) 
  
  
  out <- left_join(euc_elev, edge_tension_per_node) %>%
    summarise(
      mean_euc_elev = mean(euc_elev),
      mean_tension_per_node = mean(mean_tension_per_node))  %>%#this takes advantage of the skew in the mean to aid differentiation
    #some nodes may have the same total tension but spread across different amount of nodes
    mutate(
      graph_type = graph_type,
      graph_id = graph_ref
    )
  
  end_time <- Sys.time()
  
  print(paste("graph ", .x, ". time ", as.numeric(end_time-start_time), "seconds", sep = " "))
  
  return(out)
  
})

  
  