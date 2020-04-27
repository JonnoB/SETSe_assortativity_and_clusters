

if(file.exists(file.path(PLwd, "peels_quintet_processed","mulit_quintet.rds"))){
  
  multi_quintet <- readRDS(file.path(PLwd, "peels_quintet_processed","mulit_quintet.rds"))
  
} else {
  
  ##  
  #generate all the graphs. takes a few minutes
  ##  
  
  #creates a list of 200 elements where each element contains a list of 5 elements where each element is a random
  #example of each of the peels quintet types
  multi_quintet <-c(1:200) %>% map(~{
    #set.seed so graphs are repeatable
    set.seed(.x)
    quintet_g_list <- LETTERS[1:5] %>%
      map(~create_assortivity_graph_from_subgroups(class_data_df, quintet %>% rename(edges = .x), 10) %>%
            set.edge.attribute(., "type", value = .x) %>%
            set.graph.attribute(., "type", value = .x) %>%
            set.vertex.attribute(., "node", value = as.character(1:vcount(.)))
          
      ) 
    
    return(quintet_g_list)
    #transposes the list to make a list 5 elements long where each element is a list of 200
  }) %>% transpose()  %>%
    map(~{
      
      tot_comps <-.x %>% map_dbl(~components(.x)$no)
      #checks to make sure there is only a single component
      single_comp_networks <- .x[tot_comps==1]
      
      return(single_comp_networks[1:100])    
      
    }) %>% flatten()
  
  write_rds(multi_quintet,  path = file.path(PLwd, "peels_quintet_processed","mulit_quintet.rds"))
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

 

#This adds in lots of data to the edges of the network.
#The node data allows the edges to be assigned to a within group between group class.
#the k values are then assigned based on the edge class
1:500 %>% walk(~{
  start_time <- Sys.time()
  
  #take the instance of randomly generated Peels quintet graph
  graph_ref <- .x
  g_out <- multi_quintet[[graph_ref]]
  
  #get the name of the graph type A to E
  graph_type <- graph_attr(g_out, "type")
  
  #convert to a list of dataframes
  g_df <- as_data_frame(g_out, what = "both")
  
  #The first two joins merge the node data onto the edges for the "from" and "to" nodes, this gives the group and sub group data
  #The third join adds in the k values
  edge_df <- left_join( #first join
    g_df$edges %>% mutate(edge_name = paste(from, to, sep = "_"),
                          from = as.character(from),
                          to = as.character(to)),
    g_df$vertices %>%
      rename(from_class = class, from_sub_class = sub_class),
    by = c("from"="node")) %>%
    left_join(.,    #second join
              g_df$vertices %>%
                rename(to_class = class, to_sub_class = sub_class),
              by = c("to"="node")) %>%
    mutate(
      match_type = case_when(
        from_sub_class == to_sub_class ~ "sub",
        from_class == to_class ~ "class",
        TRUE ~ "inter"
      ),
    #  flow = 1, no longer necessary due to upgraded SETSe
      distance = 1,
    #  capacity = 1
    ) %>%
    left_join(k_options2, by = "match_type") #third and final join
  
  #Remake the graph including the addition edge data
  g_out <-graph_from_data_frame(edge_df, directed = FALSE,  g_df$vertices %>% select(node, class, sub_class) %>%
                                  mutate(force = ifelse(class=="A", 1/20, -1/20))) #Forces sum to 1
  
  means_df <-0:6 %>% map_df(~{
    
    
    
    save_path <- file.path(PLwd, "peel_strain_files", paste0("graph_ref_", graph_ref, "_k_", .x, ".rds") )
if(!file.exists(save_path)){    
  setse_complete <- auto_SETSe(g_out,
                               force = "force",
                               distance = "distance",
                               edge_name = "edge_name",
                               k = paste0("k_", .x),
                               tol = sum(abs(vertex_attr(g_out, "force")))/10000,
                               sparse = FALSE, #The networks are small so dense networks are faster approx x3 in this case
                               verbose = F)
    
    
#The biconnected components don't work due to a naming error why?   

  # SETSe_bicomp2(.,
  #               force = "force",
  #               distance = "distance",
  #               edge_name = "edge_name",
  #               k = "k",
  #               tstep = 0.02,
  #               tol = sum(abs(vertex_attr(., "force")))/10000,
  #               static_limit = 1,
  #               sparse = F,
  #               verbose = TRUE)
  
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
      bind_cols(edge_tension_per_node ) %>%
      mutate(
        k =.x,
        graph_type = graph_type,
             graph_id = graph_ref)
    
    saveRDS(setse_complete, save_path)
    }

  })
  
  end_time <- Sys.time()
  
  print(cat(paste("graph ", .x, ". time ", sep = ""), difftime(end_time,start_time)))
  
})


peel_strain_sum <- list.files(file.path(PLwd, "peel_strain_files"), full.names= T) %>% 
  map_df(~{
    setse_complete <- readRDS(.x)
    Out  <-setse_complete$graphsummary 
    return(Out)
  })
  
  