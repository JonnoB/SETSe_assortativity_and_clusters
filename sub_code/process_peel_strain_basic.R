

#This script generates all 500 random examples of the basic peels quintet classes. 
#It takes a long time and all that is needed is the final output.
#This script simplifies the code in the main markdown and speeds it up after the first run by only requiring the final files.

if(file.exists( file.path(PLwd, "peels_quintet_processed","peel_strain_sum.rds")) ){
  
  peel_strain_sum <- readRDS( file.path(PLwd, "peels_quintet_processed","peel_strain_sum.rds"))
  
} else {
  
  ##  
  #generate all the graphs. takes a few minutes
  ##  
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
  }) %>% transpose()  %>%
    map(~{
      
      tot_comps <-.x %>% map_dbl(~components(.x)$no)
      #checks to make sure there is only a single component
      single_comp_networks <- .x[tot_comps==1]
      
      return(single_comp_networks[1:100])    
      
    }) %>% flatten()
  
  write_rds(multi_quintet,  path = file.path(PLwd, "peels_quintet_processed","mulit_quintet.rds"))
  
  # multi_quintet %>%
  #   map_dbl(~components(.x)$no)
  
  ##
  #calculate strains This can take several hours
  ##
  
  #This is out of date and should be replaced by auto_setse or something like that
  1:length(multi_quintet) %>% walk(~{
    print(.x)
    
    g <- multi_quintet[[.x]]
    filename <- file.path(PLwd, "peels_quintet_strain_files", paste0("Network_", get.graph.attribute(g, "type"), "_", .x, ".rds") )
    
    current_graph <- peels_spring_prep(g)
    
    #The networks are small so can be done in 1 go, they also are pretty much a single block anyway
    solved_height_network <- Find_network_balance(g = current_graph , 
                                                  force ="Net_Generation",
                                                  flow = "flow",
                                                  distance = "distance",
                                                  capacity = "Link.Limit",
                                                  tstep = 0.005, 
                                                  tol = 1e-10, 
                                                  maxIter = 20000, 
                                                  mass = 1)
    
    write_rds(solved_height_network,  path = filename)
    
  })
  
  
  
  #get file paths of embeddings
  files <- list.files(file.path(PLwd, "peels_quintet_strain_files" ), full.names = TRUE)
  ##
  #process strains and heights
  ##
  strain_solution_peel <-1:length(files) %>% map_df(~{
    NodeList <- read_rds(files[.x])$NodeList %>%
      rename(elevation = z)
    
    peels_spring_prep(multi_quintet[[.x]]) %>%
      calc_tension_strain(., NodeList, "distance", "Link.Limit", "flow", edge_name = "Link") %>%
      mutate(Network = str_split(basename(files[.x]), pattern = "_")[[1]][2],
             graph = str_split(basename(files[.x]), pattern = "_")[[1]][3] %>% gsub(".rds", "", .))
    
  })
  
  height_solution_peel <-1:length(files) %>% map_df(~{
    NodeList <- read_rds(files[.x])$NodeList %>%
      left_join(multi_quintet[[.x]] %>% 
                  as_data_frame(what = "vertices") %>%
                  mutate(node = 1:n() %>% as.character), by = "node")  %>%
      mutate(Network = str_split(basename(files[.x]), pattern = "_")[[1]][2],
             graph = str_split(basename(files[.x]), pattern = "_")[[1]][3] %>% gsub(".rds", "", .))
    
    
  })
  
  saveRDS(strain_solution_peel, file.path(PLwd, "peels_quintet_processed","strain_solution_peel.rds"))
  saveRDS(height_solution_peel, file.path(PLwd, "peels_quintet_processed","height_solution_peel.rds"))
  
  
  #combine into a single datagrale
  peel_strain_sum <- strain_solution_peel %>%
    group_by(Network, graph) %>%
    summarise(mean_strain = mean(strain),
              median_strain = median(strain),
              sd_strain = sd(strain),
              mean_tension = mean(tension),
              median_tension = median(tension),
              sd_tension = sd(tension)) %>%
    left_join(height_means_by_group %>%
                filter(class=="A"))
  left_join(height_means)
  
  saveRDS(peel_strain_sum, file = file.path(PLwd, "peels_quintet_processed","peel_strain_sum.rds"))
  
  rm(strain_solution_peel);rm(height_solution_peel);rm(files);rm(multi_quintet)
}
