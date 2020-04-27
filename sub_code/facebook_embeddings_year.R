# facebook uni embeddings
#This subscript finds the embeddings of all the uni's it does it in sparse mode to attempt to stop the ram exploding
#with the large matrices, as the matrices are super sparse.

uni_stats %>%
  arrange(nodes) %>%
  pull(file_name) %>%
  walk(~{
    
    uni_pattern <- grep(.x, uni_files )
    
    file_name <- file.path("/home/jonno/setse_1_data/facebook_embeddings/facebook_year",
                           paste0(.x, ".rds"))
    
    if(file.exists(file_name)){
      
      print(paste("file", basename(file_name), "exists. proceeding to next file"))
      
    } else{
      start_time <- Sys.time()
      
      g <- readRDS(uni_files[uni_pattern])  %>% #load file
        remove_small_components()  %>%
        facebook_year_clean() %>% prepare_SETSe_continuous(., k = 1000, force_var = "year", 
                                                           sum_to_one = FALSE, 
                                                           distance = 100) 
      
      embeddings_data <- SETSe_bicomp2(g, 
                                       tstep = 0.01,
                                       mass = 1,#sum(abs(vertex_attr(g, "force"))/2)/vcount(g), variable mass is useful when force is constant
                                       tol = sum(abs(vertex_attr(g, "force")))/1000,
                                       verbose = TRUE, 
                                       sparse = TRUE, 
                                       sample = 100,
                                       static_limit = sum(abs(vertex_attr(g, "force")))) #if after 100 turns the static_force is more than the starting force. just stop.
      
      node_detail <- embeddings_data$edge_embeddings %>% tibble() %>%
        separate(., col = edge_name, into = c("from","to"), sep = "-") %>%
        select(from, to, tension) %>%
        pivot_longer(cols = c(from, to), names_to = "node_type", values_to = "node") %>%
        select(tension, node) %>%
        group_by(node) %>%
        summarise(sum_tension = sum(tension),
                  mean_tension = mean(tension),
                  median_tension = median(tension),
                  euc_tension = sqrt(sum(tension^2))) %>%
        left_join(embeddings_data$node_embeddings %>% tibble(), by = "node") %>%
        left_join(as_data_frame(g, what = "vertices"), by = c("node"="name")) %>%
        mutate(uni = .x)
      
      stop_time <- Sys.time()
      time_diff <- tibble(uni  = .x, start_time = start_time, stop_time = stop_time)
      
      results <-list(embeddings_data = embeddings_data,
                     node_detail = node_detail,
                     time_taken = time_diff)
      
      saveRDS(results, file = file_name )
    }
    
  })
