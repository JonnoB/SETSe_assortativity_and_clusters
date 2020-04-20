# facebook uni embeddings
#This subscript finds the embeddings of all the uni's it does it in sparse mode to attempt to stop the ram exploding
#with the large matrices, as the matrices are super sparse.

uni_stats %>%
  arrange(nodes) %>%
  pull(uni_name) %>% {.[c(1:20,22:100)]} %>%
  walk(~{
    
    # uni_name <- uni_stats %>%
    #   arrange(nodes) %>%
    #   pull(uni_name) %>% {.[.x]}
    # 
    # file_id <- uni_stats %>%
    #   arrange(nodes) %>%
    #   pull(file_id) %>% {.[.x]}
    
    start_time <- Sys.time()
    
    uni_pattern <- grep(.x, uni_files )
    
    g <- readRDS(uni_files[uni_pattern])  %>% #load file
      remove_small_components()  %>%
      facebook_year_clean() %>% prepare_SETSe_continuous(., k = 1000, force_var = "year", 
                                                         sum_to_one = FALSE, 
                                                         distance = 100000) 
    
    embeddings_data <- auto_SETSe2(g, tstep = 0.1, verbose = TRUE, sparse = T, sample = 100)
    
    
    node_detail <- embeddings_data$edge_embeddings %>% tibble() %>%
      separate(., col = edge_name, into = c("from","to"), sep = "-") %>%
      select(from, to, tension) %>%
      pivot_longer(cols = c(from, to), names_to = "node_type", values_to = "node") %>%
      select(tension, node) %>%
      group_by(node) %>%
      summarise(sum_tension = sum(tension),
                mean_tension = mean(tension)) %>%
      left_join(embeddings_data$node_embeddings %>% tibble(), by = "node") %>%
      left_join(as_data_frame(g, what = "vertices"), by = c("node"="name")) %>%
      mutate(uni = .x)
    
  stop_time <- Sys.time()
  time_diff <- stop_time-start_time 
   
   results <-list(embeddings_data = embeddings_data,
                node_detail = node_detail,
                time_taken = time_diff)
   
   file_name <- file.path("/home/jonno/setse_1_data/facebook_embeddings/facebook_year",
                          paste0(.x, ".rds"))
   
   saveRDS(results, file = file_name )
    
    
  })
