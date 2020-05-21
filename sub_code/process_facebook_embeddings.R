
#this loads the facebook embeddings then saves each list element as a separate file so as not to clog up the memory

if(!(length(list.files(file.path("/home/jonno/setse_1_data/facebook_embeddings", 
                                 "processed_embeddings")))==7)){
  
  file_paths <- list.files("/home/jonno/setse_1_data/facebook_embeddings/HPC_embeddings", full.names = T)
  
  facebook_embeddings_data <- 1:length(file_paths) %>%
    map(~{
      
      print(.x)
      file_name <- basename(file_paths)[.x]
      readRDS(file_paths[.x]) %>%
        flatten() %>%
        map(~{
          
          Out <-  .x %>% mutate(file_name = str_remove(file_name, ".rds"))
          
          
          return(Out)
        })
      
    }) %>% transpose() 
  
  
  embeddings_names <-names(facebook_embeddings_data)
  
  embeddings_names %>%
    walk(~{
      facebook_embeddings_data[[.x]] %>%
        bind_rows() %>%
        saveRDS(., file.path("/home/jonno/setse_1_data/facebook_embeddings", 
                             "processed_embeddings", paste0("facebook_", .x, ".rds")))
      
    })
  
  rm(embeddings_names)
  rm(facebook_embeddings_data)
  rm(file_paths)
}
