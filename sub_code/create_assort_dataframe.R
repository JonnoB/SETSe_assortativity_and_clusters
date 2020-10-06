
#creates the embedding mean embedding values for assortativity


if(file.exists(file.path(PLwd, "embedded_assortativity.rds"))){
  
  embedded_assort <- read_rds(file.path(PLwd, "embedded_assortativity.rds"))
  
} else {
  
  
  #create the combinations too test
  combos <-expand_grid(file_name = str_remove(list.files(file.path(PLwd, "facebook_embeddings/facebook"), pattern = ".rds"), 
                                              pattern = ".rds"))
  
  all_embeddings <- 1:nrow(combos) %>%
    map_df(~{
      
      #ID number of iteration
      file_number <- .x
      #Uni to be analysed
      file_name <- combos$file_name[.x]
      
      
      print(paste("file number", file_number  ,"file", file_name))
      embeddings_data <- readRDS(file.path(PLwd, "facebook_embeddings/facebook",paste0(file_name, ".rds")))
      
      #get the embeddings for all the different networks into a dataframe ready to be analysed for performance
      other_embeddings <- mean_embeddings_value (folder_path, file_name)
      
      
    })
  
  
  node_details_df <- readRDS(
    file.path("/home/jonno/setse_1_data/facebook_embeddings/processed_embeddings",
              "facebook_node_detail.rds" )) %>%
    mutate(abs_elevation = abs(elevation)) %>%
    group_by(file_name) %>%
    summarise_at(., vars(sum_tension:elevation , abs_elevation), mean, na.rm = T) %>%
    left_join(uni_stats, by = "file_name") %>%
    mutate(model = "SETSe") %>%
    select(model, mean_X1 = mean_tension, 
           mean_X2 = abs_elevation, 
           mean_abs_X1 = mean_tension, 
           mean_abs_X2 = abs_elevation,
           file_name)
  
  
  out <- bind_rows(all_embeddings, node_details_df) %>%
    left_join(uni_stats, by = "file_name") %>%
    mutate(node_edge = nodes/edges)
  
  
  #save the performance of the embeddings method
  saveRDS(out, file = file.path(PLwd, "embedded_assortativity.rds"))
  
  #cleanup
  rm(out);rm(node_details_df);rm(all_embeddings);rm(combos)
  
}


