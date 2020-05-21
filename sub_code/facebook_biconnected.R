#Create the biconnected data for facebook the graphs with the time it takes to calculate.

1:100 %>%
  walk(~{
    
    file_name <- file.path("/home/jonno/setse_1_data/facebook100/facebook100_biconnected",
                           basename(uni_files[.x]))
    
    #only run if the file does not already exist
    #this prevents pointless re-running of the same graphs saving time
    if(!file.exists(file_name)){
      
      g <- readRDS(uni_files[.x]) %>%
        remove_small_components() 
      #get the data on the uni's
      uni_name <- basename(uni_files[.x]) %>% str_remove(., ".rds") %>% str_remove(., "[0-9]+")
      data_id <- basename(uni_files[.x]) %>% str_remove(., ".rds") %>% str_remove(., "[ a-zA-Z]+") %>% str_remove(., " ")
      
      start_time <- Sys.time()
      Out <- biconnected.components(g)
      stop_time <- Sys.time()
      
      names(Out)
      Out$time <- stop_time-start_time 
      
      saveRDS(Out, file = file_name ) 
    }
    
  })


file_paths <-list.files("/home/jonno/setse_1_data/facebook100/facebook100_biconnected", full.names = T)
biconnected_data <- 1:length(file_paths) %>%
  map_df(~{
    
    print(file_paths[.x])
    test <-readRDS(file_paths[.x])
    
    component_size <- (test$components) %>% map_dbl(length)
    
    tibble(
      file_name =  basename(uni_files[.x]) %>% str_remove(., ".rds"),
      total = test$no, 
      time = test$time,
      max_size = max(component_size),
      min = min(component_size),
      mean_size = mean(component_size),
      median_size = median(component_size),
      component_over_two = sum(component_size>2))
    
  })

rm(file_paths)