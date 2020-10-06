
#create the combinations too test
combos <-expand_grid(file_name = str_remove(list.files(file.path(PLwd, "facebook_embeddings/facebook"), pattern = ".rds"), 
                                            pattern = ".rds"))
#graph folders
folder_path <- "/home/jonno/setse_1_data/facebook100/facebook100_igraph"

if(!dir.exists(file.path(PLwd, "facebook_classifier"))){
  dir.create(file.path(PLwd, "facebook_classifier"))
}

1:nrow(combos) %>%
  walk(~{
    
    #ID number of iteration
    file_number <- .x
    #Uni to be analysed
    file_name <- combos$file_name[.x]
    
    #Check to see if the analysis has already been performed.
    #If it has skip to next uni
    #The analysis is quite slow so this avoids uneccessary re-calculation
    if(file.exists(file.path(PLwd, "facebook_classifier", paste0(file_name, ".rds")))){
      
      print(paste("Iteration", .x ,"file", file_name, "exists", "proceeeding to next file"))
      
    } else{
      
      #the other periods shouldn't be used as 2004 has so few occurances
      active_period <-2005
      
      print(paste("file number", file_number  ,"file", file_name, "active_period", paste(active_period, collapse = ", ")))
      embeddings_data <- readRDS(file.path(PLwd, "facebook_embeddings/facebook",paste0(file_name, ".rds")))
      
      #All data from the network prepped for prediction
      data_node_details <- embeddings_data$node_detail %>% 
        mutate(
          target = ifelse(is.na(student_faculty) , student_faculty,0),
          target = ifelse(student_faculty %in% 1:2, student_faculty, 0 ),
          target = factor(target, levels = 0:2),
          euc_tension2 = (euc_tension-mean(euc_tension))/sd(euc_tension),
          mean_tension2 = (mean_tension-mean(mean_tension))/sd(mean_tension),
          elevation2 = (elevation-mean(elevation))/sd(elevation))  %>%
        select(node, target, euc_tension2:elevation2, year)
      
      #create the performance df for the naive neighbour voting method.
      network_vote_performance <- create_network_vote_performance(
        folder_path = folder_path, 
        file_name, 
        data_node_details,
        active_period)
      
      #get the embeddings for all the different networks into a dataframe ready to be analysed for performance
      embeddings <- clean_embeddings_pre_eval(folder_path, file_name, active_period, data_node_details)

      
      knn_perf <- unique(embeddings$model) %>%
        map_df(~{
          get_embeddings_knn_performance(embeddings %>% filter(model ==.x), active_period, network_vote_performance)
          
        })

      
      #Two questions is setse better than chance? is it better than the actual graph?
      #This knn evaluates the embeddings produced by SETSe. It then left joins on the majority vote results of the network

      
      #save the performance of the embeddings method
      saveRDS(knn_perf, file = file.path(PLwd, "facebook_classifier", paste0(file_name, ".rds")))
      
    }
    
    
  })



