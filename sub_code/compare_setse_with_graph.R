
#create the combinations too test
combos <-expand_grid(file_name = str_remove(list.files("/home/jonno/setse_1_data/facebook_embeddings/HPC_embeddings"), 
                                            pattern = ".rds"))

if(!dir.exists(file.path(PLwd, "facebook_classifier"))){
  dir.create(file.path(PLwd, "facebook_classifier"))
}

1:nrow(combos) %>%
  walk(~{
    
    file_number <- .x
    file_name <- combos$file_name[.x]
      
      if(file.exists(file.path(PLwd, "facebook_classifier", paste0(file_name, ".rds")))){
        
        print(paste("Iteration", .x ,"file", file_name, "exists", "proceeeding to next file"))
        
      } else{
        
        performance_df  <- 1:3 %>% map_df(~{    
        
          active_period <- list(2004L, 2005L, 2004:2005)[[.x]]
          
        print(paste("file number", file_number  ,"file", file_name, "active_period", paste(active_period, collapse = ", ")))
        embeddings_data <- readRDS(file.path(PLwd, "facebook_embeddings/HPC_embeddings",paste0(file_name, ".rds")))
        
        data_node_details <- embeddings_data$node_detail %>% filter(
          (year %% 1)==0,
          year %in% active_period,
          student_faculty %in% 1:2) %>%
          mutate(
            target = factor(student_faculty, levels = 1:2),
            euc_tension2 = (euc_tension-mean(euc_tension))/sd(euc_tension),
            mean_tension2 = (mean_tension-mean(mean_tension))/sd(mean_tension),
            elevation2 = (elevation-mean(elevation))/sd(elevation)) 
        
        g <- readRDS(list.files("/home/jonno/setse_1_data/facebook100/facebook100_igraph", full.names = T, pattern = file_name))
        
        #remove all irrelevant nodes aka the wrong years and only student class one and two
        #The neighbours who vote can come from any year as the embeddings used the relationships from all years
        g <- g %>%
          remove_small_components() 
        
        g_list <-as_data_frame(g, what = "both")
        
        #create edge list with attribute of the same class
        g_edges <- g_list$edges %>%
          left_join(g_list$vertices %>% select(name, from_student = student_faculty), by =c("from" ="name")) %>%
          left_join(g_list$vertices %>% select(name, to_student = student_faculty), by =c("to" ="name")) %>%
          mutate(same_class = (to_student ==from_student)*1) %>%
          select(from, to, same_class)
        
        
        g <- graph_from_data_frame(g_edges, directed = F, vertices = g_list$vertices)
        
        #make_adjacency matrix weighted by the same_class edge attribute.
        same_class_vect <- Matrix::colSums(as_adjacency_matrix(g, type = "both", attr = "same_class"))
        
        same_class_df <- tibble(node = names(same_class_vect), same_class = same_class_vect, degree = degree(g), 
                                fraction = same_class/degree,
                                correct = fraction>0.5)
        
        same_class_df <- data_node_details %>%
          #left joining on to the data_node_details section  removes all nodes not inlcuded in the analysis
          left_join(.,same_class_df, by = "node") %>%
          mutate(predictions = case_when(correct ~ student_faculty,
                                         !correct & student_faculty ==1 ~ 2,
                                         !correct & student_faculty ==2 ~ 1,
                                         is.na(correct) & student_faculty ==1 ~ 2,
                                         is.na(correct) & student_faculty ==2 ~ 1,
                                         TRUE ~0))%>%
          filter(predictions!=0) #I don't think it is possible to have a zero but just incase I will remove them
        
        
        network_knn <- fb_metrics(same_class_df, truth = factor(student_faculty, levels = 1:2), 
                                  estimate = factor(predictions, levels = 1:2)) %>%
          rename(metric = .metric,
                 network_knn = .estimate) %>%
          select(-.estimator)
        
        
        #sum the edges and divide by node degree. if value over 0.5 then it is correctly ID'ed.
        #if below 0.5 then it is incorrectly ID'd
        #get metrics of this result.
        #compare with setset... who wins? or how close does setse come?
        
        #Two questions is setse better than chance? is it better than the actual graph?
        
        knn_perf <- seq(from = 1, to = 21, by =2) %>% map_df(~{
          mod <- class::knn.cv(train = data_node_details %>% select(mean_tension2, euc_tension2, elevation2), 
                               cl = factor(data_node_details$target, levels = 1:2), 
                               k = .x)
          
          tibble(truth = data_node_details$target,
                 estimate = mod) %>%
            fb_metrics(truth = truth, estimate = estimate) %>%
            mutate(k = .x)
          
        }) %>%
          rename(metric = .metric,
                 estimate = .estimate) %>%
          select(-.estimator) %>%
          mutate(file_name = file_name,
                 active_period = paste(active_period, collapse = ", "),
                 student_1 = sum(ifelse(data_node_details$student_faculty==1, 1, 0)),
                 student_2 = sum(ifelse(data_node_details$student_faculty==1, 0, 1))) %>%
          left_join(network_knn, by = "metric" ) 
        
        return(knn_perf)
        
      
      
    })
    
    saveRDS(performance_df, file = file.path(PLwd, "facebook_classifier", paste0(file_name, ".rds")))
    
      }
    
    
  })



