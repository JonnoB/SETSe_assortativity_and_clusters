
#create the combinations too test
combos <-expand_grid(file_name = str_remove(list.files("/home/jonno/setse_1_data/facebook_embeddings/HPC_embeddings"), 
                                            pattern = ".rds"))

if(!dir.exists(file.path(PLwd, "facebook_classifier_year"))){
  dir.create(file.path(PLwd, "facebook_classifier_year"))
}

1:nrow(combos) %>%
  walk(~{
    
    file_number <- .x
    file_name <- combos$file_name[.x]
    
    if(file.exists(file.path(PLwd, "facebook_classifier_year", paste0(file_name, ".rds")))){
      
      print(paste("Iteration", .x ,"file", file_name, "exists", "proceeeding to next file"))
      
    } else{
      

        print(paste("file number", file_number  ,"file", file_name))
        embeddings_data <- readRDS(file.path(PLwd, "facebook_embeddings/HPC_embeddings",paste0(file_name, ".rds")))
        
        data_node_details <- embeddings_data$node_detail %>%
          mutate(
            target = ifelse((year %% 1)==0 & year %in% 2003:2009, year, 0),
            target =  ifelse(is.na(target), 0, target),
            target = factor(target, levels = c(0,2003:2009)),
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
        voted_preds <- g_list$edges %>%
          bind_rows(g_list$edges %>% rename(to2 = from, from2 = to) %>% rename(from = from2, to = to2)) %>%
          #insert the cleaned target values to ensure the graph and the embeddings have the same values
          #the values have to be converted to integers otherwise there is some wierd side effects. They are converted back to factors in the next code block
          left_join(data_node_details %>% mutate(target = as.integer(as.character(target))) %>% select(node, from_target = target) , by =c("from" ="node")) %>%
          left_join(data_node_details %>% mutate(target = as.integer(as.character(target)))  %>% select(node, to_target = target), by =c("to" ="node")) %>%
          mutate(same_class = (to_target ==from_target)*1) %>%
          #get the counts for each year for every node
          group_by(from, from_target, to_target) %>%
          summarise(counts = n()) %>%
          #for each node find the year with most votes, also find if there is a tie
          #if a node is max and there is no tie then that year goes into the preds column otherwise it is 0
          group_by(from, from_target) %>%
          mutate(is_max = max(counts)== counts,
                 is_tie = sum(is_max)>1,
                 preds = ifelse(is_max & !is_tie, to_target, 0)) %>%
          group_by(from) %>%
          #summarise again get the truth and the prediction for each year
          summarise(truth = first(from_target),
                    preds = max(preds))

     
        network_knn <- fb_metrics(voted_preds, truth = factor(truth, levels = c(0,2003:2009)), 
                                  estimate = factor(preds, levels = c(0,2003:2009))) %>%
          rename(metric = .metric,
                 network_knn = .estimate) %>%
          select(-.estimator)
        
        
        #sum the edges and divide by node degree. if value over 0.5 then it is correctly ID'ed.
        #if below 0.5 then it is incorrectly ID'd
        #get metrics of this result.
        #compare with setset... who wins? or how close does setse come?
        
        #Two questions is setse better than chance? is it better than the actual graph?
        
        knn_perf <- seq(from = 1, to = 21, by =2) %>% map_df(~{
          mod <- class::knn.cv(train = data_node_details %>% select(mean_tension2, elevation2), 
                               cl = fct_drop(data_node_details$target), 
                               k = .x)
          
          tibble(truth = fct_drop(data_node_details$target),
                 estimate = mod) %>%
            fb_metrics(truth = truth, estimate = estimate) %>%
            mutate(k = .x)
          
        }) %>%
          rename(metric = .metric,
                 estimate = .estimate) %>%
          select(-.estimator) %>%
          mutate(file_name = file_name) %>%
          left_join(network_knn, by = "metric" ) 
        

      
      saveRDS(knn_perf, file = file.path(PLwd, "facebook_classifier_year", paste0(file_name, ".rds")))
      
    }
    
    
  })



