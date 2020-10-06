#create the combinations too test
combos <-expand_grid(file_name = str_remove(list.files(file.path(PLwd, "facebook_embeddings/facebook"), pattern = ".rds"), 
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
      
      performance_df  <- 1 %>% map_df(~{    
        
        #the other periods shouldn't be used as 2004 has so few occurances
        active_period <-2005 #list(2004L, 2005L, 2004:2005)[[.x]]
        
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
        
        
        g <- readRDS(list.files("/home/jonno/setse_1_data/facebook100/facebook100_igraph", full.names = T, pattern = file_name))
        
        #remove all irrelevant nodes aka the wrong years and only student class one and two
        #The neighbours who vote can come from any year as the embeddings used the relationships from all years
        g <- g 
        
        #The difference is caused as some of the targets have no friends in the active year
        #This means that either I have to keep all years but only analyse the results from the active year or
        #
        #g_df <- as_data_frame(g, what = "vertices") %>%
        #  left_join(data_node_details %>% mutate(target = as.integer(as.character(target)))%>% 
        #                                           select(name = node, from_target = target) ,.)
        
        g_list <-as_data_frame(g)# %>%
        #remove all edges that join nodes not in the dataset
        #filter(from %in% data_node_details$node, to %in% data_node_details$node)
        
        #create edge list with attribute of the same class
        voted_preds <- g_list %>%
          bind_rows(g_list %>% rename(to2 = from, from2 = to) %>% rename(from = from2, to = to2)) %>%
          #insert the cleaned target values to ensure the graph and the embeddings have the same values
          #the values have to be converted to integers otherwise there is some wierd side effects. They are converted back to factors in the next code block
          left_join(data_node_details %>% mutate(target = as.integer(as.character(target))) %>% 
                      select(node, from_target = target, year) , by =c("from" ="node")) %>%
          left_join(data_node_details %>% mutate(target = as.integer(as.character(target)))  %>% 
                      select(node, to_target = target), by =c("to" ="node")) %>%
          mutate(same_class = (to_target ==from_target)*1) %>%
          #as only data for the active years is included there is a lot of NA values.
          #these can be removed by keeping only complete cases
          filter(complete.cases(.)) %>%
          #get the counts for each year for every node
          group_by(from, from_target, to_target, year) %>%
          summarise(counts = n()) %>%
          #for each node find the year with most votes, also find if there is a tie
          #if a node is max and there is no tie then that year goes into the preds column otherwise it is 0
          group_by(from, from_target, year) %>%
          mutate(is_max = max(counts)== counts,
                 is_tie = sum(is_max)>1,
                 preds = ifelse(is_max & !is_tie, to_target, 0)) %>%
          group_by(from, year) %>%
          #summarise again get the truth and the prediction for each year
          summarise(truth = first(from_target),
                    preds = max(preds)) %>%
          ungroup %>%
          #remove the other years and the other student types
          filter(
            year %in% active_period,
            truth !=0) 
        
        #test <- left_join(data_node_details %>% select(from = "node", target),voted_preds)
        
        network_knn <- fb_metrics(voted_preds, truth = factor(truth, levels = 2:1), 
                                  estimate = factor(preds, levels = 2:1)) %>%
          rename(metric = .metric,
                 network_knn = .estimate) %>%
          select(-.estimator) %>%
          bind_cols(tibble(network_type1 = sum(voted_preds$truth==1), network_type2 = sum(voted_preds$truth==2)) %>%
                      #makes the data frame the same rows for all the metrics
                      slice(rep(1, length(attr(fb_metrics, "metrics")))))
        
        
        #sum the edges and divide by node degree. if value over 0.5 then it is correctly ID'ed.
        #if below 0.5 then it is incorrectly ID'd
        #get metrics of this result.
        #compare with setset... who wins? or how close does setse come?
        
        
        data_node_details_mod <-  data_node_details %>%
          #remove the other years and the other student types
          filter(
            year %in% active_period,
            target !=0) 
        
        
        #Two questions is setse better than chance? is it better than the actual graph?
        
        knn_perf <- seq(from = 1, to = 21, by =2) %>% map_df(~{
          mod <- class::knn.cv(train = data_node_details_mod %>% select(mean_tension2, elevation2), 
                               cl = fct_drop(data_node_details_mod$target), 
                               k = .x)
          
          tibble(truth = fct_drop(data_node_details_mod$target) %>% factor(., levels = 2:1),
                 estimate = mod %>% factor(., levels = 2:1)) %>%
            fb_metrics(truth = truth, estimate = estimate) %>%
            mutate(k = .x)
          
        }) %>%
          rename(metric = .metric,
                 estimate = .estimate) %>%
          select(-.estimator) %>%
          mutate(file_name = file_name,
                 active_period = paste(active_period, collapse = ", "),
                 student_1 = sum(data_node_details_mod$target==1),
                 student_2 = sum(data_node_details_mod$target==2)) %>%
          left_join(network_knn, by = "metric" ) 
        
        return(knn_perf)
        
        
        
      })
      
      saveRDS(performance_df, file = file.path(PLwd, "facebook_classifier", paste0(file_name, ".rds")))
      
    }
    
    
  })


