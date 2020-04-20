#This script converts the .mat files to igraph files.

facebook100_files <- list.files("/home/jonno/setse_1_data/facebook100/facebook100", pattern = ".mat", full.names = T) 

facebook100_files <- facebook100_files[!grepl("schools.mat", facebook100_files)]

1:length(facebook100_files) %>%
  walk(~{
    
    file_name_load <- facebook100_files[.x]
    
    uni_name <- str_remove(basename(file_name_load), ".mat")
    
    print(paste("Uni", .x,uni_name))
    
    #read univeristy file
    test <- readMat(file_name_load)
    
    #add in student number as explicit colunm header
    #this means the student id is definately in the order we expect
    colnames(test$A) <- 1:ncol(test$A)
    
    #convert to graph using the column name ids
    test_g <- graph_from_adjacency_matrix(test$A, mode = "undirected", add.colnames = "names")
    #convert back into a data frame edge list
    test_df <- as_data_frame(test_g) %>% tibble()
    #process the local infor file nameing the columns according to the readme file.
    #Student ID has to be added in here as well
    local_info <- as_tibble(test$local.info, .name_repair = "unique") %>% set_names(., nm = c("student_faculty", "gender", "major", "minor", "dorm", "year", "high_school")) %>%
      mutate(student_id = 1:n()) %>%
      select(student_id, everything())
    
    #convert back into a graph adding in the  local info data to be associated with each node
    test_g2 <- graph_from_data_frame(test_df, directed = FALSE, vertices = local_info)
    
    file_name <- file.path("/home/jonno/setse_1_data/facebook100/facebook100_igraph",
                           paste0(uni_name, ".rds"))
    saveRDS(test_g2, file_name)
    
  })


rm(facebook100_files)