
###########################
###########################
# # 
# # MYRIAD RUN SCRIPT
# # 
### This script is run after the HPC has processed the data. Doing this online reduces the amount of data that needs
### to be downloaded greatly
# # 
# # 
###########################
###########################

packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "magrittr",
              "igraph", "devtools", "minpack.lm", "readr", "stringr",
              "Matrix")


new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(packages, library, character.only = TRUE)

#source_path <- "/home/jonno/setse_1_data/facebook_embeddings/facebook"
#target_path <- "/home/jonno/setse_1_data/facebook_embeddings/test"
#Set up file system to read the correct folders this switches between aws and windows mode

#creates the correct root depending on whether this is on the cloud or not

  #This is for the folder that is on the cloud
  project_folder <- getwd()
  
  basewd <- "/home/ucabbou"
  #on the home dir not in the project folder like when it is done on my own comp
  load_data_files_path <- "/home/ucabbou/Scratch/facebook" #load the files
  save_data_files_path <- file.path(project_folder) #save the files

  print("load support functions")
  list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
  list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))

  print("start processing data")
  
  print(list.files(load_data_files_path, pattern = ".rds"))
  # process_facebook_embeddings(load_data_files_path, 
  #                             file.path(project_folder , "processed_embeddings"))
  target_path <- file.path(project_folder , "processed_embeddings")
  source_path <- load_data_files_path
  
  if(!dir.exists(target_path)){dir.create(target_path)}
  
    file_paths <- list.files(source_path, full.names = T, pattern = ".rds")
    
    facebook_embeddings_data <- 1:length(file_paths) %>%
      map(~{
        
        print(.x)
        file_name <- basename(file_paths)[.x]
        readRDS(file_paths[.x]) %>%
          flatten() %>%
          map(~{
            
            Out <-  .x 
            Out$file_name <- str_remove(file_name, ".rds")
            
            return(Out)
          })
        
      }) %>% transpose() 
    
    
    embeddings_names <-names(facebook_embeddings_data)
    
    embeddings_names %>%
      walk(~{
        facebook_embeddings_data[[.x]] %>%
          bind_rows() %>%
          saveRDS(., file.path(target_path, paste0("facebook_", .x, ".rds")))
        
      })
    
  print("finish processing data")



