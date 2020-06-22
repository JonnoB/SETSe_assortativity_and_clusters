
###########################
###########################
# # 
# # MYRIAD RUN SCRIPT
# # 
### This script creates the embeddings for the facebook 100 dataset. It only uses auto_setse not setse_bicomp. it is for reference only
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


#Set up file system to read the correct folders this switches between aws and windows mode

#creates the correct root depending on whether this is on the cloud or not
if(dir.exists("/home/jonno")){
  #This folder is for use on my machine
  project_folder <- "/home/jonno/setse_1_data"
  facebook_graphs <- file.path(project_folder,"facebook100/facebook100_igraph")
  basewd <- "/home/jonno"
  load_data_files_path <- file.path(project_folder) #load the files
  save_data_files_path <- file.path("/home/jonno/setse_1_data/facebook_embeddings/facebook_year") #save the files
  library(NetworkSpringEmbedding)
  list.files("/home/jonno/Useful_PhD__R_Functions", pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
}else{
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  
  basewd <- "/home/ucabbou"
  #on the home dir not in the project folder like when it is done on my own comp
  load_data_files_path <- file.path(basewd,"facebook_data") #load the files
  save_data_files_path <- file.path(project_folder) #save the files
  facebook_graphs <- file.path(load_data_files_path, "facebook100_igraph")
  
  #If it is not on my computer then the variables need to be loaded from the system environment
  #Get the task ID
  task_id <- Sys.getenv("SGE_TASK_ID")

  list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
  list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
}





print("load uni stats dataframe")
uni_stats <- readRDS(file.path(load_data_files_path, "facebook_uni_stats.rds"))

uni_name <- uni_stats %>%
  dplyr::arrange(nodes) %>%
  dplyr::pull(file_name)

print(task_id)
#print(task_id==1)
print(class(task_id))
uni_name <- uni_name[as.numeric(task_id)]

print(uni_name)

print(paste("task id", task_id, uni_name))

uni_file_path <-  list.files(facebook_graphs, pattern = uni_name, full.names = T)

print(paste("load uni graph", uni_name, uni_file_path))
file_name <- file.path(save_data_files_path,
                       paste0(uni_name, ".rds"))

  start_time <- Sys.time()

  g <- readRDS(uni_file_path)  %>% #load file
    remove_small_components()  %>%
    facebook_year_clean() %>% 
    prepare_SETSe_continuous(., node_names = "name", k = 1000, force_var = "year", 
                                                       sum_to_one = FALSE, 
                                                       distance = 100) 
  
  embeddings_data <- auto_SETSe(g, 
                                tstep = 0.01,
                                mass = 1,#sum(abs(vertex_attr(g, "force"))/2)/vcount(g), variable mass is useful when force is constant
                                tol = sum(abs(vertex_attr(g, "force")))/1000,
                                verbose = FALSE,
                                sparse = TRUE, 
                                sample = 200,
                                static_limit = sum(abs(vertex_attr(g, "force"))), #static_force is more than the starting force. just stop.
                                hyper_tol = 0.1,
                                max_iter = 60000,
                                hyper_max = 6000) 
  
  node_detail <- embeddings_data$edge_embeddings %>% tibble() %>%
    separate(., col = edge_name, into = c("from","to"), sep = "-") %>%
    select(from, to, tension) %>%
    pivot_longer(cols = c(from, to), names_to = "node_type", values_to = "node") %>%
    select(tension, node) %>%
    group_by(node) %>%
    summarise(sum_tension = sum(tension),
              mean_tension = mean(tension),
              median_tension = median(tension),
              euc_tension = sqrt(sum(tension^2))) %>%
    left_join(embeddings_data$node_embeddings %>% tibble(), by = "node") %>%
    left_join(as_data_frame(g, what = "vertices"), by = c("node"="name")) %>%
    mutate(uni = uni_name)
  
  stop_time <- Sys.time()
  time_diff <- tibble(uni  = uni_name, start_time = start_time, stop_time = stop_time)
  
  results <-list(embeddings_data = embeddings_data,
                 node_detail = node_detail,
                 time_taken_process = time_diff)
  
  saveRDS(results, file = file_name )
