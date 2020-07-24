
###########################
###########################
# # 
# # MYRIAD RUN SCRIPT
# # 
### This script embedds the origin component only for network UIllinois20. This is becuase I am having
###Issues converging a few large networks, It appears there are sudden divergences which I don't
###Understand at all. It also looks like smaller tiemsteps resolve the issue. Which does make sense.
### But I don't know how you are supposed to know what timestep to set.
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
  library(rSETSe)
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
  task_id <- as.numeric(Sys.getenv("SGE_TASK_ID"))

  list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
  list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
}



params <- expand.grid(tstep = c(0.001, 0.005, 0.01), drag = c(0.1, 0.5, 1, 2, 4, 10))


print(paste0("Task id =",task_id, " system params: tstep=", params$tstep[task_id], ", drag=", params$drag[task_id]))

file_name <- file.path(save_data_files_path,
                       paste0("Uillinois20_origin_task_id_",task_id, ".rds"))

  start_time <- Sys.time()

  g <- readRDS(file.path(load_data_files_path, "Uillinois20_origin.rds"))  
  
  
  embeddings_data <- SETSe(g = g,
                           tstep = params$tstep[task_id],
                           max_iter = 60000,
                           coef_drag = params$drag[task_id],
                           tol = sum(abs(vertex_attr(g, "force")))/1000,
                           sparse = TRUE,
                           sample = 100,
                           static_limit = sum(abs(vertex_attr(g, "force"))))
  
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
    mutate(tstep = params$tstep[task_id],
           drag = params$drag[task_id])
  
  stop_time <- Sys.time()
  time_diff <- tibble(tstep = params$tstep[task_id],
                      drag = params$drag[task_id], start_time = start_time, stop_time = stop_time)
  
  results <-list(embeddings_data = embeddings_data,
                 node_detail = node_detail,
                 time_taken_process = time_diff)
  
  saveRDS(results, file = file_name )
