
#create the facebook uni stats if they haven't already been created

if(file.exists(file.path(PLwd, "facebook_uni_stats.rds"))){
  
  uni_stats <- readRDS(file.path(PLwd, "facebook_uni_stats.rds"))
  
} else {
uni_stats <-1:length(uni_files) %>%
  map_df(~{
    
    g <- readRDS(uni_files[.x])
    #get the data on the uni's
    uni_name <- basename(uni_files[.x]) %>% str_remove(., ".rds") %>% str_remove(., "[0-9]+")
    data_id <- basename(uni_files[.x]) %>% str_remove(., ".rds") %>% str_remove(., "[ a-zA-Z]+") %>% str_remove(., " ")
    components_info <- components(g)
    
    print(uni_name)
    
    g_df <- as_data_frame(g, what = "vertices") %>% tibble
    
    dorms_df <- facebook_fraction(g, "dorm")
    year_df <- facebook_fraction(g, "year")
    
    tibble(file_number = .x, 
           file_name =  basename(uni_files[.x]) %>% str_remove(., ".rds"),
           uni_name = uni_name,
           data_id = data_id,
           nodes = vcount(g), edges = ecount(g), 
           max_component = max(components_info$csize),
           max_comp_perc = max_component/nodes,
           total_components = components_info$no,
           dorms = length(unique(g_df$dorm)),
            year_assort = assortativity_nominal(g, types = as.factor(vertex_attr(g, "year")), directed = F),
            gen_assort = assortativity_nominal(g, types = as.factor(vertex_attr(g, "gender")), directed = F),
            student_assort = assortativity_nominal(g, types = as.factor(vertex_attr(g, "student_faculty")), directed = F),
           dorm_assort = assortativity_nominal(g, types = as.factor(vertex_attr(g, "dorm")), directed = F),
           big_dorms = dorms_df %>% filter(classes != "Other") %>% nrow,
           big_dorm_fraction = dorms_df %>% filter(classes =="Other") %>% pull(percentage) %>%{1- sum(.)},
           mid_dorms = dorms_df %>% filter(classes2 != "Other") %>% nrow,
           mid_dorm_fraction = dorms_df %>% filter(classes2 =="Other") %>% pull(percentage) %>%{1- sum(.)},
           big_year_fraction = year_df %>% filter(classes =="Other") %>% pull(percentage) %>%{1- sum(.)},
           mid_year_fraction = year_df %>% filter(classes2 =="Other") %>% pull(percentage) %>%{1- sum(.)},
           density = ecount(g)/(vcount(g)*(vcount(g)-1)/2)
           # biconn_comps = biconnected.components(g)$no #this is extremley slow to calculate
    )
    
  }) %>%
  mutate(data_id = as.integer(data_id))

  saveRDS(uni_stats, file.path(PLwd, "facebook_uni_stats.rds"))

}