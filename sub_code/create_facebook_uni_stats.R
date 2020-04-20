
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