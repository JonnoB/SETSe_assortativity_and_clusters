
#Creates the PEELS quintet building blocks and prepares the data for the initial plot


sub_classes_vect <-c("d_1", "d_2", "c_1", "c_2")


class_data_df <- expand.grid(class = c("A", "B"), sub = 1:2) %>%
  as_tibble() %>%
  mutate(sub_class = paste(class, sub, sep = "_"),
         size = 10 #replace with a vector if group sizes uneevn
  ) #finds the start position along the axis of the matrix


#this sub chunk creates a df of the sub class combinations from the upper triangular block of the adjacency matrix
{
  network_classes <- matrix(FALSE, ncol = nrow(class_data_df), nrow = nrow(class_data_df)) 
  network_classes[upper.tri(network_classes, diag = TRUE)] <-TRUE
  network_classes <- network_classes %>% as_tibble %>%
    set_names(pull(class_data_df, sub_class)) %>%
    mutate(sub_class_1 = pull(class_data_df, sub_class)) %>%
    {.} %>%
    gather(key = sub_class_2, value = keep, -sub_class_1) %>%
    filter(keep) %>%
    select(-keep) %>%
    left_join(class_data_df %>% select(sub_class_1 = sub_class, class_1 = class))%>%
    left_join(class_data_df %>% select(sub_class_2 = sub_class, class_2 = class))
  }




#The quintet

quintet <-  network_classes %>%
  mutate(
    A = case_when(
      sub_class_1 == sub_class_2 ~ 10,
      TRUE ~20
    ),
    
    B = case_when(
      class_1 != class_2 ~20,
      (sub_class_1 == "A_1" & sub_class_2 == "A_1")| (sub_class_1 == "B_2" & sub_class_2 == "B_2") ~ 38,
      (sub_class_1 == "A_1" & sub_class_2 == "A_2")| (sub_class_1 == "B_1" & sub_class_2 == "B_2") ~ 2,
      TRUE ~0
    ),
    
    C = case_when(
      (sub_class_1 == "A_1" & sub_class_2 == "A_1")~38,
      (sub_class_1 == "A_1" & sub_class_2 == "A_2")~2,
      (sub_class_1 == "A_2" & sub_class_2 == "B_2") ~80,
      (sub_class_1 == "B_1" & sub_class_2 == "B_2")~20,
      (class_1 == "B" & class_2 == "B")~10,
      TRUE ~0
    ),
    
    D = case_when(
      sub_class_1 == sub_class_2 ~ 10,
      (sub_class_1 == "A_1" & sub_class_2 == "A_2")| (sub_class_1 == "B_1" & sub_class_2 == "B_2") ~ 20,
      (sub_class_1 == "A_2" & sub_class_2 == "B_2") ~80,
      TRUE ~0
    ),
    
    
    E = case_when(
      (sub_class_1 == "A_1" & sub_class_2 == "A_1")| (sub_class_1 == "B_2" & sub_class_2 == "B_2") ~ 38,
      (sub_class_1 == "A_1" & sub_class_2 == "A_2")| (sub_class_1 == "B_1" & sub_class_2 == "B_2") ~ 2,
      (sub_class_1 == "B_1" & sub_class_2 == "A_2") ~ 80,
      TRUE ~0
    )) 

#set.seed so graphs are repeatable
set.seed(1235)
quintet_g_list <- LETTERS[1:5] %>%
  map(~create_assortivity_graph_from_subgroups(class_data_df, quintet %>% rename(edges = .x), 10) %>%
        set.edge.attribute(., "type", value = .x) %>%
        set.graph.attribute(., "type", value = .x)
      
  )
#check there is only a single component
quintet_g_list %>%
  map_dbl(~components(.x)$no)

#Checks the assortativity. They are all zero
quintet_g_list %>%
  map_dbl(~assortativity_nominal(.x, 
                                 types = as.factor(get.vertex.attribute(.x, "class")), 
                                 directed = FALSE))

plot_list <- 1:5 %>%
  map(~{ggraph(quintet_g_list[[.x]]) +
      geom_edge_fan()+
      geom_node_point(aes(fill = class, shape = grepl("1", sub_class)), size=3) +
      scale_shape_manual(values=c(21, 24)) +
      guides(fill = "none", shape = "none") +
      labs(title = paste("Type", LETTERS[.x]))})
