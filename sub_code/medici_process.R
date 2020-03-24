#The medici code

#This is a bit of a Frankenstien that allows the medici analysis to be run in relateivley intelligable chunks

#The data is loaded several times, becuase I can't be bothered to finness the process

g_dl <- load_dl_graph(files[5], directed = FALSE)

#all names
medici_vertex <- bind_rows(as_data_frame(g_dl[[1]], what = "vertices"),
                           as_data_frame(g_dl[[2]], what = "vertices")) %>%
  distinct()

#add in the partisan data for the medici dataset, also add in the force of the network
medici_data <- read_lines(files[6])
medici_partisan <- medici_data[26:41] %>%
  str_squish() %>%
  tibble(name = medici_data[5:20], V1 = .) %>%
  separate(., col = V1, into =c("wealth", "priors", "ties"),  sep  = " ", convert = TRUE) %>%
  # full_join(medici_vertex, .) %>%
  mutate(party = c(
    "medici",
    "split",
    "medici",
    "oligarch",
    "split",
    "oligarch",
    "oligarch",
    "oligarch",
    "oligarch",
    "medici",
    "medici",
    "oligarch",
    "split",
    "medici",
    "medici",
    "neutral"
    
  )
  ) 

g_medici <- bind_rows(
  as_data_frame(g_dl[[1]]) %>% distinct %>% mutate(type = "marriage",
                                                   weight = 1),
  as_data_frame(g_dl[[2]]) %>% distinct  %>% mutate(type = "Business",
                                                    weight = 1)) %>%
  group_by(from, to) %>%
  summarise(edge_weight = sum(weight))  %>%
  graph_from_data_frame(., directed = FALSE, vertices = medici_partisan) 


medici_strozzi <- medici_function("MEDICI", "STROZZI")

medici_albizzi <- medici_function("MEDICI", "ALBIZZI")

AlbizziandStrozzi <-medici_albizzi$summary %>%
  mutate(name = case_when(
    name == "clustering_elev" ~"Elev vs Albizzi",
    TRUE ~ name
  )) %>%
  filter(name == "Elev vs Albizzi") %>%
  bind_rows(.,
            medici_strozzi$summary %>%
              mutate(name = case_when(
                name == "clustering_elev" ~"Elev vs Strozzi",
                TRUE ~ name
              ))
  )  


rm(g_dl); rm(medici_strozzi);rm(medici_albizzi);rm(medici_data); rm(medici_partisan)