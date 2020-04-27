# model the basic peels quintet


#create the dataset to test the nearest neighbours
modelling_peels_df <- peel_strain_sum %>% ungroup %>%
  select(mean_tension_per_node, mean_abs_elevation, graph_type) 

#create the formula to use
mod_form <- as.formula(graph_type ~ mean_tension_per_node + mean_abs_elevation)

#create the ten sets of repeat folds
#make a large number of repeats to be able to get distribution
set.seed(4622)
rs_obj <- vfold_cv(modelling_peels_df, v = 10, repeats = 100)#, strata = Network)

#the function that will return the accuracy of each fold
knn_holdout_results <- function(splits, k, ...) {
  train_df = analysis(splits)
  
  # Save the 10%
  holdout <- assessment(splits)
  # Fit the model to the 90%
  mod <- class::knn(train = train_df %>% select(-graph_type), 
                    cl = train_df$graph_type, 
                    test = holdout%>% select(-graph_type), k = k)
  
  # `augment` will save the predictions with the holdout data set
  res <- holdout %>% mutate(pred = factor(mod, levels = c("A", "B", "C", "D", "E")),
                            graph_type = factor(graph_type, levels = c("A", "B", "C", "D", "E")))
  
  #Output the accuracy of the model
  accuracy(res, graph_type, pred)
}

#This function will return the accuracy of the logistic regression
# log_holdout_results <- function(splits, ...) {
#   train_df = analysis(splits)
#   
#   # Save the 10%
#   holdout <- assessment(splits)
#   # Fit the model to the 90%
#   mod <- vglm(Network ~., family=multinomial, data = train_df)
#   
#   # `augment` will save the predictions with the holdout data set
#   res <- predict(mod, holdout, type="response") %>% apply(., 1, which.max)
#   
#   res <- holdout %>%
#     mutate(pred =  predict(mod, holdout, type="response") %>% 
#              apply(., 1, which.max)%>% LETTERS[.] %>% factor(., levels = c("A", "B", "C", "D", "E")),
#            Network = factor(Network, levels = c("A", "B", "C", "D", "E")))
#   
#   #Output the accuracy of the model
#   accuracy(res, Network, pred)
# }

nearest_k_results <-1:10 %>% map_df(~{
  test2 <- map2_df(.x = rs_obj$splits, 
                   .y = .x, 
                   .f =  ~knn_holdout_results(.x, .y,  mod_form))
  
  tibble(k = .x, accuracy =mean(test2$.estimate))
  
})


# log_reg_results <-rs_obj$splits %>% map_df(~{
#   test2 <- log_holdout_results(.x,  mod_form)
#   
#   tibble(accuracy =mean(test2$.estimate))
#   
# })
# 
# #aggregate by the number of repeats
# boot_res <- log_reg_results %>%
#   mutate(v = rep(1:100, each = 10)) %>%
#   group_by(v) %>%
#   summarise(accuracy = mean(accuracy))