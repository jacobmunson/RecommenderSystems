###########################

## Relevant Packages
library(dplyr)
library(readr)
library(reshape2)
library(doParallel)
library(ggplot2)

setwd("~/Documents")

Rcpp::sourceCpp('GitHub/RecommenderSystems/Handling Large Data/Rcpp/neighborhood_based_evaluation_helper_files.cpp')
source('GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

D = read_csv("Recommender Systems - Home Folder/ml-latest-small/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")

D = as_tibble(D)
message("Splitting data...")
dataset = D
source('GitHub/RecommenderSystems/Handling Large Data/general_cross_validation.R')

D_train = D1_train # D[train_index,]
D_test = D1_test # D[-train_index,]
nrow(D_test);nrow(D_train)


# Matrix for storing similarity
# stored_sim_matrix = expand.grid(user1 = unique(D$user), 
#                                 user2 = unique(D$user), 
#                                 sim_cs = NA, 
#                                 sim_pc = NA, 
#                                 sim_lu = NA, 
#                                 sim_lg = NA)
#stored_sim_matrix = as_tibble(stored_sim_matrix)

## Setting k for kNN 
K = c(20)
eps = 0.001


# Prediction dataframe to fill up
# pred_df_total = expand.grid(test_user = unique(D_test$user), 
#                             item = unique(D$item), 
#                             k = K, 
#                             #k_actual = NA, 
#                             pred_cs_weighted = NA, 
#                             pred_cs_unweighted = NA, 
#                             pred_pc_weighted = NA,
#                             pred_pc_unweighted = NA,
#                             pred_lu_weighted = NA, 
#                             pred_lu_unweighted = NA, 
#                             pred_lg_weighted = NA,
#                             pred_lg_unweighted = NA) %>% arrange(test_user, item)

items_per_user = 1000 # need to put a line in this function such that an actual rated item (high rating) is included in the user's set of TopN
pred_df_total = data.frame()
user_interest_threshold = 5
for(i in 1:length(unique(D_test$user))){
  
  user_i = unique(D_test$user)[i]
  user_i_test_values = D_test %>% filter(user == user_i) %>% filter(rating >= user_interest_threshold)
  
  test_val_user_i = user_i_test_values[sample(x = 1:nrow(user_i_test_values), size = 1),]
  
  items_for_user_i = c(test_val_user_i$item, sample(unique(D$item), size = items_per_user, replace = F))
  
  
  df_item = expand.grid(test_user = user_i, 
                        item = items_for_user_i, 
                        k = K)
  pred_df_total = bind_rows(pred_df_total, df_item)
  print(i)
}

# pred_df_total %>% group_by(test_user, k) %>% tally() %>% ungroup() %>% select(n) %>% unique()
pred_df_total = pred_df_total %>% mutate(pred_cs_weighted = NA, 
                                         pred_cs_unweighted = NA, 
                                         pred_pc_weighted = NA,
                                         pred_pc_unweighted = NA,
                                         pred_lu_weighted = NA, 
                                         pred_lu_unweighted = NA, 
                                         pred_lg_weighted = NA,
                                         pred_lg_unweighted = NA) %>% arrange(test_user, item)

dim(pred_df_total)
head(pred_df_total)


## Build LiRa Distributions
source('GitHub/RecommenderSystems/build_lira_distributions.R')
## Chunk test set
shard_multiplier = 20
source('GitHub/RecommenderSystems/chunk_test_set_users.R')

## LiRa LRT Variance Estimation
#sd_pop = lira_lrt_sd_sampling(dataset = D_train, iter = 1500)

## LiRa Gaussian - Pure Chance Standard Deviation Parameter
sd_pc = 4

use_stored_similarity = F
rm(sim_matrix);gc() # 
start = Sys.time()

num_cores = detectCores();
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
cat(format(Sys.time(), "%a %b %d %X %Y"), "\n")

sim_matrix = foreach(s = 1:2, .combine = rbind, 
                     .packages = c("dplyr","reshape2","Rcpp"),
                     .noexport = c("top_n", "lira_loop", "lira_gaussian_loop", 
                                   "correlationCoefficient", "cosine_vector_similarity")) %dopar% {
                                  
                                     source('GitHub/RecommenderSystems/recommender_systems_helper_functions.R')
                                     Rcpp::sourceCpp('GitHub/RecommenderSystems/Handling Large Data/Rcpp/neighborhood_based_evaluation_helper_files.cpp')
                                     shard_s = shards[[s]]
                                     # prediction_data_frame = c()
                                     print(s)
                                     for(i in 1:length(shard_s)){
                                       user_i = shard_s[i] #unique(D_test$user)[i]
                                       # user_i = 1
                                       # user_i_train = D_train %>% filter(user == user_i)
                                       
                                       pred_df_total_user_i = pred_df_total %>% filter(test_user == user_i)
                                       num_items = length(unique(pred_df_total_user_i$item))
                                       # j = 1
                                       
                                       
                                       start = Sys.time()
                                       
                                       for(j in 1:num_items){
                                         
                                         
                                         # pred_df_total_user_i %>% filter(item == unique(pred_df_total_user_i$item)[j])
                                         
                                         # D_test_i = pred_df_total_user_i[j,c("test_user","item")]; colnames(D_test_i) = c("user","item")
                                         
                                         
                                         user_i_item_j = unique(pred_df_total_user_i$item)[j] #pred_df_total_user_i[j,"item"]
                                         
                                         D_test_i = cbind(user = user_i, item = unique(pred_df_total_user_i$item)[j])
                                         
                                         train_set = D_train
                                         test_set = as.data.frame(D_test_i)
                                         
                                         # get training values for item j, excluding user i
                                         D_train_user_i_item_j = D_train %>% filter(item == user_i_item_j, user != user_i)
                                         # grab the users
                                         potential_coraters_user_i_item_j = D_train_user_i_item_j %>% select(user) %>% .$user
                                         
                                         
                                         # between test user and potential coraters - check if their similarity is stored in stored_sim_matrix
                                         
                                         
                                         if(use_stored_similarity){
                                           stored_sims_already_computed = stored_sim_matrix %>% 
                                             filter(user1 %in% c(user_i,potential_coraters_user_i_item_j)| 
                                                      user2 %in% c(user_i,potential_coraters_user_i_item_j)) %>% 
                                             select(user1, user2, sim_lu) %>% 
                                             na.omit()
                                         }
                                         
                                         # D_test_i[1,"user"]
                                         # potential_coraters_user_i_item_j
                                         
                                         
                                         
                                         # check if there are potential coraters 
                                         # this can be a spot to put a fallback predictor
                                         if(length(potential_coraters_user_i_item_j) > 0){
                                           
                                           
                                           # the set of coraters that don't have a similarity stored
                                           if(use_stored_similarity){
                                             potential_coraters_user_i_item_j = setdiff(potential_coraters_user_i_item_j, 
                                                                                        unique(c(stored_sims_already_computed$user1, stored_sims_already_computed$user2)))
                                           }

                                           # get data for coraters and original user
                                           potential_coraters_user_i_item_j_train_obs = D_train %>% filter(D_train$user %in% c(user_i, potential_coraters_user_i_item_j))
                                           # reshape data to be ratings matrix format
                                           potential_coraters_user_i_item_j_train_obs_reshaped = dcast(data = potential_coraters_user_i_item_j_train_obs, formula = user ~ item, value.var = "rating")
                                           
                                           # dim(potential_coraters_user_i_item_j_train_obs_reshaped)
                                           #potential_coraters_user_i_item_j_train_obs_reshaped
                                           
                                           rownames(potential_coraters_user_i_item_j_train_obs_reshaped) = potential_coraters_user_i_item_j_train_obs_reshaped$user
                                           potential_coraters_user_i_item_j_train_obs_reshaped = potential_coraters_user_i_item_j_train_obs_reshaped[,-1]
                                           
                                           # making sure row1 of matrix is the test user
                                           test_user_row = which(rownames(potential_coraters_user_i_item_j_train_obs_reshaped) == as.character(user_i))
                                           B_temp = potential_coraters_user_i_item_j_train_obs_reshaped[test_user_row,]
                                           
                                           old_name = rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[1]
                                           
                                           potential_coraters_user_i_item_j_train_obs_reshaped[test_user_row,] = potential_coraters_user_i_item_j_train_obs_reshaped[1,]
                                           potential_coraters_user_i_item_j_train_obs_reshaped[1,] = B_temp
                                           
                                           rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[1] = as.character("empty_first_row")
                                           rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[test_user_row] = as.character("empty_last_row")
                                           rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[1] = as.character(user_i)
                                           rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[test_user_row] = as.character(old_name)
                                           
                                           
                                           # forcing into matrix format for manipulation purposes
                                           potential_coraters_user_i_item_j_train_obs_reshaped = as.matrix(potential_coraters_user_i_item_j_train_obs_reshaped)
                                           
                                           # the places where a prediction is needed
                                           k_error_df_lirau = pred_df_total_user_i %>% filter(item == user_i_item_j) #data.frame(user = user_i, item = user_i_item_j, K, k_current = NA, sim = "lira_uniform", weighted_pred = NA, unweighted_pred = NA)
                                           
                                           # after reshaping, is the matrix more than just the original user - is it possible that it's just the one user?
                                           if(nrow(potential_coraters_user_i_item_j_train_obs_reshaped) > 0){
                                             
                                             
                                             
                                             similarity_vector = compute_neighbor_similarity(user_item_matrix = potential_coraters_user_i_item_j_train_obs_reshaped,
                                                                                             test_observation = test_set,
                                                                                             similarity_measure = "lira_uniform")
                                             
                                             similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                             #similarity_vector = similarity_vector[-1]
                                             
                                             
                                             
                                             if(use_stored_similarity){
                                               stored_sims_already_computed = stored_sims_already_computed %>% 
                                                 select(user = user1, y = sim_lu) %>% filter(user != user_i)
                                               
                                               for(s in 1:length(similarity_vector)){
                                                 
                                                 stored_sim_matrix[which(((stored_sim_matrix$user1 == user_i & 
                                                                             stored_sim_matrix$user2 == names(similarity_vector)[s]) | 
                                                                            (stored_sim_matrix$user2 == user_i & 
                                                                               stored_sim_matrix$user1 == names(similarity_vector)[s]))),"sim_lu"] = 
                                                   similarity_vector[s]
                                               }
                                             }
                                             
                                             # storing newly computed similarities
                                             
                                             
                                             # getting ratings and similarities together for newly computed neighbors
                                             
                                             
                                             neighbor_ratings_fresh = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                             neighbor_ratings_fresh = merge(neighbor_ratings_fresh[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                             
                                             
                                             
                                             # getting ratings and similarities together for stored neighbors
                                             
                                             if(use_stored_similarity){
                                               neighbor_ratings_precomputed = train_set[which(train_set$item == test_set$item & train_set$user %in% stored_sims_already_computed$user),]
                                               neighbor_ratings_precomputed = merge(neighbor_ratings_precomputed[c("user","rating")], stored_sims_already_computed, by.x = "user", by.y = "user")
                                               
                                               # putting the above together
                                               neighbor_ratings = bind_rows(neighbor_ratings_fresh, neighbor_ratings_precomputed) %>% arrange(desc(y))
                                             }else{neighbor_ratings = neighbor_ratings_fresh}
                                             
                                             
                                             
                                              
                                             neighbor_ratings = neighbor_ratings_fresh 
                                             
                                             neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
                                             
                                             if(any(neighbor_ratings$user == test_set$user)){
                                               neighbor_ratings = neighbor_ratings[-which(neighbor_ratings$user == test_set$user),]
                                             }
                                             
                                             
                                             # columns: k, k_current, ae_nn, ae_knn
                                             # dimensions: K * 4
                                             for(k in 1:length(K)){
                                               
                                               k_current = min(length(similarity_vector), K[k])
                                               k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                               
                                               prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                               
                                               sim = prediction_neighbors$rating #y
                                               
                                               # if(any(prediction_neighbors$y <= 0)){
                                               #   prediction_neighbors = prediction_neighbors %>% 
                                               #     mutate(temp_y = y + abs(min(y)) + eps, 
                                               #            weight_y = temp_y/max(temp_y))
                                               # }else{prediction_neighbors = prediction_neighbors %>% 
                                               #   mutate(weight_y = y/max(y))}
                                               
                                               
                                               k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                               
                                               # pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                               
                                               pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
                                               k_error_df_lirau[k,"pred_lu_weighted"] = pred_rating_weighted # - test_set$rating)
                                               
                                               pred_rating_unweighted = mean(prediction_neighbors$rating)
                                               k_error_df_lirau[k,"pred_lu_unweighted"] = pred_rating_unweighted # - test_set$rating)
                                               
                                             }
                                           }
                                           
                                           pred_df_total_user_i[which(pred_df_total_user_i$test_user == user_i &
                                                                        pred_df_total_user_i$item == user_i_item_j),
                                                         c("pred_lu_weighted","pred_lu_unweighted")] = k_error_df_lirau[,c("pred_lu_weighted","pred_lu_unweighted")]
                                           
                                           # prediction_data_frame = bind_rows(prediction_data_frame, k_error_df_lirau) 
                                           
                                           
                                           # start
                                           
                                           if(nrow(potential_coraters_user_i_item_j_train_obs_reshaped) > 0){
                                             
                                             
                                             
                                             similarity_vector = compute_neighbor_similarity(user_item_matrix = potential_coraters_user_i_item_j_train_obs_reshaped,
                                                                                             test_observation = test_set,
                                                                                             similarity_measure = "lira_gaussian_pure_chance")
                                             
                                             similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                             #similarity_vector = similarity_vector[-1]
                                             
                                             
                                             if(use_stored_similarity){
                                               stored_sims_already_computed = stored_sims_already_computed %>% 
                                                 select(user = user1, y = sim_lu) %>% filter(user != user_i)
                                               
                                               for(s in 1:length(similarity_vector)){
                                                 
                                                 stored_sim_matrix[which(((stored_sim_matrix$user1 == user_i & 
                                                                             stored_sim_matrix$user2 == names(similarity_vector)[s]) | 
                                                                            (stored_sim_matrix$user2 == user_i & 
                                                                               stored_sim_matrix$user1 == names(similarity_vector)[s]))),"sim_lu"] = 
                                                   similarity_vector[s]
                                               }
                                             }
                                             
                                             # storing newly computed similarities
                                             
                                             
                                             # getting ratings and similarities together for newly computed neighbors
                                             neighbor_ratings_fresh = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                             neighbor_ratings_fresh = merge(neighbor_ratings_fresh[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                             
                                             
                                             # getting ratings and similarities together for stored neighbors
                                             
                                             if(use_stored_similarity){
                                               neighbor_ratings_precomputed = train_set[which(train_set$item == test_set$item & train_set$user %in% stored_sims_already_computed$user),]
                                               neighbor_ratings_precomputed = merge(neighbor_ratings_precomputed[c("user","rating")], stored_sims_already_computed, by.x = "user", by.y = "user")
                                               
                                               # putting the above together
                                               neighbor_ratings = bind_rows(neighbor_ratings_fresh, neighbor_ratings_precomputed) %>% arrange(desc(y))
                                             }else{neighbor_ratings = neighbor_ratings_fresh}
                                             
                                             
                                             #neighbor_ratings = neighbor_ratings[-1,] 
                                             neighbor_ratings = neighbor_ratings_fresh 
                                             
                                             neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
                                             
                                             if(any(neighbor_ratings$user == test_set$user)){
                                               neighbor_ratings = neighbor_ratings[-which(neighbor_ratings$user == test_set$user),]
                                             }
                                             
                                             # columns: k, k_current, ae_nn, ae_knn
                                             # dimensions: K * 4
                                             for(k in 1:length(K)){
                                               
                                               k_current = min(length(similarity_vector), K[k])
                                               k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                               
                                               prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                               
                                               sim = prediction_neighbors$rating #y
                                               
                                               # if(any(prediction_neighbors$y <= 0)){
                                               #   prediction_neighbors = prediction_neighbors %>% 
                                               #     mutate(temp_y = y + abs(min(y)) + eps, 
                                               #            weight_y = temp_y/max(temp_y))
                                               # }else{prediction_neighbors = prediction_neighbors %>% 
                                               #   mutate(weight_y = y/max(y))}
                                               
                                               
                                               k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                               
                                               # pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                               
                                               pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
                                               k_error_df_lirau[k,"pred_lg_weighted"] = pred_rating_weighted # - test_set$rating)
                                              
                                               pred_rating_unweighted = mean(prediction_neighbors$rating)
                                               k_error_df_lirau[k,"pred_lg_unweighted"] = pred_rating_unweighted # - test_set$rating)
                                               
                                             }
                                           }
                                           
                                           pred_df_total_user_i[which(pred_df_total_user_i$test_user == user_i &
                                                                        pred_df_total_user_i$item == user_i_item_j),
                                                         c("pred_lg_weighted","pred_lg_unweighted")] = k_error_df_lirau[,c("pred_lg_weighted","pred_lg_unweighted")]
                                           
                                           # prediction_data_frame = bind_rows(prediction_data_frame, k_error_df_lirau)
                                           
                                           ### end
                                           
                                           
                                           if(nrow(potential_coraters_user_i_item_j_train_obs_reshaped) > 0){
                                             
                                             
                                             
                                             similarity_vector = compute_neighbor_similarity(user_item_matrix = potential_coraters_user_i_item_j_train_obs_reshaped,
                                                                                             test_observation = test_set,
                                                                                             similarity_measure = "pearson_pwc")
                                             
                                             similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                             #similarity_vector = similarity_vector[-1]
                                             
                                             
                                             if(use_stored_similarity){
                                               stored_sims_already_computed = stored_sims_already_computed %>% 
                                                 select(user = user1, y = sim_lu) %>% filter(user != user_i)
                                               
                                               for(s in 1:length(similarity_vector)){
                                                 
                                                 stored_sim_matrix[which(((stored_sim_matrix$user1 == user_i & 
                                                                             stored_sim_matrix$user2 == names(similarity_vector)[s]) | 
                                                                            (stored_sim_matrix$user2 == user_i & 
                                                                               stored_sim_matrix$user1 == names(similarity_vector)[s]))),"sim_lu"] = 
                                                   similarity_vector[s]
                                               }
                                             }
                                             
                                             # storing newly computed similarities
                                             
                                             
                                             # getting ratings and similarities together for newly computed neighbors
                                             neighbor_ratings_fresh = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                             neighbor_ratings_fresh = merge(neighbor_ratings_fresh[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                             
                                             
                                             # getting ratings and similarities together for stored neighbors
                                             
                                             if(use_stored_similarity){
                                               neighbor_ratings_precomputed = train_set[which(train_set$item == test_set$item & train_set$user %in% stored_sims_already_computed$user),]
                                               neighbor_ratings_precomputed = merge(neighbor_ratings_precomputed[c("user","rating")], stored_sims_already_computed, by.x = "user", by.y = "user")
                                               
                                               # putting the above together
                                               neighbor_ratings = bind_rows(neighbor_ratings_fresh, neighbor_ratings_precomputed) %>% arrange(desc(y))
                                             }else{neighbor_ratings = neighbor_ratings_fresh}
                                             
                                             
                                             #neighbor_ratings = neighbor_ratings[-1,] 
                                             neighbor_ratings = neighbor_ratings_fresh 
                                             
                                             if(any(neighbor_ratings$user == test_set$user)){
                                               neighbor_ratings = neighbor_ratings[-which(neighbor_ratings$user == test_set$user),]
                                             }
                                             
                                             # columns: k, k_current, ae_nn, ae_knn
                                             # dimensions: K * 4
                                             for(k in 1:length(K)){
                                               
                                               k_current = min(length(similarity_vector), K[k])
                                               k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                               
                                               prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                               
                                               sim = prediction_neighbors$rating #y
                                               
                                               # if(any(prediction_neighbors$y <= 0)){
                                               #   prediction_neighbors = prediction_neighbors %>% 
                                               #     mutate(temp_y = y + abs(min(y)) + eps, 
                                               #            weight_y = temp_y/max(temp_y))
                                               # }else{prediction_neighbors = prediction_neighbors %>% 
                                               #   mutate(weight_y = y/max(y))}
                                               
                                               
                                               k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                               
                                               # pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                               
                                               # should there be an absolute value up top?
                                               pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
                                               k_error_df_lirau[k,"pred_pc_weighted"] = pred_rating_weighted # - test_set$rating)
                                               
                                               pred_rating_unweighted = mean(prediction_neighbors$rating)
                                               k_error_df_lirau[k,"pred_pc_unweighted"] = pred_rating_unweighted # - test_set$rating)
                                               
                                             }
                                           }
                                           
                                           pred_df_total_user_i[which(pred_df_total_user_i$test_user == user_i &
                                                                        pred_df_total_user_i$item == user_i_item_j),
                                                         c("pred_pc_weighted","pred_pc_unweighted")] = k_error_df_lirau[,c("pred_pc_weighted","pred_pc_unweighted")]
                                           
                                           # prediction_data_frame = bind_rows(prediction_data_frame, k_error_df_lirau)
                                           ### end cosine
                                           
                                           
                                           
                                           
                                           
                                         }
                                         
                                         
                                         
                                         # cat("User:", user_i, "/", num_users, 
                                         #     ", Item: ", user_i_item_j,"|" , 
                                         #     j, "/", num_items ,
                                         #     "->", round(j/num_items *100,2),"% finished on user", 
                                         #     "| Percent users: ", round(i/num_users*100,2), "%", "\n")
                                         
                                         
                                       }
                                       
                                       end = Sys.time()
                                       print(end - start)
                                       end - start
                                     }
                                     
                                     
                                     pred_df_total_user_i # prediction_data_frame 
                                     
                                     
                                   }

stopCluster(cl);# dim(M)
end = Sys.time()
end - start # 1.02  

discordr::send_message(message = paste0("Mystery Machine done, took ", 
                                        round(end - start,2)," ",
                                        units(end - start), " "))




dim(sim_matrix)
head(sim_matrix)

sim_matrix %>% select(test_user, item) %>% group_by(test_user) %>% add_tally()







###################






use_stored_similarity = F

i = 1
num_users = 150 #length(unique(D_test$user))

user_times = rep(NA, num_users)
for(i in 1:num_users){
  
  user_i = unique(D_test$user)[i]
  
  
  # user_i = 1
  # user_i_train = D_train %>% filter(user == user_i)
  
  pred_df_total_user_i = pred_df_total %>% filter(test_user == user_i)
  num_items = length(unique(pred_df_total_user_i$item))
  # j = 1
  
  
  start = Sys.time()
  
  for(j in 1:num_items ){
    
    
    # pred_df_total_user_i %>% filter(item == unique(pred_df_total_user_i$item)[j])
    
    # D_test_i = pred_df_total_user_i[j,c("test_user","item")]; colnames(D_test_i) = c("user","item")
    
    
    user_i_item_j = unique(pred_df_total_user_i$item)[j] #pred_df_total_user_i[j,"item"]
    
    D_test_i = cbind(user = user_i, item = unique(pred_df_total_user_i$item)[j])
    
    train_set = D_train
    test_set = as.data.frame(D_test_i)
    
    # get training values for item j, excluding user i
    D_train_user_i_item_j = D_train %>% filter(item == user_i_item_j, user != user_i)
    # grab the users
    potential_coraters_user_i_item_j = D_train_user_i_item_j %>% select(user) %>% .$user
    
    
    # between test user and potential coraters - check if their similarity is stored in stored_sim_matrix
    
    
    if(use_stored_similarity){
      stored_sims_already_computed = stored_sim_matrix %>% 
        filter(user1 %in% c(user_i,potential_coraters_user_i_item_j)| 
                 user2 %in% c(user_i,potential_coraters_user_i_item_j)) %>% 
        select(user1, user2, sim_lu) %>% 
        na.omit()
    }
    
    # D_test_i[1,"user"]
    # potential_coraters_user_i_item_j
    
    
    
    # check if there are potential coraters 
    # this can be a spot to put a fallback predictor
    if(length(potential_coraters_user_i_item_j) > 0){
      
      
      # the set of coraters that don't have a similarity stored
      potential_coraters_user_i_item_j = setdiff(potential_coraters_user_i_item_j, 
                                                 unique(c(stored_sims_already_computed$user1, stored_sims_already_computed$user2)))
      # get data for coraters and original user
      potential_coraters_user_i_item_j_train_obs = D_train %>% filter(D_train$user %in% c(user_i, potential_coraters_user_i_item_j))
      # reshape data to be ratings matrix format
      potential_coraters_user_i_item_j_train_obs_reshaped = dcast(data = potential_coraters_user_i_item_j_train_obs, formula = user ~ item, value.var = "rating")

      # dim(potential_coraters_user_i_item_j_train_obs_reshaped)
      #potential_coraters_user_i_item_j_train_obs_reshaped
      
      rownames(potential_coraters_user_i_item_j_train_obs_reshaped) = potential_coraters_user_i_item_j_train_obs_reshaped$user
      potential_coraters_user_i_item_j_train_obs_reshaped = potential_coraters_user_i_item_j_train_obs_reshaped[,-1]
      
            # making sure row1 of matrix is the test user
      test_user_row = which(rownames(potential_coraters_user_i_item_j_train_obs_reshaped) == as.character(user_i))
      B_temp = potential_coraters_user_i_item_j_train_obs_reshaped[test_user_row,]
      
      old_name = rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[1]
      
      potential_coraters_user_i_item_j_train_obs_reshaped[test_user_row,] = potential_coraters_user_i_item_j_train_obs_reshaped[1,]
      potential_coraters_user_i_item_j_train_obs_reshaped[1,] = B_temp
      
      rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[1] = as.character("empty_first_row")
      rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[test_user_row] = as.character("empty_last_row")
      rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[1] = as.character(user_i)
      rownames(potential_coraters_user_i_item_j_train_obs_reshaped)[test_user_row] = as.character(old_name)
      
      
      # forcing into matrix format for manipulation purposes
      potential_coraters_user_i_item_j_train_obs_reshaped = as.matrix(potential_coraters_user_i_item_j_train_obs_reshaped)
      
      # the places where a prediction is needed
      k_error_df_lirau = pred_df_total_user_i %>% filter(item == user_i_item_j) #data.frame(user = user_i, item = user_i_item_j, K, k_current = NA, sim = "lira_uniform", weighted_pred = NA, unweighted_pred = NA)
      
      # after reshaping, is the matrix more than just the original user - is it possible that it's just the one user?
      if(nrow(potential_coraters_user_i_item_j_train_obs_reshaped) > 0){
        
        
        
        similarity_vector = compute_neighbor_similarity(user_item_matrix = potential_coraters_user_i_item_j_train_obs_reshaped,
                                                        test_observation =test_set,
                                                        similarity_measure = "lira_uniform")
        
        similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
        similarity_vector = similarity_vector[-1]
        
        
        if(use_stored_similarity){
          stored_sims_already_computed = stored_sims_already_computed %>% 
            select(user = user1, y = sim_lu) %>% filter(user != user_i)
          
          for(s in 1:length(similarity_vector)){
            
            stored_sim_matrix[which(((stored_sim_matrix$user1 == user_i & 
                                        stored_sim_matrix$user2 == names(similarity_vector)[s]) | 
                                       (stored_sim_matrix$user2 == user_i & 
                                          stored_sim_matrix$user1 == names(similarity_vector)[s]))),"sim_lu"] = 
              similarity_vector[s]
          }
        }
        
        # storing newly computed similarities

        
        # getting ratings and similarities together for newly computed neighbors
        neighbor_ratings_fresh = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
        neighbor_ratings_fresh = merge(neighbor_ratings_fresh[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
        
        
        # getting ratings and similarities together for stored neighbors
        
        if(use_stored_similarity){
          neighbor_ratings_precomputed = train_set[which(train_set$item == test_set$item & train_set$user %in% stored_sims_already_computed$user),]
          neighbor_ratings_precomputed = merge(neighbor_ratings_precomputed[c("user","rating")], stored_sims_already_computed, by.x = "user", by.y = "user")
          
          # putting the above together
          neighbor_ratings = bind_rows(neighbor_ratings_fresh, neighbor_ratings_precomputed) %>% arrange(desc(y))
        }else{neighbor_ratings = neighbor_ratings_fresh}
        
        
        #neighbor_ratings = neighbor_ratings[-1,] 
        neighbor_ratings = neighbor_ratings_fresh 
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          
          k_current = min(length(similarity_vector), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          
          prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
          
          sim = prediction_neighbors$rating #y
          
          # if(any(prediction_neighbors$y <= 0)){
          #   prediction_neighbors = prediction_neighbors %>% 
          #     mutate(temp_y = y + abs(min(y)) + eps, 
          #            weight_y = temp_y/max(temp_y))
          # }else{prediction_neighbors = prediction_neighbors %>% 
          #   mutate(weight_y = y/max(y))}
          
          
          k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)

          # pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
          
          pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
          k_error_df_lirau[k,"weighted_pred"] = pred_rating_weighted # - test_set$rating)
          
          pred_rating_unweighted = mean(prediction_neighbors$rating)
          k_error_df_lirau[k,"unweighted_pred"] = pred_rating_unweighted # - test_set$rating)
          
        }
      }
      
      pred_df_total[which(pred_df_total$test_user == user_i & 
                          pred_df_total$item == user_i_item_j),
                    c("pred_lu_weighted","pred_lu_unweighted")] = k_error_df_lirau[,c("weighted_pred","unweighted_pred")]
      
      
      # start
      
      if(nrow(potential_coraters_user_i_item_j_train_obs_reshaped) > 0){
        
        
        
        similarity_vector = compute_neighbor_similarity(user_item_matrix = potential_coraters_user_i_item_j_train_obs_reshaped,
                                                        test_observation = test_set,
                                                        similarity_measure = "lira_gaussian_pure_chance")
        
        similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
        similarity_vector = similarity_vector[-1]
        
        
        if(use_stored_similarity){
          stored_sims_already_computed = stored_sims_already_computed %>% 
            select(user = user1, y = sim_lu) %>% filter(user != user_i)
          
          for(s in 1:length(similarity_vector)){
            
            stored_sim_matrix[which(((stored_sim_matrix$user1 == user_i & 
                                        stored_sim_matrix$user2 == names(similarity_vector)[s]) | 
                                       (stored_sim_matrix$user2 == user_i & 
                                          stored_sim_matrix$user1 == names(similarity_vector)[s]))),"sim_lu"] = 
              similarity_vector[s]
          }
        }
        
        # storing newly computed similarities
        
        
        # getting ratings and similarities together for newly computed neighbors
        neighbor_ratings_fresh = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
        neighbor_ratings_fresh = merge(neighbor_ratings_fresh[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
        
        
        # getting ratings and similarities together for stored neighbors
        
        if(use_stored_similarity){
          neighbor_ratings_precomputed = train_set[which(train_set$item == test_set$item & train_set$user %in% stored_sims_already_computed$user),]
          neighbor_ratings_precomputed = merge(neighbor_ratings_precomputed[c("user","rating")], stored_sims_already_computed, by.x = "user", by.y = "user")
          
          # putting the above together
          neighbor_ratings = bind_rows(neighbor_ratings_fresh, neighbor_ratings_precomputed) %>% arrange(desc(y))
        }else{neighbor_ratings = neighbor_ratings_fresh}
        
        
        #neighbor_ratings = neighbor_ratings[-1,] 
        neighbor_ratings = neighbor_ratings_fresh 
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          
          k_current = min(length(similarity_vector), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          
          prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
          
          sim = prediction_neighbors$rating #y
          
          # if(any(prediction_neighbors$y <= 0)){
          #   prediction_neighbors = prediction_neighbors %>% 
          #     mutate(temp_y = y + abs(min(y)) + eps, 
          #            weight_y = temp_y/max(temp_y))
          # }else{prediction_neighbors = prediction_neighbors %>% 
          #   mutate(weight_y = y/max(y))}
          
          
          k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
          
          # pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
          
          pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
          k_error_df_lirau[k,"weighted_pred"] = pred_rating_weighted # - test_set$rating)
          
          pred_rating_unweighted = mean(prediction_neighbors$rating)
          k_error_df_lirau[k,"unweighted_pred"] = pred_rating_unweighted # - test_set$rating)
          
        }
      }
      
      pred_df_total[which(pred_df_total$test_user == user_i & 
                            pred_df_total$item == user_i_item_j),
                    c("pred_lg_weighted","pred_lg_unweighted")] = k_error_df_lirau[,c("weighted_pred","unweighted_pred")]
      
      
      
      ### end
      
      
      if(nrow(potential_coraters_user_i_item_j_train_obs_reshaped) > 0){
        
        
        
        similarity_vector = compute_neighbor_similarity(user_item_matrix = potential_coraters_user_i_item_j_train_obs_reshaped,
                                                        test_observation = test_set,
                                                        similarity_measure = "pearson_pwc")
        
        similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
        similarity_vector = similarity_vector[-1]
        
        
        if(use_stored_similarity){
          stored_sims_already_computed = stored_sims_already_computed %>% 
            select(user = user1, y = sim_lu) %>% filter(user != user_i)
          
          for(s in 1:length(similarity_vector)){
            
            stored_sim_matrix[which(((stored_sim_matrix$user1 == user_i & 
                                        stored_sim_matrix$user2 == names(similarity_vector)[s]) | 
                                       (stored_sim_matrix$user2 == user_i & 
                                          stored_sim_matrix$user1 == names(similarity_vector)[s]))),"sim_lu"] = 
              similarity_vector[s]
          }
        }
        
        # storing newly computed similarities
        
        
        # getting ratings and similarities together for newly computed neighbors
        neighbor_ratings_fresh = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
        neighbor_ratings_fresh = merge(neighbor_ratings_fresh[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
        
        
        # getting ratings and similarities together for stored neighbors
        
        if(use_stored_similarity){
          neighbor_ratings_precomputed = train_set[which(train_set$item == test_set$item & train_set$user %in% stored_sims_already_computed$user),]
          neighbor_ratings_precomputed = merge(neighbor_ratings_precomputed[c("user","rating")], stored_sims_already_computed, by.x = "user", by.y = "user")
          
          # putting the above together
          neighbor_ratings = bind_rows(neighbor_ratings_fresh, neighbor_ratings_precomputed) %>% arrange(desc(y))
        }else{neighbor_ratings = neighbor_ratings_fresh}
        
        
        #neighbor_ratings = neighbor_ratings[-1,] 
        neighbor_ratings = neighbor_ratings_fresh 
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          
          k_current = min(length(similarity_vector), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          
          prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
          
          sim = prediction_neighbors$rating #y
          
          # if(any(prediction_neighbors$y <= 0)){
          #   prediction_neighbors = prediction_neighbors %>% 
          #     mutate(temp_y = y + abs(min(y)) + eps, 
          #            weight_y = temp_y/max(temp_y))
          # }else{prediction_neighbors = prediction_neighbors %>% 
          #   mutate(weight_y = y/max(y))}
          
          
          k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
          
          # pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
          
          # should there be an absolute value up top?
          pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
          k_error_df_lirau[k,"weighted_pred"] = pred_rating_weighted # - test_set$rating)
          
          pred_rating_unweighted = mean(prediction_neighbors$rating)
          k_error_df_lirau[k,"unweighted_pred"] = pred_rating_unweighted # - test_set$rating)
          
        }
      }
      
      pred_df_total[which(pred_df_total$test_user == user_i & 
                            pred_df_total$item == user_i_item_j),
                    c("pred_pc_weighted","pred_pc_unweighted")] = k_error_df_lirau[,c("weighted_pred","unweighted_pred")]
      
      ### end cosine
      
      
      
      
      
    }
    
    
    
    cat("User:", user_i, "/", num_users, 
        ", Item: ", user_i_item_j,"|" , 
        j, "/", num_items ,
        "->", round(j/num_items *100,2),"% finished on user", 
        "| Percent users: ", round(i/num_users*100,2), "%", "\n")
    
    
  }
  
  end = Sys.time()
  print(end - start)
  end - start
  
  user_times[i] = as.numeric(difftime(end, start), units="hours")
  
  
}

summary(user_times)
mean(user_times)

sum(user_times)/24

sum(user_times)/length(user_times)*length(unique(D_train$user))/24

plot(user_times)

rm(stored_sim_matrix);rm(pred_df_total);gc()

head(pred_df_total)














## Start Evaluation
rm(sim_matrix);gc() # 
start = Sys.time()

num_cores = detectCores();
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
cat(format(Sys.time(), "%a %b %d %X %Y"), "\n")

sim_matrix = foreach(i = 1:num_shards, .combine = rbind, 
                     .packages = c("dplyr","reshape2","Rcpp"),
                     .noexport = c("top_n", 
                                   "lira_loop", 
                                   "lira_gaussian_loop", 
                                   "correlationCoefficient", 
                                   "cosine_vector_similarity")) %do% {
                                     source('GitHub/RecommenderSystems/recommender_systems_helper_functions.R')
                                     Rcpp::sourceCpp('GitHub/RecommenderSystems/Handling Large Data/Rcpp/neighborhood_based_evaluation_helper_files.cpp')
                                     
                                     # print(i)
                                     k_error_df_total = c() # work on predefining this maybe
                                     
                                     
                                     # shards[[i]] # the users in this shard
                                     # expand.grid(user = shards[[i]], item = unique(D_train$item)) #, pred_weighted = NA, pred_unweighted = NA)
                                     # nrow(pred_df)
                                     # 5*length(unique(D_train$item))
                                     
                                     
                                     D_subset = expand.grid(user = shards[[i]], item = unique(D_train$item)) #, pred_weighted = NA, pred_unweighted = NA)
                                     
                                     for(j in 1:nrow(D_subset)){
                                       
                                       cat("i: ", i,"/",num_shards," | j: ",j,"/",nrow(D_subset),  "\n")
                                       #j=1
                                       # print(j) # j = 60
                                       D_test_i = D_subset[j,]
                                       # D_test_i
                                       
                                       potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user)
                                       
                                       
                                       if(nrow(potential_coraters) > 0){
                                         A = D_train %>% filter(D_train$user %in% c(D_test_i$user,potential_coraters$user))
                                         B = dcast(data = A, formula = user~item, value.var = "rating")
                                         ####B = B[order(B$user),]
                                         
                                         rownames(B) = B$user
                                         B = as.matrix(B[,-1]) # unaltered from here on out
                                         
                                         if(any(rownames(B) == D_test_i$user)){
                                           test_user_row = which(rownames(B) == as.character(D_test_i$user))
                                           B_temp = B[test_user_row,]
                                           
                                           old_name = rownames(B)[1]
                                           
                                           B[test_user_row,] = B[1,]
                                           B[1,] = B_temp
                                           
                                           rownames(B)[1] = as.character(D_test_i$user)
                                           rownames(B)[test_user_row] = as.character(old_name)
                                           
                                           B = B[-1,]
                                         }
                                         
                                         # str(B)
                                         # str( B[which(rownames(B) %in% train_set$user),])
                                         # as.matrix(B)
                                         
                                         train_set = D_train
                                         test_set = D_test_i
                                         
                                         
                                         rownames_B = rownames(B)
                                         # only carry around actual co-raters
                                         
                                         if(is.null(nrow(B))){B = t(as.matrix(B)); rownames(B) = rownames_B}else{B = B[which(rownames(B) %in% train_set$user),]}
                                         
                                         
                                         ### make similarity vector out here...
                                         ### manipulate and declare temps inside of nrow(B) > 1
                                         
                                         # k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_lrt_test", ae_nn = NA, ae_knn = NA)
                                         # if(nrow(B) > 1){
                                         # 
                                         #   similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                         #                                                   test_observation = D_test_i,
                                         #                                                   similarity_measure = "lira_lrt")
                                         # 
                                         #   similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                         # 
                                         #   neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                         #   neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                         #   neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
                                         # 
                                         #   # columns: k, k_current, ae_nn, ae_knn
                                         #   # dimensions: K * 4
                                         #   for(k in 1:length(K)){
                                         # 
                                         #     k_current = min(length(similarity_vector), K[k])
                                         #     k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                         # 
                                         #     prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                         # 
                                         #     sim = prediction_neighbors$rating#y
                                         # 
                                         #     if(length(tau) == 1){
                                         #       if(length(sim) > min_num_sim){
                                         #         if(sd(sim) > tau*sd(neighbor_ratings$rating)){
                                         #           
                                         #           
                                         #           prediction_neighbors = prediction_neighbors %>%
                                         #             filter(rating >= quantile(rating, probs = lower_q) & rating <= quantile(rating, probs = upper_q))
                                         #             #filter(rating > 1*mean(rating) - sd_scale*sd(rating) & rating < 1*mean(rating) + sd_scale*sd(rating))
                                         #             
                                         #           
                                         #           #filter(y > 1*mean(y) - sd_scale*sd(y)) # - 1.25*sd(y) & y < 1*mean(y) + 1.25*sd(y)
                                         #           #sim = sim[sim > 1*mean(sim) - 1.5*sd(sim) & sim < 1*mean(sim) + 1.5*sd(sim)]
                                         #           #sim = sim[sim > quantile(sim, probs = 0.5)]
                                         #         }
                                         #       }
                                         # 
                                         #     }
                                         # 
                                         #     #sim = sim[sim > 1*mean(neighbor_ratings$y) - 1.5*sd(neighbor_ratings$y)]
                                         #     #prediction_neighbors = neighbor_ratings[1:length(sim),]
                                         # 
                                         #     if(any(prediction_neighbors$y <= 0)){
                                         #       prediction_neighbors = prediction_neighbors %>% mutate(temp_y = y + abs(min(y)) + eps, weight_y = temp_y/max(temp_y))
                                         #     }else{prediction_neighbors = prediction_neighbors %>% mutate(weight_y = y/max(y))}
                                         #     
                                         # 
                                         #     k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                         #     
                                         #     pred_rating_nn = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                         #     k_error_df_lirau[k,"ae_nn"] = abs(pred_rating_nn - test_set$rating)
                                         # 
                                         #     pred_rating_knn = mean(prediction_neighbors$rating)
                                         #     k_error_df_lirau[k,"ae_knn"] = abs(pred_rating_knn - test_set$rating)
                                         # 
                                         #   }
                                         # }
                                         # k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
                                         
                                         k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K, k_current = NA, sim = "lira_uniform", weighted_pred = NA, unweighted_pred = NA)
                                         if(nrow(B) > 0){
                                           
                                           similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                                                           test_observation = D_test_i,
                                                                                           similarity_measure = "lira_uniform")
                                           
                                           similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                           
                                           neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                           neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                           neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
                                           
                                           # columns: k, k_current, ae_nn, ae_knn
                                           # dimensions: K * 4
                                           for(k in 1:length(K)){
                                             
                                             k_current = min(length(similarity_vector), K[k])
                                             k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                             
                                             prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                             
                                             sim = prediction_neighbors$rating #y
                                             
                                             if(any(prediction_neighbors$y <= 0)){
                                               prediction_neighbors = prediction_neighbors %>% mutate(temp_y = y + abs(min(y)) + eps, weight_y = temp_y/max(temp_y))
                                             }else{prediction_neighbors = prediction_neighbors %>% mutate(weight_y = y/max(y))}
                                             
                                             
                                             k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                             
                                             pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                             k_error_df_lirau[k,"weighted_pred"] = pred_rating_weighted # - test_set$rating)
                                             
                                             pred_rating_unweighted = mean(prediction_neighbors$rating)
                                             k_error_df_lirau[k,"unweighted_pred"] = pred_rating_unweighted # - test_set$rating)
                                             
                                           }
                                         }
                                         k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
                                         
                                         k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K, k_current = NA, sim = "lira_gaussian_pc_sd4", weighted_pred = NA, unweighted_pred = NA)
                                         if(nrow(B) > 0){
                                           
                                           similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                                                           test_observation = D_test_i,
                                                                                           similarity_measure = "lira_gaussian_pure_chance")
                                           
                                           similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                           
                                           neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                           neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                           neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
                                           
                                           # columns: k, k_current, ae_nn, ae_knn
                                           # dimensions: K * 4
                                           for(k in 1:length(K)){
                                             
                                             k_current = min(length(similarity_vector), K[k])
                                             k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                             
                                             prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                             
                                             sim = prediction_neighbors$rating #y
                                             
                                             if(any(prediction_neighbors$y <= 0)){
                                               prediction_neighbors = prediction_neighbors %>% mutate(temp_y = y + abs(min(y)) + eps, weight_y = temp_y/max(temp_y))
                                             }else{prediction_neighbors = prediction_neighbors %>% mutate(weight_y = y/max(y))}
                                             
                                             
                                             k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                             
                                             pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                             k_error_df_lirau[k,"weighted_pred"] = pred_rating_weighted # - test_set$rating)
                                             
                                             pred_rating_unweighted = mean(prediction_neighbors$rating)
                                             k_error_df_lirau[k,"unweighted_pred"] = pred_rating_unweighted # - test_set$rating)
                                             
                                           }
                                           
                                         }
                                         k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
                                         
                                         k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K, k_current = NA, sim = "pearson_pwc", weighted_pred = NA, unweighted_pred = NA)
                                         if(nrow(B) > 0){
                                           
                                           similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                                                           test_observation = D_test_i,
                                                                                           similarity_measure = "pearson_pwc")
                                           
                                           similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                           
                                           neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                           neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                           neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
                                           
                                           # columns: k, k_current, ae_nn, ae_knn
                                           # dimensions: K * 4
                                           for(k in 1:length(K)){
                                             
                                             k_current = min(length(similarity_vector), K[k])
                                             k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                             
                                             prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                             
                                             sim = prediction_neighbors$rating #y
                                             
                                             if(any(prediction_neighbors$y <= 0)){
                                               prediction_neighbors = prediction_neighbors %>% mutate(temp_y = y + abs(min(y)) + eps, weight_y = temp_y/max(temp_y))
                                             }else{prediction_neighbors = prediction_neighbors %>% mutate(weight_y = y/max(y))}
                                             
                                             
                                             k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                             
                                             pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                             k_error_df_lirau[k,"weighted_pred"] = pred_rating_weighted # - test_set$rating)
                                             
                                             pred_rating_unweighted = mean(prediction_neighbors$rating)
                                             k_error_df_lirau[k,"unweighted_pred"] = pred_rating_unweighted # - test_set$rating)
                                             
                                           }
                                         }
                                         k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
                                         
                                         k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K, k_current = NA, sim = "cosine", weighted_pred = NA, unweighted_pred = NA)
                                         if(nrow(B) > 0){
                                           
                                           similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                                                           test_observation = D_test_i,
                                                                                           similarity_measure = "cosine")
                                           
                                           similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
                                           
                                           neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
                                           neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
                                           neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
                                           
                                           # columns: k, k_current, ae_nn, ae_knn
                                           # dimensions: K * 4
                                           for(k in 1:length(K)){
                                             
                                             k_current = min(length(similarity_vector), K[k])
                                             k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
                                             
                                             prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
                                             
                                             sim = prediction_neighbors$rating #y
                                             
                                             if(any(prediction_neighbors$y <= 0)){
                                               prediction_neighbors = prediction_neighbors %>% mutate(temp_y = y + abs(min(y)) + eps, weight_y = temp_y/max(temp_y))
                                             }else{prediction_neighbors = prediction_neighbors %>% mutate(weight_y = y/max(y))}
                                             
                                             
                                             k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
                                             
                                             pred_rating_weighted = sum(prediction_neighbors$rating * prediction_neighbors$weight_y)/sum(abs(prediction_neighbors$weight_y)) # is this supposed to be /|abs(sim)|
                                             k_error_df_lirau[k,"weighted_pred"] = pred_rating_weighted # - test_set$rating)
                                             
                                             pred_rating_unweighted = mean(prediction_neighbors$rating)
                                             k_error_df_lirau[k,"unweighted_pred"] = pred_rating_unweighted # - test_set$rating)
                                             
                                           }
                                         }
                                         k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
                                         
                                         
                                       }
                                       
                                     }
                                     
                                     k_error_df_total 
                                     
                                   }

stopCluster(cl);# dim(M)
end = Sys.time()
end - start # 1.02  

discordr::send_message(message = paste0("Mystery Machine done, took ", 
                                        round(end - start,2)," ",
                                        units(end - start), " "))

# EXAMINE BEFORE SAVING
head(sim_matrix, n = 15) # visual inspection

#D_sim = bind_rows(D_sim, sim_matrix) 

#D_100ml_non_bm_test = c()
D_100ml_non_bm_test = bind_rows(D_100ml_non_bm_test, sim_matrix)

D_100ml_non_bm_test %>%
  group_by(sim, K) %>%
  summarize(MAE_nn = mean(ae_nn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1)  # + ggtitle("MovieLens - 100k - latest")

data = D_100ml_non_bm_test
data %>% 
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_nn, na.rm = T), 
            RMSE_nn = sqrt(mean(ae_nn^2, na.rm = T)), 
            sd_nn = sd(ae_nn, na.rm = T)) %>% #print(n = 72)
  group_by(sim) %>%
  summarize(min_mae = min(MAE_nn)) %>%
  inner_join(data %>%
               group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
               summarize(MAE_nn = mean(ae_nn, na.rm = T), 
                         RMSE_nn = sqrt(mean(ae_nn^2, na.rm = T)), 
                         sd_nn = sd(ae_nn, na.rm = T)),
             by = c("sim" = "sim", "min_mae" = "MAE_nn")) %>% 
  arrange(min_mae) %>% 
  inner_join(data %>% 
               group_by(sim) %>% 
               mutate(count = n()) %>% 
               filter(is.na(ae_nn)) %>% 
               group_by(sim) %>% 
               mutate(count_na = n()) %>% 
               mutate(na_pct = 100*count_na/count) %>% 
               select(sim, na_pct) %>% unique(), by = c("sim" = "sim") )

##

D_100ml_non_bm_test %>%
  group_by(sim, K) %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1)  # + ggtitle("MovieLens - 100k - latest")

data = D_100ml_non_bm_test
data %>% 
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T), 
            RMSE_nn = sqrt(mean(ae_knn^2, na.rm = T)), 
            sd_nn = sd(ae_knn, na.rm = T)) %>% #print(n = 72)
  group_by(sim) %>%
  summarize(min_mae = min(MAE_nn)) %>%
  inner_join(data %>%
               group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
               summarize(MAE_nn = mean(ae_knn, na.rm = T), 
                         RMSE_nn = sqrt(mean(ae_knn^2, na.rm = T)), 
                         sd_nn = sd(ae_knn, na.rm = T)),
             by = c("sim" = "sim", "min_mae" = "MAE_nn")) %>% 
  arrange(min_mae) %>% 
  inner_join(data %>% 
               group_by(sim) %>% 
               mutate(count = n()) %>% 
               filter(is.na(ae_knn)) %>% 
               group_by(sim) %>% 
               mutate(count_na = n()) %>% 
               mutate(na_pct = 100*count_na/count) %>% 
               select(sim, na_pct) %>% unique(), by = c("sim" = "sim") )




###################################

#write_csv(sim_matrix_testing_modified_sd4, "ml_100k_latest_sim_matrix_testing_modified_sd4.csv")

data %>% group_by(sim, K) %>%
  summarize(mu_k = mean(k_current, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = mu_k, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1) 




emergency_comparisons_dataset4 
D_sim 


data = bind_rows(sm1,sm2,sm3,sm4,sm5,
                 sm1a,sm2a,sm3a,sm4a,sm5a,
                 sm1ab,sm2ab,sm3ab,sm4ab,sm5ab)#bind_rows(sm1abc,sm2abc,sm3abc)  #bind_rows(sm1,sm2,sm3,sm4,sm5,sm1a,sm2a,sm3a,sm4a,sm5a)  #sim_matrix #emergency_comparisons_dataset4 # sim_matrix_testing_modified_sd4 # 
data = D_100ml_non_bm_test  #sim_matrix 
data %>%
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T), sd_nn = sd(ae_knn, na.rm = T)) %>% #print(n = 72)
  group_by(sim) %>%
  summarize(min_mae = min(MAE_nn)) %>%
  inner_join(data %>%
               group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
               summarize(MAE_nn = mean(ae_knn, na.rm = T), sd_nn = sd(ae_knn, na.rm = T)),
             by = c("sim" = "sim", "min_mae" = "MAE_nn")) %>% 
  arrange(min_mae) %>% 
  inner_join(data %>% 
               group_by(sim) %>% 
               mutate(count = n()) %>% 
               filter(is.na(ae_knn)) %>% 
               group_by(sim) %>% 
               mutate(count_na = n()) %>% 
               mutate(na_pct = 100*count_na/count) %>% 
               select(sim, na_pct) %>% unique(),
             by = c("sim" = "sim")) %>% print(n = 75)





