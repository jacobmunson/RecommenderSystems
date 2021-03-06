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

message("Splitting data...")
dataset = D
source('GitHub/RecommenderSystems/Handling Large Data/general_cross_validation.R')

D_train = D1_train # D[train_index,]
D_test = D1_test # D[-train_index,]
nrow(D_test);nrow(D_train)


## Build LiRa Distributions
source('GitHub/RecommenderSystems/build_lira_distributions.R')
## Chunk test set
shard_multiplier = 20
source('GitHub/RecommenderSystems/chunk_test_set_users.R')

## LiRa LRT Variance Estimation
#sd_pop = lira_lrt_sd_sampling(dataset = D_train, iter = 1500)

## LiRa Gaussian - Pure Chance Standard Deviation Parameter
sd_pc = 4
## Setting k for kNN 
K = c(3,5,7,10,15,20,30,40,50,60,80,160)
eps = 0.001
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
                                   "cosine_vector_similarity")) %dopar% {
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





