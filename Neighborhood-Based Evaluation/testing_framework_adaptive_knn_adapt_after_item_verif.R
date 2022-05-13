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

D_train = D5_train # D[train_index,]
D_test = D5_test # D[-train_index,]
nrow(D_test);nrow(D_train)


## Build LiRa Distributions
source('GitHub/RecommenderSystems/build_lira_distributions.R')
## Chunk test set
source('GitHub/RecommenderSystems/chunk_test_set.R')

## LiRa LRT Variance Estimation
#sd_pop = lira_lrt_sd_sampling(dataset = D_train, iter = 1500)

## LiRa Gaussian - Pure Chance Standard Deviation Parameter
sd_pc = 4
use_lira = T; use_pearson_pwc = T; use_lira_gauss = T; use_cosine = T
## Setting k for kNN 
K = c(3,5,10,15,20,30,50,80,160)# 3,5,7, 40,50,60,80,160)

## Start Evaluation
rm(sim_matrix);gc() # 
start = Sys.time()

num_cores = detectCores();
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
cat(format(Sys.time(), "%a %b %d %X %Y"), "\n")

sim_matrix = foreach(i = 1:num_shards, .combine = rbind, 
                     .packages = c("dplyr","reshape2","Rcpp"),
                     .noexport = c("top_n", "lira_loop", "lira_gaussian_loop", 
                                   "correlationCoefficient", "cosine_vector_similarity")) %dopar% {
  source('GitHub/RecommenderSystems/recommender_systems_helper_functions.R')
  Rcpp::sourceCpp('GitHub/RecommenderSystems/Handling Large Data/Rcpp/neighborhood_based_evaluation_helper_files.cpp')
                       
  print(i)
  k_error_df_total = c() # work on predefining this maybe
  
  # i = 39
  D_subset = D_test[shards[[i]],]
  
  # j = which(D_subset$user == 90 & D_subset$item == 581)
  
  for(j in 1:nrow(D_subset)){
    # print(j) # j = 60
    D_test_i = D_subset[j,]
    
    potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user)
    if(nrow(potential_coraters) > 0){
      A = D_train %>% filter(D_train$user %in% c(D_test_i$user,potential_coraters$user))
      B = dcast(data = A, formula = user~item, value.var = "rating")
      rownames(B) = B$user
      B = as.matrix(B[,-1]) # unaltered from here on out
      
      test_user_row = which(rownames(B) == as.character(D_test_i$user))
      B_temp = B[test_user_row,]
      
      old_name = rownames(B)[1]
      
      B[test_user_row,] = B[1,]
      B[1,] = B_temp
      
      rownames(B)[1] = as.character(D_test_i$user)
      rownames(B)[test_user_row] = as.character(old_name)
      
      train_set = D_train
      test_set = D_test_i
      
      B = B[which(rownames(B) %in% train_set$user),] # only carry around actual co-raters
      
      # for fallback predictions
      user_avg = train_set %>% filter(user == test_set$user) %>% summarize(mu_user = mean(rating)) %>% .$mu_user
      ae = abs(user_avg - test_set$rating)
      
      if(use_lira){

        if(nrow(B) > 1){
          
          k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K, 
                                        k_current = NA, sim = "lira_uniform", pred_nn = NA, ae_nn = NA, pred_knn = NA, ae_knn = NA)
          
          similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                          test_observation = D_test_i,
                                                          similarity_measure = "lira_uniform")
          
          similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
          
          neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
          neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
          neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
          
          # columns: k, k_current, ae_nn, ae_knn
          # dimensions: K * 4
          if(nrow(neighbor_ratings) > 0){
            for(k in 1:length(K)){
              
              k_current = min(length(similarity_vector), K[k])
              k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
              
              prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
              
              sim = prediction_neighbors$rating#y
              
              k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
              
              pred_rating_nn = sum(prediction_neighbors$rating * abs(prediction_neighbors$y))/sum(abs(prediction_neighbors$y))
              k_error_df_lirau[k,"pred_nn"] = pred_rating_nn 
              k_error_df_lirau[k,"ae_nn"] = abs(pred_rating_nn - test_set$rating)
              
              pred_rating_knn = mean(prediction_neighbors$rating)
              k_error_df_lirau[k,"pred_knn"] = pred_rating_knn
              k_error_df_lirau[k,"ae_knn"] = abs(pred_rating_knn - test_set$rating)
              
            }
          }else{
            k_error_df_lirau[,"k_current"] = NA
            
            k_error_df_lirau[,"pred_nn"] = user_avg
            k_error_df_lirau[,"ae_nn"] = ae
            
            k_error_df_lirau[,"pred_knn"] = user_avg
            k_error_df_lirau[,"ae_knn"] = ae
          }
          
          if(any(is.nan(k_error_df_lirau$pred_nn))){
            k_error_df_lirau[,"k_current"] = NA
            
            k_error_df_lirau[,"pred_nn"] = user_avg
            k_error_df_lirau[,"ae_nn"] = ae
            
            k_error_df_lirau[,"pred_knn"] = user_avg
            k_error_df_lirau[,"ae_knn"] = ae
          } # this is in case the only neighbor has similarity 0
          
          
        }
       
        k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      }

      if(use_lira_gauss){

        if(nrow(B) > 1){
          
          k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, 
                                        K, k_current = NA, sim = "lira_gaussian_pc_sd4", pred_nn = NA, ae_nn = NA, pred_knn = NA, ae_knn = NA)
          
          similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                          test_observation = D_test_i,
                                                          similarity_measure = "lira_gaussian_pure_chance")
          
          similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
          
          neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
          neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
          
          neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
          
          
          
          
          # columns: k, k_current, ae_nn, ae_knn
          # dimensions: K * 4
          if(nrow(neighbor_ratings) > 0){
            for(k in 1:length(K)){
              
              k_current = min(length(similarity_vector), K[k])
              k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
              
              prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
              
              sim = prediction_neighbors$rating#y
              
              k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
              
              pred_rating_nn = sum(prediction_neighbors$rating * abs(prediction_neighbors$y))/sum(abs(prediction_neighbors$y))
              k_error_df_lirau[k,"pred_nn"] = pred_rating_nn
              k_error_df_lirau[k,"ae_nn"] = abs(pred_rating_nn - test_set$rating)
              
              pred_rating_knn = mean(prediction_neighbors$rating)
              k_error_df_lirau[k,"pred_knn"] = pred_rating_knn
              k_error_df_lirau[k,"ae_knn"] = abs(pred_rating_knn - test_set$rating)
              
            }
          }else{
            k_error_df_lirau[,"k_current"] = NA
            
            k_error_df_lirau[,"pred_nn"] = user_avg
            k_error_df_lirau[,"ae_nn"] = ae
            
            k_error_df_lirau[,"pred_knn"] = user_avg
            k_error_df_lirau[,"ae_knn"] = ae
          }
          
          if(any(is.nan(k_error_df_lirau$pred_nn))){
            k_error_df_lirau[,"k_current"] = NA
            
            k_error_df_lirau[,"pred_nn"] = user_avg
            k_error_df_lirau[,"ae_nn"] = ae
            
            k_error_df_lirau[,"pred_knn"] = user_avg
            k_error_df_lirau[,"ae_knn"] = ae
          } # this is in case the only neighbor has similarity 0
        }
        
        k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)        
      }

      if(use_pearson_pwc){

      if(nrow(B) > 1){
        
        k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, 
                                      K, k_current = NA, sim = "pearson_pwc", pred_nn = NA, ae_nn = NA, pred_knn = NA, ae_knn = NA)
        
        similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                        test_observation = D_test_i,
                                                        similarity_measure = "pearson_pwc")
        
        similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
        
        neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
        neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
        
        neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
        
        
        
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        
        if(nrow(neighbor_ratings) > 0){
          for(k in 1:length(K)){
            
            k_current = min(length(similarity_vector), K[k])
            k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
            
            prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
            
            sim = prediction_neighbors$rating#y
          
            k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
            
            pred_rating_nn = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y))
            k_error_df_lirau[k,"pred_nn"] = pred_rating_nn
            k_error_df_lirau[k,"ae_nn"] = abs(pred_rating_nn - test_set$rating)
            
            pred_rating_knn = mean(prediction_neighbors$rating)
            k_error_df_lirau[k,"pred_knn"] = pred_rating_knn
            k_error_df_lirau[k,"ae_knn"] = abs(pred_rating_knn - test_set$rating)
            
          }
        }else{
          k_error_df_lirau[,"k_current"] = NA
          
          k_error_df_lirau[,"pred_nn"] = user_avg
          k_error_df_lirau[,"ae_nn"] = ae
          
          k_error_df_lirau[,"pred_knn"] = user_avg
          k_error_df_lirau[,"ae_knn"] = ae
        }
        
        if(any(is.nan(k_error_df_lirau$pred_nn))){
          k_error_df_lirau[,"k_current"] = NA
          
          k_error_df_lirau[,"pred_nn"] = user_avg
          k_error_df_lirau[,"ae_nn"] = ae
          
          k_error_df_lirau[,"pred_knn"] = user_avg
          k_error_df_lirau[,"ae_knn"] = ae
        } # this is in case the only neighbor has similarity 0

      }
        
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      }
      
      if(use_cosine){

      if(nrow(B) > 1){
        
        k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, 
                                      K, k_current = NA, sim = "cosine", pred_nn = NA, ae_nn = NA, pred_knn = NA, ae_knn = NA)
        
        similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                        test_observation = D_test_i,
                                                        similarity_measure = "cosine")
        
        similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
        
        neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
        neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
        
        neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
        
        
        
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        if(nrow(neighbor_ratings) > 0){
          for(k in 1:length(K)){
            
            k_current = min(length(similarity_vector), K[k])
            k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
            
            prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
            
            sim = prediction_neighbors$rating#y
            
            k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
            
            pred_rating_nn = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y))
            k_error_df_lirau[k,"pred_nn"] = pred_rating_nn
            k_error_df_lirau[k,"ae_nn"] = abs(pred_rating_nn - test_set$rating)
            
            pred_rating_knn = mean(prediction_neighbors$rating)
            k_error_df_lirau[k,"pred_knn"] = pred_rating_knn
            k_error_df_lirau[k,"ae_knn"] = abs(pred_rating_knn - test_set$rating)
            
          }
        }else{
          k_error_df_lirau[,"k_current"] = NA
          
          k_error_df_lirau[,"pred_nn"] = user_avg
          k_error_df_lirau[,"ae_nn"] = ae
          
          k_error_df_lirau[,"pred_knn"] = user_avg
          k_error_df_lirau[,"ae_knn"] = ae
        }
        
        if(any(is.nan(k_error_df_lirau$pred_nn))){
          k_error_df_lirau[,"k_current"] = NA
          
          k_error_df_lirau[,"pred_nn"] = user_avg
          k_error_df_lirau[,"ae_nn"] = ae
          
          k_error_df_lirau[,"pred_knn"] = user_avg
          k_error_df_lirau[,"ae_knn"] = ae
        } # this is in case the only neighbor has similarity 0
        
      }
        
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      }
      
    }else{
      # fallback predictors
      
      train_set = D_train
      test_set = D_test_i

      if(use_lira){
        k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K = NA, 
                                      k_current = NA, sim = "lira_uniform", 
                                      pred_nn = user_avg, ae_nn = ae, pred_knn = user_avg, ae_knn = ae)
        k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      }
      
      if(use_lira_gauss){
        k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K = NA, 
                                      k_current = NA, sim = "lira_gaussian_pc_sd4", 
                                      pred_nn = user_avg, ae_nn = ae, pred_knn = user_avg, ae_knn = ae)
        k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      }
      
      if(use_pearson_pwc){
        k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K = NA, 
                                      k_current = NA, sim = "pearson_pwc", 
                                      pred_nn = user_avg, ae_nn = ae, pred_knn = user_avg, ae_knn = ae)
        k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      }
      
      if(use_cosine){
        k_error_df_lirau = data.frame(user = test_set$user, item = test_set$item, K = NA, 
                                      k_current = NA, sim = "cosine", 
                                      pred_nn = user_avg, ae_nn = ae, pred_knn = user_avg, ae_knn = ae)
        k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      }
     
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

#D_sim = c()
D_sim = bind_rows(D_sim, sim_matrix) 

A = D_sim %>%
    group_by(sim, K) %>%
  summarize(MAE = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1) + 
  ggtitle("MovieLens - 100k - latest", subtitle = "Unweighted")

B = D_sim %>%
  group_by(sim, K) %>%
  summarize(MAE = mean(ae_nn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1) + 
  ggtitle("MovieLens - 100k - latest", subtitle = "Weighted")

C = D_sim %>%
  group_by(sim, K) %>%
  summarize(RMSE = sqrt(mean(ae_knn^2, na.rm = T))) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = RMSE, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1) + 
  ggtitle("MovieLens - 100k - latest", subtitle = "Unweighted")

D = D_sim %>%
  group_by(sim, K) %>%
  summarize(RMSE = sqrt(mean(ae_nn^2, na.rm = T))) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = RMSE, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1) + 
  ggtitle("MovieLens - 100k - latest", subtitle = "Weighted")


cowplot::plot_grid(A, B, C, D)



data = D_sim
data %>% 
  group_by(sim, K) %>% filter(!is.na(K)) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T), 
            RMSE_nn = sqrt(mean(ae_knn^2, na.rm = T)), 
            sd_nn = sd(ae_knn, na.rm = T)) %>% #print(n = 72)
  group_by(sim) %>%
  summarize(min_mae = min(RMSE_nn)) %>%
  inner_join(data %>% filter(!is.na(K)) %>%
               group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
               summarize(MAE_nn = mean(ae_knn, na.rm = T), 
                         RMSE_nn = sqrt(mean(ae_knn^2, na.rm = T)), 
                         sd_nn = sd(ae_knn, na.rm = T)),
             by = c("sim" = "sim", "min_mae" = "RMSE_nn")) %>% 
  arrange(min_mae)






















data %>% 
  group_by(sim, K) %>% filter(!is.na(K)) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
summarize(MAE_nn = mean(ae_knn, na.rm = T), 
          RMSE_nn = sqrt(mean(ae_knn^2, na.rm = T)), 
          sd_nn = sd(ae_knn, na.rm = T)) %>% #print(n = 72)
group_by(sim) %>%
summarize(min_mae = min(RMSE_nn)) %>%
inner_join(data %>% filter(!is.na(K)) %>%
             group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
             summarize(MAE_nn = mean(ae_knn, na.rm = T), 
                       RMSE_nn = sqrt(mean(ae_knn^2, na.rm = T)), 
                       sd_nn = sd(ae_knn, na.rm = T)),
           by = c("sim" = "sim", "min_mae" = "RMSE_nn")) %>% 
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





