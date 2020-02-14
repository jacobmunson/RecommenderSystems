###########################

## Relevant Packages
library(dplyr)
library(readr)
library(reshape2)
library(doParallel)

source('~/Documents/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

# ratings <- read.table("Recommender Systems - Home Folder/ml-1m/ratings.dat")
# ratings <- read.table("Recommender Systems - Home Folder/ml-10M/ratings.dat")
# ratings <- read.table("Documents/Recommender Systems - Home Folder/ml-1m/ratings.dat")

# Format user::item::rating::timestamp
D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
head(D) # visual check
#dim(D) # how many? 
#head(D)
D = as_tibble(D)


D <- read_csv("~/Documents/ml-latest-small/ml-latest-small/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")



set.seed(1)

train_index = sample(x = 1:nrow(D), size = 0.8*nrow(D), replace = F)
D_train = D[train_index,]
D_test = D[-train_index,]

lira_same_cluster_pdf = lira_same_cluster_distribution(V = sort(unique(D_train$rating))) 
lira_pure_chance_pdf = lira_pure_chance_distribution(V = sort(unique(D_train$rating))) 
lira_binary_same_cluster_pdf = lira_binary_same_cluster_distribution()
lira_binary_pure_chance_pdf = lira_binary_pure_chance_distribution()

nrow(D_test)
nrow(D_train)

## Splitting the dataset into smaller peices for parallel processing
chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}
dim(D_test)
num_shards = detectCores()*50
shards = chunk(vector = seq(1:nrow(D_test)), num_splits = num_shards)
names(shards) = seq(1,num_shards,1)

## Estimating variance for  LiRa LRT Similarity
sd_pop = lira_lrt_sd_sampling(dataset = D_train, iter = 1500)
sd_pc = 2

## Setting k for kNN 
K = c(3,5,7,10,15,20,30,40,50,60,80,160)

## Start Evaluation
rm(sim_matrix);gc() # 
start = Sys.time()

num_cores = detectCores();
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
cat(format(Sys.time(), "%a %b %d %X %Y"), "\n")

sim_matrix = foreach(i = 1:num_shards, .combine = rbind, .packages = c("dplyr","reshape2")) %dopar% {

  k_error_df_total = c()

  D_subset =  D_test[shards[[i]],]

  for(j in 1:nrow(D_subset)){
    #print(j) # j = 8
    D_test_i = D_subset[j,]
    
    potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user)
    if(nrow(potential_coraters) > 0){
      A = D_train %>% filter(D_train$user %in% c(D_test_i$user,potential_coraters$user))
      B = dcast(data = A, formula = user~item, value.var = "rating")
      B = B[order(B$user),]
      rownames(B) = B$user
      B = as.matrix(B[,-1]) # unaltered from here on out
      

      
      ## done
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gaussian_mu0_sdneg0.5", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){

        B_lirau = compute_neighbor_similarity(user_item_matrix = B,
                                              test_observation = D_test_i,
                                              similarity_measure = "lira_gaussian_pure_chance")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau,
                                                      sd_scaling = T, mean_scaling = T,
                                                      sd_scale = -0.5, mu_scale = 0)
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          k_current = min(length(B_lirau), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          k_error_df_lirau[k,"k_current"] = k_current
          #B_lirau = B_lirau[1:k_current]

          pred_lirau = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau[1:k_current])
          k_error_df_lirau[k,"ae_nn"] = pred_lirau[1]
          k_error_df_lirau[k,"ae_knn"] = pred_lirau[2]
        }
      }
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)

      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gaussian_mu0_sdneg1", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B,
                                              test_observation = D_test_i,
                                              similarity_measure = "lira_gaussian_pure_chance")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau,
                                                      sd_scaling = T, mean_scaling = T,
                                                      sd_scale = -1, mu_scale = 0)
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          k_current = min(length(B_lirau), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          k_error_df_lirau[k,"k_current"] = k_current
          #B_lirau = B_lirau[1:k_current]
          
          pred_lirau = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau[1:k_current])
          k_error_df_lirau[k,"ae_nn"] = pred_lirau[1]
          k_error_df_lirau[k,"ae_knn"] = pred_lirau[2]
        }
      }
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gaussian_mu1", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B,
                                              test_observation = D_test_i,
                                              similarity_measure = "lira_gaussian_pure_chance")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau,
                                                      mean_scaling = T,mu_scale = 1)

        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          k_current = min(length(B_lirau), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          k_error_df_lirau[k,"k_current"] = k_current
          #B_lirau = B_lirau[1:k_current]
          
          pred_lirau = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau[1:k_current])
          k_error_df_lirau[k,"ae_nn"] = pred_lirau[1]
          k_error_df_lirau[k,"ae_knn"] = pred_lirau[2]
        }
      }
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gaussian_mu0.5", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B,
                                              test_observation = D_test_i,
                                              similarity_measure = "lira_gaussian_pure_chance")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau,
                                                      mean_scaling = T,mu_scale = 0.5)
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          k_current = min(length(B_lirau), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          k_error_df_lirau[k,"k_current"] = k_current
          #B_lirau = B_lirau[1:k_current]
          
          pred_lirau = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau[1:k_current])
          k_error_df_lirau[k,"ae_nn"] = pred_lirau[1]
          k_error_df_lirau[k,"ae_knn"] = pred_lirau[2]
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


dim(sim_matrix)
head(sim_matrix, n = 15) # visual inspection


sim_matrix %>% 
  group_by(sim, K) %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1)


#1
# RUN THIS
#D_ml_latest = D_sim
# D1 = sim_matrix
# D2 = sim_matrix
# D3 = sim_matrix
#D4 = sim_matrix
#D5 = sim_matrix
#D6 = sim_matrix
D3 %>% 
  filter(sim == "lira_multinomial_gaussian_as1_pos_only")

D_sim = bind_rows(D_sim,sim_matrix)#,D2,D3,D4,D5)


D_sim %>% 
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + 
  geom_line(linetype="dashed", size=1) + 
  ggtitle("MovieLens - 100k - latest")

send_current_ggplot()

#send_console(
D_sim %>% 
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>%
  group_by(sim) %>% 
  summarize(min_mae = min(MAE_nn)) %>%
  inner_join(D_sim %>% 
               group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
               summarize(MAE_nn = mean(ae_knn, na.rm = T)), by = c("sim" = "sim", "min_mae" = "MAE_nn"))
#)


# lira_lrt_ml100k_test_run = sim_matrix # 45.31min
# lira_gaussian_ml100k_test_run = sim_matrix # 48.70min


#str(D_total_emergency)
unique(emergency_comparisons_dataset4$sim)
D_lira = D_total_emergency %>% filter(sim == "lirau")
D_lira %>% head()


# D_total = bind_rows(D1,D2,D3)#,D4,D5)#, D_lira)
# unique(D_a_lot_of_comparisons$sim)
# str(D_total)

# D_a_lot_of_comparisons = D_total
# write_csv(D_a_lot_of_comparisons, path = "emeregency_comparisons_dataset.csv")
#write_csv(D_total_test, path = "emergency_comparisons_dataset4.csv")
# D_a_lot_of_comparisons %>% select(sim) %>% unique()
# D_temp = D_a_lot_of_comparisons %>% 
#   filter(sim %in% c("lirau",
#                     "lira_gauss",
#                     "lira_uniform_mean0_sd1",
#                     "lira_uniform_mean0"))
#D_temp  %>% select(sim) %>% unique()
#D_temp = D_total
D_total_test = bind_rows(D_temp ,D_total, D1,D2,D3,D4,D5,D6)

D_total_test %>% select(sim) %>% unique()

D_total_test %>% 
  group_by(K, sim) %>% 
  #filter(sim != "lira_uniform_scale_by_max", sim != "lirau") %>% 
  filter(sim %in% c("lira_gauss",
                    #"lira_uniform_mean0", 
                    #"lira_gauss_mean0",  
                    "cosine",
                    #"lira_gauss_pos_only", 
                    #"lira_gaussian_normalized_mu1", 
                    #"lira_gaussian_normalized",
                    #"lira_gauss_mean0_sd1", # the good one
                    #"lira_uniform_mean0_sd1", 
                    #"lira_uniform_mean0_sd2",
                    #"lira_uniform_mean0_sd0.5",
                    "lira_gauss_mean0_sd1", 
                    "lira_gauss_mean0_sd2",
                    #"lira_gauss_mean0_sd0.5",
                    #"lira_gauss_mean0_sd2.5",
                    #"lira_gauss_quantile0.15",
                    #"lira_uniform_pos_only_mean0_sd1",
                    "lirau")) %>% 
  #filter(sim %in% c("cosine","lira_uniform_mean0","lira_uniform_mean0_sd1","lira_gauss_mean1", "lirau")) %>% 
    summarize(MAE = mean(ae_knn, na.rm = T)) %>%
  ggplot(aes(x = K, y = MAE, group = `sim`, color = `sim`)) + geom_line() + geom_point()





bind_rows(D_total %>% filter(sim == "lirau"),D1, D2, D3, D4, D5, D6) %>% 
  group_by(K, sim) %>% 
  # filter(sim %in% c("lira_gauss_quantile0.65_mu0_sd2",
  #                   "lira_gauss_pos_only_quantile0.65_mu0_sd2")) %>%
  summarize(MAE = mean(ae_knn, na.rm = T), sim_sd = sd(ae_knn, na.rm = T)) %>% 
  ggplot(aes(x = K, y = MAE, group = `sim`, color = `sim`)) + geom_line() + geom_point()


bind_rows(D_total, D_total_test %>% filter(sim %in% c("cosine","lirau"))) %>% 
  group_by(K, sim) %>% 
  summarize(MAE = mean(ae_knn, na.rm = T), sim_sd = sd(ae_knn, na.rm = T)) %>% 
  ggplot(aes(x = K, y = MAE, group = `sim`, color = `sim`)) + geom_line() + geom_point() + ggtitle(label = "LiRa - LRT Variants")





D_total_test
D_total_test %>% 
  filter(sim %in% c("lira_gauss",
                    "cosine",
                    "lira_gauss_mean0_sd1", 
                    "lira_gauss_mean0_sd2",
                    "lira_gauss_mean0_sd2.5",
                    "lirau")) %>%
  group_by(K, sim) %>% 
  summarize(MAE = mean(ae_knn, na.rm = T), sim_sd = sd(ae_knn, na.rm = T)) %>% 
  ggplot(aes(x = K, y = sim_sd, group = `sim`, color = `sim`)) + geom_line() + geom_point() + ggtitle(label = "SD against K value")
send_current_ggplot()



sim_matrix %>% #filter(sim == "lira_uniform_mean0_sd") %>%
  #select(K, k_current) %>% 
  group_by(K, sim) %>% 
  summarize(mean_k = mean(k_current), sd_k = sd(k_current)) %>%
  ggplot(aes(x = K, y = mean_k, col = `sim`)) + 
  geom_line() + geom_point() + ggtitle(label = "Average Actual Used K against Prescribed K") #+
  geom_point(aes(x = K, y = mean_k + sd_k), col = 'red') #+ 
  geom_point(aes(x = K, y = mean_k - sd_k), col = 'red')

###################################
