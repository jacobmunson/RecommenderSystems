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
#D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
#D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
#head(D) # visual check
#dim(D) # how many? 
#head(D)
#D = as_tibble(D)


D <- read_csv("~/Documents/ml-latest-small/ml-latest-small/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")

lira_same_cluster_pdf = lira_same_cluster_distribution(V = sort(unique(D_train$rating))) 
lira_pure_chance_pdf = lira_pure_chance_distribution(V = sort(unique(D_train$rating))) 
lira_binary_same_cluster_pdf = lira_binary_same_cluster_distribution()
lira_binary_pure_chance_pdf = lira_binary_pure_chance_distribution()

train_index = sample(x = 1:nrow(D), size = 0.8*nrow(D), replace = F)
D_train = D[train_index,]
D_test = D[-train_index,]

set.seed(2)

nrow(D_test)
nrow(D_train)



## Splitting the dataset into smaller peices for parallel processing
chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}
dim(D_test)
num_shards = detectCores()*10
shards = chunk(vector = seq(1:nrow(D_test)), num_splits = num_shards)
names(shards) = seq(1,num_shards,1)

dim(D_test)
rm(sim_matrix); gc();

## Estimating variance for Gaussian LiRa Similarity
iter = 1500
diff_vector = c()
for(i in 1:iter){
  user_pair = sample(x = unique(D_train$user), size = 2, replace = FALSE)
  user_pair_data = D_train %>% filter(user == user_pair[1] | user == user_pair[2])
  user_pair_matrix = dcast(data = user_pair_data, formula = user~item, value.var = "rating")
  user_pair_matrix = user_pair_matrix[,-1]
  diff_vector = c(diff_vector,as.numeric(user_pair_matrix[1,] - user_pair_matrix[2,]))
  
}
sd_pop = sd((diff_vector), na.rm = TRUE); print(sd_pop)
round(mean(diff_vector, na.rm = TRUE),2) # should be close to 0
rm(diff_vector) # pretty long, so let's remove it

## Setting k for kNN 
# To-do: make this a vector and process all smaller k at same time
#K_global = 3  # 3,5,7,10,15,20,30,40,50,60,80,160
K = c(3,5,7,10,15,20,30,40,50,60,80,160)

sd_pc = 2

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
      

      ## LiRa Uniform Similarity
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_uniform_mean0", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, test_observation = D_test_i, 
                                              similarity_measure = "lira_uniform")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau, 
                                                      mean_scaling = T, mu_scale = 0)
        
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
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_uniform_mean0_sd1", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "lira_uniform")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau, 
                                                      mean_scaling = T, sd_scaling = T, mu_scale = 0, sd_scale = 1)
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          k_current = min(length(B_lirau), K[k])
          #k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          k_error_df_lirau[k,"k_current"] = k_current
          #B_lirau = B_lirau[1:k_current]
          
          pred_lirau = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau[1:k_current])
          
          k_error_df_lirau[k,"ae_nn"] = pred_lirau[1]
          k_error_df_lirau[k,"ae_knn"] = pred_lirau[2]
        }
      }
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_uniform_pos_only", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "lira_uniform")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau, positive_only = T)
        
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
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_uniform_scale_by_max", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "lira_uniform")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau, scale_similarity_by_max = T)
        
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
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_uniform_normalized", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "lira_uniform")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau, normalize_similarity = T)
        
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
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_uniform_normalized_mu1", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "lira_uniform")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau, 
                                                      normalize_similarity = T, mean_scaling = T, mu_scale = 1)
        
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
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gauss", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "lira_gaussian_pure_chance")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau)
        
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
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gauss_mean1", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "lira_gaussian_pure_chance")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau, mean_scaling = T, mu_scale = 1)
        
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
      
      ## Cosine
      k_error_df_cosine = data.frame(K, k_current = NA, sim = "cosine", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = compute_neighbor_similarity(user_item_matrix = B, 
                                              test_observation = D_test_i, 
                                              similarity_measure = "cosine")
        B_lirau = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = B_lirau)

        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          k_current = min(length(B_lirau), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          k_error_df_cosine[k,"k_current"] = k_current
          #B_lirau = B_lirau[1:k_current]
          
          pred_lirau = prediction_evaluation_function(train_set = D_train, 
                                                      test_set = D_test_i, 
                                                      similarity_vector = B_lirau[1:k_current])
          k_error_df_cosine[k,"ae_nn"] = pred_lirau[1]
          k_error_df_cosine[k,"ae_knn"] = pred_lirau[2]
        }
      }
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_cosine)
      
      
    }
    
    
  }
  
  k_error_df_total
  
}

stopCluster(cl);# dim(M)
end = Sys.time()
end - start # 1.02

dim(sim_matrix)
head(sim_matrix, n = 15) # visual inspection
stopifnot(nrow(sim_matrix) == nrow(D_test)*length(K))

sim_matrix %>% group_by(sim, K) %>% summarize(MAE_nn = mean(ae_nn, na.rm = T)) %>% print(n = 100)#%>% filter(sim == "pc_iz") #
sim_matrix %>% select(k_current) %>% unique()

sim_matrix %>% 
  group_by(sim, K) %>% filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1)


sim_matrix %>% 
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% print(n = 100)


D_ml_latest_1 = sim_matrix # sd_pc = 2
D_ml_latest_2 = sim_matrix # sd_pc = 2




# D1 = sim_matrix
# D2 = sim_matrix
# D3 = sim_matrix
# D4 = sim_matrix
# D5 = sim_matrix

#D_total = bind_rows(D_total_emergency, D1,D2,D3,D4,D5)

str(D_total)
#D_total_emergency

D_total %>% 
  group_by(K, sim) %>% 
  filter(sim != "lira_bin", sim != "lirau") %>% 
  summarize(MAE = mean(ae_knn, na.rm = T)) %>%
  ggplot(aes(x = K, y = MAE, group = `sim`, color = `sim`)) + geom_line() + geom_point()

###################################

data.frame(similarity = c("Cosine Similarity,","Pearson Correlation - PWC,", "Pearson Correlation - IZ,", 
                          "LiRa - Uniform,","LiRa - Gaussian,","LiRa - Gaussian1,",
                          "LiRa - Binary,", "LiRa - Multinomial,",
                          "Cosine Similarity,","Pearson Correlation - PWC,", "Pearson Correlation - IZ,", 
                          "LiRa - Uniform,","LiRa - Gaussian,","LiRa - Gaussian1,",
                          "LiRa - Binary,", "LiRa - Multinomial,"),
           method = c(rep("NN-RST,",8),rep("kNN,",8)),
           k = rep(paste0(K_global,","),16),
           mean = paste0(round(colMeans(sim_matrix, na.rm = TRUE),7),","), 
           adj_mean = paste0(round(colSums(sim_matrix, na.rm = T)/nrow(sim_matrix),7),","), 
           pct_na = rbind(round(length(which(is.na(sim_matrix[,"mae_cs_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_pwc_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_iz_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lu_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_log_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lu_bin_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lmn_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_cs_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_pwc_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_iz_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lu_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_log_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lu_bin_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lmn_knn"])))/nrow(sim_matrix),7))
)

data.frame(similarity = c("Cosine Similarity,","Pearson Correlation - PWC,", "Pearson Correlation - IZ,", "LiRa - Uniform,","LiRa - Gaussian,","LiRa - LogGaussian,","LiRa - Binary", "LiRa - Multinomial,",
                          "Cosine Similarity,","Pearson Correlation - PWC,", "Pearson Correlation - IZ,", "LiRa - Uniform,","LiRa - Gaussian,","LiRa - LogGaussian,","LiRa - Binary", "LiRa - Multinomial,"),
           method = c(rep("NN-RST,",7),rep("kNN,",7)),
           k = rep(paste0(K_global,","),14),
           mean = paste0(round(colMeans(sim_matrix, na.rm = TRUE),7),","), 
           adj_mean = paste0(round(colSums(sim_matrix, na.rm = T)/nrow(sim_matrix),7),","))
           #pct_na = rbind(round(length(which(is.na(sim_matrix[,"mae_cs_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_pwc_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_iz_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lu_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_log_nn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_cs_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_pwc_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_pc_iz_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lu_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_knn"])))/nrow(sim_matrix),7),
                          round(length(which(is.na(sim_matrix[,"mae_lg_log_knn"])))/nrow(sim_matrix),7))
           

