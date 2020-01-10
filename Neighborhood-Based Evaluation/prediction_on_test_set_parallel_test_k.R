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
#colnames(D) = c("user","item","rating","timestamp")
#head(D)
#D = as_tibble(D)

 
  
# D <- read_csv("Documents/ml-latest-small/ml-latest-small/ratings.csv")
# colnames(D) = c("user","item","rating","timestamp")
# dim(D)
# head(D)

set.seed(2)

test = sample(x = seq(1,nrow(D),1), size = 0.20*nrow(D), replace = FALSE) #33098 - 50000 /  91924 - 100000
D_test = D[test,]
D_train = D[-test,]
nrow(D_test) + nrow(D_train) == nrow(D)

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
  #dim(user_pair_matrix)
  #x = D_train[sample(x = 1:nrow(D_train), size = 2, replace = FALSE),]
  diff_vector = c(diff_vector,as.numeric(user_pair_matrix[1,] - user_pair_matrix[2,]))
  
}
sd_pop = sd((diff_vector), na.rm = TRUE); print(sd_pop)
round(mean(diff_vector, na.rm = TRUE),2) # should be close to 0
rm(diff_vector) # pretty long, so let's remove it

## Setting k for kNN 
# To-do: make this a vector and process all smaller k at same time
#K_global = 3  # 3,5,7,10,15,20,30,40,50,60,80,160
K = c(3,5,7,10,15,20,30,40,50,60,80,160)

# set K = c(3,5,7,10,15,20,30,40,50,60,80,160)
# set K_max = max(K)
# inside of each loop 

sd_pc = 2

## Start Evaluation
rm(sim_matrix);gc() # 
start = Sys.time()

num_cores = detectCores();
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)
cat(format(Sys.time(), "%a %b %d %X %Y"), "\n")

sim_matrix = foreach(i = 1:num_shards, .combine = rbind, .packages = c("dplyr","reshape2")) %dopar% {
  #K_global = c(15)
  # library(dplyr)
  # library(reshape2)
  #library(microbenchmark)
  
  # mae_lu_nn = c(); mae_lu_knn = c()
  # mae_cs_nn = c(); mae_cs_knn = c()
  # mae_pc_pwc_nn = c(); mae_pc_pwc_knn = c()
  # mae_pc_iz_nn = c(); mae_pc_iz_knn = c()
  # mae_lg_nn = c(); mae_lg_knn = c()
  # mae_lg_log_nn = c(); mae_lg_log_knn = c()
  # mae_lu_bin_nn = c(); mae_lu_bin_knn = c()
  # mae_lmn_nn = c(); mae_lmn_knn = c()
  
  k_error_df_total = c()
  
  #print(i)
  #i = 1
  D_subset =  D_test[shards[[i]],]

  for(j in 1:nrow(D_subset)){
    #print(j)
    D_test_i = D_subset[j,]
    
    potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user)
    if(nrow(potential_coraters) > 0){
      A = D_train %>% filter(D_train$user %in% c(D_test_i$user,potential_coraters$user))
      B = dcast(data = A, formula = user~item, value.var = "rating")
      B = B[order(B$user),]
      rownames(B) = B$user
      B = as.matrix(B[,-1]) # unaltered from here on out
      
      ## Pearson Correlation - Pairwise Complete
      B_pc_pwc = cor(t(B), use = "pairwise.complete.obs")
      B_pc_pwc = B_pc_pwc[which(rownames(B_pc_pwc) == D_test_i$user),] # dplyr with arrange to cover step below and filter for below that
      B_pc_pwc = B_pc_pwc[order(B_pc_pwc, decreasing = TRUE)]
      B_pc_pwc = B_pc_pwc[-1]
      
      
      #B_pc_pwc = B_pc_pwc[!is.na(B_pc_pwc)]
      B_pc_pwc = B_pc_pwc[is.finite(B_pc_pwc)] 
      
      
      # after is.finite()
      # now I have all neighbors
      # make data.frame of K and error () - "k_error_df" - still need
      k_error_df_pc_pwc = data.frame(K, k_current = NA, sim = "pc_pwc", ae_nn = NA, ae_knn = NA)
      # columns: k, k_current, ae_nn, ae_knn
      # dimensions: K * 4
      for(k in 1:length(K)){
        k_current = min(length(B_pc_pwc), K[k])
        k_error_df_pc_pwc[k,"k_current"] = k_current
        #B_pc_pwc = B_pc_pwc[1:k_current]
        
        pred_pc_pwc = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_pc_pwc[1:k_current])
        # pred_pc_pwc is one obs from test set
        k_error_df_pc_pwc[k,"ae_nn"] = pred_pc_pwc[1] # to store entries 1 and 2
        k_error_df_pc_pwc[k,"ae_knn"] = pred_pc_pwc[2]
        ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
        ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_pc_pwc)
      # declare k_error_df_total = c() outside of j loop
      
      # K = min(length(B_pc_pwc),K_global)
      # B_pc_pwc = B_pc_pwc[1:K]
      # 
      # pred_pc_pwc = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_pc_pwc)
      # mae_pc_pwc_nn[j] = pred_pc_pwc[1]; 
      # mae_pc_pwc_knn[j] = pred_pc_pwc[2];
      
      ####### Prediction f()
      
      
      ## Pearson Correlation - Impute Zero
      B_temp = B
      B_temp[is.na(B_temp)] = 0
      B_pc_iz = cor(t(B_temp))
      B_pc_iz = B_pc_iz[which(rownames(B_pc_iz) == D_test_i$user),] # dplyr with arrange to cover step below and filter for below that
      B_pc_iz = B_pc_iz[order(B_pc_iz, decreasing = TRUE)]
      #B_pc_iz = B_pc_iz[!is.na(B_pc_iz)]
      
      B_pc_iz = B_pc_iz[-1]
      
      B_pc_iz = B_pc_iz[is.finite(B_pc_iz)]
      #B_pc_iz = B_pc_iz[!is.na(B_pc_iz)]
      
      
      k_error_df_pc_iz = data.frame(K, k_current = NA, sim = "pc_iz", ae_nn = NA, ae_knn = NA)
      # columns: k, k_current, ae_nn, ae_knn
      # dimensions: K * 4
      for(k in 1:length(K)){
        k_current = min(length(B_pc_iz), K[k])
        k_error_df_pc_iz[k,"k_current"] = k_current
        #B_pc_iz = B_pc_iz[1:k_current]
        
        pred_pc_iz = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_pc_iz[1:k_current])
        # pred_pc_pwc is one obs from test set
        k_error_df_pc_iz[k,"ae_nn"] = pred_pc_iz[1] # to store entries 1 and 2
        k_error_df_pc_iz[k,"ae_knn"] = pred_pc_iz[2]
        ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
        ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_pc_iz)
      
      
      # K = min(length(B_pc_iz),K_global)
      # B_pc_iz = B_pc_iz[1:K]
      # 
      # pred_pc_iz = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_pc_iz)
      # mae_pc_iz_nn[j] = pred_pc_iz[1]; 
      # mae_pc_iz_knn[j] = pred_pc_iz[2];
      
      
      ####### Prediction f()
      
      
      ## Cosine Similarity
      B_temp = B
      B_temp[is.na(B_temp)] = 0
      B_cs = cosine_similarity(matrix = B_temp)
      B_cs[is.nan(B_cs)] = 0
      B_cs = B_cs[which(rownames(B_cs) == D_test_i$user),]
      B_cs = B_cs[order(B_cs, decreasing = TRUE)]
      #B_cs = B_cs[B_cs >= 0 & !is.na(B_cs)]
      B_cs = B_cs[-1]
      
      #B_cs = B_cs[!is.na(B_cs)]
      B_cs = B_cs[is.finite(B_cs)]
      
      k_error_df_cs = data.frame(K, k_current = NA, sim = "cs", ae_nn = NA, ae_knn = NA)
      # columns: k, k_current, ae_nn, ae_knn
      # dimensions: K * 4
      for(k in 1:length(K)){
        k_current = min(length(B_cs), K[k])
        k_error_df_cs[k,"k_current"] = k_current
        #B_cs = B_cs[1:k_current]
        
        pred_pc_cs = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_cs[1:k_current])
        # pred_pc_pwc is one obs from test set
        k_error_df_cs[k,"ae_nn"] = pred_pc_cs[1] # to store entries 1 and 2
        k_error_df_cs[k,"ae_knn"] = pred_pc_cs[2]
        ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
        ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_cs)    
      
      # K = min(length(B_cs),K_global)
      # B_cs = B_cs[1:K]
      # #is.finite(Inf)
      # 
      # pred_cs = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_cs)
      # mae_cs_nn[j] = pred_cs[1]; 
      # mae_cs_knn[j] = pred_cs[2];
      # 
      ##### Prediction f()
      
      
      ## LiRa Uniform Similarity
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lirau", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lirau = lapply(1:nrow(B), FUN = function(k){lira(x_u = B[which(rownames(B) == D_test_i$user),], 
                                                           x_v = B[k,], 
                                                           lira_same_cluster_pdf = lira_same_cluster_pdf, 
                                                           lira_pure_chance_pdf = lira_pure_chance_pdf)}) #length(unique(D_test$rating))
        # time test this.
        #library(microbenchmark)
        #B_lirau = do.call(cbind.data.frame, B_lirau)
        #bind
        B_lirau = bind_cols(B_lirau)
        
        colnames(B_lirau) = rownames(B)
        
        #order(B_lirau, decreasing = T)# %>% arrange()
        #arrange(desc(B_lirau))
        B_lirau = as.matrix(B_lirau)
        #B_lirau = as.matrix.data.frame(B_lirau[order(B_lirau, decreasing = TRUE)])
        B_lirau = B_lirau[,order(B_lirau, decreasing = TRUE)]
        
        B_lirau = B_lirau[-1]
        B_lirau = B_lirau[is.finite(B_lirau)]
        
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          k_current = min(length(B_lirau), K[k])
          k_error_df_lirau[k,"k_current"] = k_current
          #B_lirau = B_lirau[1:k_current]
          
          pred_lirau = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau[1:k_current])
          # pred_pc_pwc is one obs from test set
          k_error_df_lirau[k,"ae_nn"] = pred_lirau[1] # to store entries 1 and 2
          k_error_df_lirau[k,"ae_knn"] = pred_lirau[2]
          ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
          ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
          
        }
        
        
        # K = min(length(B_lirau),K_global)
        # B_lirau = B_lirau[1:K]
        # 
        # pred_lu = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau)
        # mae_lu_nn[j] = pred_lu[1]; 
        # mae_lu_knn[j] = pred_lu[2];
        #if(any(is.na(B_lirau)) & length(B_lirau) == 1){mae_lu_nn[j] = NA; mae_lu_knn[j] = NA}else{
        
        ##### Prediction f()
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_lirau)
      
      ## LiRa Multinomial Similarity
      k_error_df_lira_mn = data.frame(K, k_current = NA, sim = "lira_mn", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lira_multi = lapply(1:nrow(B), FUN = function(k){lira_multinomial(x_u = B[which(rownames(B) == D_test_i$user),], 
                                                                            x_v = B[k,],
                                                                            multinomial_pure_chance_pdf = alpha_star, 
                                                                            lira_same_cluster_pdf = lira_same_cluster_pdf)}) 
        
        
        
        # time test this.
        #library(microbenchmark)
        #B_lirau = do.call(cbind.data.frame, B_lirau)
        #bind
        B_lira_multi = bind_cols(B_lira_multi)
        
        colnames(B_lira_multi) = rownames(B)
        
        #order(B_lirau, decreasing = T)# %>% arrange()
        #arrange(desc(B_lirau))
        B_lira_multi = as.matrix(B_lira_multi)
        #B_lira_multi = as.matrix.data.frame(B_lira_multi[order(B_lira_multi, decreasing = TRUE)])
        B_lira_multi = B_lira_multi[,order(B_lira_multi, decreasing = TRUE)]
        
        B_lira_multi = B_lira_multi[-1]
        B_lira_multi = B_lira_multi[is.finite(B_lira_multi)]
        
        for(k in 1:length(K)){
          k_current = min(length(B_lira_multi), K[k])
          k_error_df_lira_mn[k,"k_current"] = k_current
          #B_lira_multi = B_lira_multi[1:k_current]
          
          pred_lira_multi = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_multi[1:k_current])
          # pred_pc_pwc is one obs from test set
          k_error_df_lira_mn[k,"ae_nn"] = pred_lira_multi[1] # to store entries 1 and 2
          k_error_df_lira_mn[k,"ae_knn"] = pred_lira_multi[2]
          ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
          ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
          
        }
        
        
        # K = min(length(B_lira_multi),K_global)
        # B_lira_multi = B_lira_multi[1:K]
        # 
        # pred_lmn = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_multi)
        # mae_lmn_nn[j] = pred_lmn[1]; 
        # mae_lmn_knn[j] = pred_lmn[2];
        #if(any(is.na(B_lirau)) & length(B_lirau) == 1){mae_lu_nn[j] = NA; mae_lu_knn[j] = NA}else{
        
        ##### Prediction f()
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_lira_mn)
      
      ## LiRa LRT
      k_error_df_lira_lrt = data.frame(K, k_current = NA, sim = "lira_lrt", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        
        B_lira_lrt = lapply(1:nrow(B), 
                            FUN = function(k){lira_lrt(x_u = B[which(rownames(B) == D_test_i$user),], 
                                                       x_v = B[k,], sd_pop = sd_pop)}) 
        
        
        B_lira_lrt = bind_cols(B_lira_lrt)
        colnames(B_lira_lrt) = rownames(B)
        B_lira_lrt = as.matrix(B_lira_lrt)
        B_lira_lrt = B_lira_lrt[,order(B_lira_lrt, decreasing = TRUE)]
        B_lira_lrt = B_lira_lrt[-1]
        
        B_lira_lrt = B_lira_lrt[is.finite(B_lira_lrt)]
        #B_lira_gauss = B_lira_gauss[!is.na(B_lira_gauss)]
        
        for(k in 1:length(K)){
          k_current = min(length(B_lira_lrt), K[k])
          k_error_df_lira_lrt[k,"k_current"] = k_current
          #B_lira_lrt = B_lira_lrt[1:k_current]
          
          pred_lira_lrt = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_lrt[1:k_current])
          # pred_pc_pwc is one obs from test set
          k_error_df_lira_lrt[k,"ae_nn"] = pred_lira_lrt[1] # to store entries 1 and 2
          k_error_df_lira_lrt[k,"ae_knn"] = pred_lira_lrt[2]
          ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
          ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
          
        }
        
        
        # K = min(length(B_lira_gauss),K_global)
        # B_lira_gauss = B_lira_gauss[1:K]
        # 
        # pred_lg = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_gauss)
        # mae_lg_nn[j] = pred_lg[1]; 
        # mae_lg_knn[j] = pred_lg[2];
        
        ######## Prediction f()
        
        
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_lira_lrt)
      
      ## LiRa Gaussian Clusters
      k_error_df_lira_gauss = data.frame(K, k_current = NA, sim = "lira_gauss", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        B_lira_gauss = lapply(1:nrow(B), FUN = function(k){lira_gaussian(x_u = B[which(rownames(B) == D_test_i$user),], 
                                                                         x_v = B[k,], 
                                                                         sd_pc = sd_pc, 
                                                                         lira_same_cluster_pdf = lira_same_cluster_pdf)})
        
        
        B_lira_gauss = bind_cols(B_lira_gauss)
        colnames(B_lira_gauss) = rownames(B)
        B_lira_gauss = as.matrix(B_lira_gauss)
        B_lira_gauss = B_lira_gauss[,order(B_lira_gauss, decreasing = TRUE)]
        B_lira_gauss = B_lira_gauss[-1]
        
        
        
        
        B_lira_gauss = B_lira_gauss[is.finite(B_lira_gauss)]
        
        for(k in 1:length(K)){
          k_current = min(length(B_lira_gauss), K[k])
          k_error_df_lira_gauss[k,"k_current"] = k_current
          #B_lira_gauss = B_lira_gauss[1:k_current]
          
          pred_lira_gauss = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_gauss[1:k_current])
          # pred_pc_pwc is one obs from test set
          k_error_df_lira_gauss[k,"ae_nn"] = pred_lira_gauss[1] # to store entries 1 and 2
          k_error_df_lira_gauss[k,"ae_knn"] = pred_lira_gauss[2]
          ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
          ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
          
        }
        
        
        # K = min(length(B_lira_gauss_log),K_global)
        # B_lira_gauss_log = B_lira_gauss_log[1:K]
        # 
        # pred_log_lg = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_gauss_log)
        # mae_lg_log_nn[j] = pred_log_lg[1];
        # mae_lg_log_knn[j] = pred_log_lg[2];
        
        ####### Prediction f()
        
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_lira_gauss)
      
      ## LiRa Uniform Similarity - Binary Data
      k_error_df_lira_bin = data.frame(K, k_current = NA, sim = "lira_bin", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        #BBB = B
        #B[1:5,1:5]
        B[B <= 2 & !is.na(B)] = 0
        B[B > 2] = 1
        B_lirau_bin = lapply(1:nrow(B), FUN = function(k){lira(x_u = B[which(rownames(B) == D_test_i$user),], 
                                                               x_v = B[k,], 
                                                               lira_same_cluster_pdf = lira_binary_same_cluster_pdf, 
                                                               lira_pure_chance_pdf = lira_binary_pure_chance_pdf)}) #length(unique(D_test$rating))
        
        B_lirau_bin = bind_cols(B_lirau_bin)
        
        colnames(B_lirau_bin) = rownames(B)
        
        #order(B_lirau, decreasing = T)# %>% arrange()
        #arrange(desc(B_lirau))
        B_lirau_bin = as.matrix(B_lirau_bin)
        #B_lirau = as.matrix.data.frame(B_lirau[order(B_lirau, decreasing = TRUE)])
        B_lirau_bin = B_lirau_bin[,order(B_lirau_bin, decreasing = TRUE)]
        
        B_lirau_bin = B_lirau_bin[-1]
        B_lirau_bin = B_lirau_bin[is.finite(B_lirau_bin)]
        
        if(D_test_i$rating <=2){D_test_i$rating = 0}else{D_test_i$rating = 1}
        D_train_temp = D_train
        D_train_temp[D_train_temp$rating <= 2,"rating"] = 0
        D_train_temp[D_train_temp$rating > 2,"rating"] = 1
        
        for(k in 1:length(K)){
          k_current = min(length(B_lirau_bin), K[k])
          k_error_df_lira_bin[k,"k_current"] = k_current
          #B_lirau_bin = B_lirau_bin[1:k_current]
          
          pred_lira_bin = prediction_evaluation_function(train_set = D_train_temp, test_set = D_test_i, similarity_vector = B_lirau_bin[1:k_current])
          # pred_pc_pwc is one obs from test set
          k_error_df_lira_bin[k,"ae_nn"] = pred_lira_bin[1] # to store entries 1 and 2
          k_error_df_lira_bin[k,"ae_knn"] = pred_lira_bin[2]
          ###mae_pc_pwc_nn[j] = pred_pc_pwc[1];
          ###mae_pc_pwc_knn[j] = pred_pc_pwc[2];
          
        }
        
        
        # K = min(length(B_lirau_bin),K_global)
        # B_lirau_bin = B_lirau_bin[1:K]
        
        # if(D_test_i$rating <=2){D_test_i$rating = 0}else{D_test_i$rating = 1}
        # D_train_temp = D_train
        # D_train_temp[D_train_temp$rating <= 2,"rating"] = 0
        # D_train_temp[D_train_temp$rating > 2,"rating"] = 1
        # 
        # pred_lu = prediction_evaluation_function(train_set = D_train_temp, test_set = D_test_i, similarity_vector = B_lirau_bin)
        # mae_lu_bin_nn[j] = pred_lu[1]; 
        # mae_lu_bin_knn[j] = pred_lu[2];
        #if(any(is.na(B_lirau)) & length(B_lirau) == 1){mae_lu_nn[j] = NA; mae_lu_knn[j] = NA}else{
        
        ##### Prediction f()
        
      }
      k_error_df_total = rbind(k_error_df_total, k_error_df_lira_bin)
      
    }
    
    
  }
  
  
  # sim = cbind(mae_cs_nn, mae_pc_pwc_nn, mae_pc_iz_nn, mae_lu_nn, mae_lg_nn, mae_lg_log_nn, mae_lu_bin_nn, mae_lmn_nn,
  #             mae_cs_knn, mae_pc_pwc_knn, mae_pc_iz_knn, mae_lu_knn, mae_lg_knn, mae_lg_log_knn, mae_lu_bin_knn, mae_lmn_knn)
  # length(which(is.na(sim)))    # 252
  # length(sim)
  # length(which(is.na(sim)))/length(sim)
  # nrow(sim)
  #sim
  k_error_df_total
  # k_error_df_total
  #}
  
}

stopCluster(cl);# dim(M)
end = Sys.time()
end - start # 1.02
sim_matrix
head(sim_matrix, n = 15) # visual inspection
stopifnot(nrow(sim_matrix) == nrow(D_test)*length(K))

sim_matrix %>% group_by(sim, K) %>% summarize(MAE_nn = mean(ae_nn, na.rm = T)) %>% filter(sim == "pc_iz") #
sim_matrix %>% select(k_current) %>% unique()

sim_matrix %>% 
  group_by(sim, K) %>% filter(sim != "lira_bin") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1)



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
           

