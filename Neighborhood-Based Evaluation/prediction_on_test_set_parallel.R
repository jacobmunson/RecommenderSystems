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

 
  
D <- read_csv("Documents/ml-latest-small/ml-latest-small/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
dim(D)
head(D)

set.seed(2)

test = sample(x = seq(1,nrow(D),1), size = 0.20*nrow(D), replace = FALSE) #33098 - 50000 /  91924 - 100000
D_test = D[test,]
D_train = D[-test,]
nrow(D_test) + nrow(D_train) == nrow(D)

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
v_pop = var(diff_vector, na.rm = TRUE); print(v_pop)
round(mean(diff_vector, na.rm = TRUE),2) # should be close to 0
rm(diff_vector) # pretty long, so let's remove it

## Setting k for kNN 
# To-do: make this a vector and process all smaller k at same time
K_global = 10 #, 40, 30, 20, 15, 10, 7, 5, 3)


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
  
  mae_lu_nn = c(); mae_lu_knn = c()
  mae_cs_nn = c(); mae_cs_knn = c()
  mae_pc_pwc_nn = c(); mae_pc_pwc_knn = c()
  mae_pc_iz_nn = c(); mae_pc_iz_knn = c()
  mae_lg_nn = c(); mae_lg_knn = c()
  mae_lg_log_nn = c(); mae_lg_log_knn = c()
  
  #print(i)
  #i = 1
  D_subset =  D_test[shards[[i]],]
  
  for(j in 1:nrow(D_subset)){
    #j = 1
    #print(j)
    D_test_i = D_subset[j,]
    
    potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user)
    
    A = D_train %>% filter(D_train$user %in% c(D_test_i$user,potential_coraters$user))
    B = dcast(data = A, formula = user~item, value.var = "rating")
    B = B[order(B$user),]
    rownames(B) = B$user
    B = as.matrix(B[,-1]) # unaltered from here on out
    
    ## Pearson Correlation - Pairwise Complete
    B_pc_pwc = cor(t(B), use = "pairwise.complete.obs")
    B_pc_pwc = B_pc_pwc[which(rownames(B_pc_pwc) == D_test_i$user),] # dplyr with arrange to cover step below and filter for below that
    B_pc_pwc = B_pc_pwc[order(B_pc_pwc, decreasing = TRUE)]
    B_pc_pwc = B_pc_pwc[!is.na(B_pc_pwc)]
    B_pc_pwc = B_pc_pwc[-1]
    K = min(length(B_pc_pwc),K_global)
    B_pc_pwc = B_pc_pwc[1:K]
    
    pred_pc_pwc = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_pc_pwc)
    mae_pc_pwc_nn[j] = pred_pc_pwc[1]; 
    mae_pc_pwc_knn[j] = pred_pc_pwc[2];
    
    ####### Prediction f()
    
    
    ## Pearson Correlation - Impute Zero
    B_temp = B
    B_temp[is.na(B_temp)] = 0
    B_pc_iz = cor(t(B_temp))
    B_pc_iz = B_pc_iz[which(rownames(B_pc_iz) == D_test_i$user),] # dplyr with arrange to cover step below and filter for below that
    B_pc_iz = B_pc_iz[order(B_pc_iz, decreasing = TRUE)]
    B_pc_iz = B_pc_iz[!is.na(B_pc_iz)]
    B_pc_iz = B_pc_iz[-1]
    
    K = min(length(B_pc_iz),K_global)
    B_pc_iz = B_pc_iz[1:K]
    
    pred_pc_iz = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_pc_iz)
    mae_pc_iz_nn[j] = pred_pc_iz[1]; 
    mae_pc_iz_knn[j] = pred_pc_iz[2];
    
    
    ####### Prediction f()
    
    
    ## Cosine Similarity
    B_temp = B
    B_temp[is.na(B_temp)] = 0
    B_cs = cosine_similarity(matrix = B_temp)
    B_cs[is.nan(B_cs)] = 0
    B_cs = B_cs[which(rownames(B_cs) == D_test_i$user),]
    B_cs = B_cs[order(B_cs, decreasing = TRUE)]
    #B_cs = B_cs[B_cs >= 0 & !is.na(B_cs)]
    B_cs = B_cs[!is.na(B_cs)]
    B_cs = B_cs[-1]
    K = min(length(B_cs),K_global)
    B_cs = B_cs[1:K]
    
    pred_cs = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_cs)
    mae_cs_nn[j] = pred_cs[1]; 
    mae_cs_knn[j] = pred_cs[2];
    
    ##### Prediction f()
    
    
    ## LiRa Uniform Similarity
    
    B_lirau = lapply(1:nrow(B), FUN = function(j){lira(x_u = B[which(rownames(B) == D_test_i$user),], 
                                                       x_v = B[j,], 
                                                       num_ratings = length(unique(D_test$rating)))}) #length(unique(D_test$rating))
    B_lirau = do.call(cbind.data.frame, B_lirau)
    colnames(B_lirau) = rownames(B)
    B_lirau = as.matrix.data.frame(B_lirau[order(B_lirau, decreasing = TRUE)])
    B_lirau = B_lirau[,-1]
    B_lirau = B_lirau[is.finite(B_lirau)]
    K = min(length(B_lirau),K_global)
    B_lirau = B_lirau[1:K]
    
    pred_lu = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lirau)
    mae_lu_nn[j] = pred_lu[1]; 
    mae_lu_knn[j] = pred_lu[2];
    #if(any(is.na(B_lirau)) & length(B_lirau) == 1){mae_lu_nn[j] = NA; mae_lu_knn[j] = NA}else{
    
    ##### Prediction f()
    
    
    ## LiRa Gaussian Clusters
    
    # B = dcast(data = A, formula = user~item, value.var = "rating")
    # B = B[order(B$user),]
    # rownames(B) = B$user
    # B = as.matrix(B[,-1])
    
    if(nrow(B) > 1){
      
      B_lira_gauss = lapply(1:nrow(B), 
                            FUN = function(i){x_u = B[which(rownames(B) == D_test_i$user),];
                            x_v = B[i,];
                            (diff = x_u - x_v);
                            n = length(diff);v = var(diff, na.rm = TRUE);
                            #sqrt(n/2)*(v/v_pop - 1 - log(v/v_pop))
                            (v_pop/v)
                            })
      
      B_lira_gauss = do.call(cbind.data.frame, B_lira_gauss)
      colnames(B_lira_gauss) = rownames(B)
      B_lira_gauss = as.matrix(B_lira_gauss[order(B_lira_gauss, decreasing = TRUE)], )
      B_lira_gauss = B_lira_gauss[,-1]
      B_lira_gauss = B_lira_gauss[is.finite(B_lira_gauss)]
      K = min(length(B_lira_gauss),K_global)
      B_lira_gauss = B_lira_gauss[1:K]
      
      pred_lg = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_gauss)
      mae_lg_nn[j] = pred_lg[1]; 
      mae_lg_knn[j] = pred_lg[2];
      
      ######## Prediction f()
      
      
      
    }else{mae_lg_knn[j] = NA; mae_lg_nn[j] = NA}
    
    ## LiRa Gaussian Clusters
    
    # B = dcast(data = A, formula = user~item, value.var = "rating")
    # B = B[order(B$user),]
    # rownames(B) = B$user
    # B = as.matrix(B[,-1])
    
    if(nrow(B) > 1){
      
      B_lira_gauss_log = lapply(1:nrow(B), 
                                FUN = function(i){x_u = B[which(rownames(B) == D_test_i$user),];
                                x_v = B[i,];
                                (diff = x_u - x_v);
                                n = length(diff);v = var(diff, na.rm = TRUE);
                                #sqrt(n/2)*(v/v_pop - 1 - log(v/v_pop))
                                log(v_pop/v)
                                })
      
      B_lira_gauss_log = do.call(cbind.data.frame, B_lira_gauss_log)
      colnames(B_lira_gauss_log) = rownames(B)
      
      B_lira_gauss_log = as.matrix(B_lira_gauss_log[order(B_lira_gauss_log, decreasing = TRUE)], )
      B_lira_gauss_log = B_lira_gauss_log[,-1]
      B_lira_gauss_log = B_lira_gauss_log[is.finite(B_lira_gauss_log)]
      K = min(length(B_lira_gauss_log),K_global)
      B_lira_gauss_log = B_lira_gauss_log[1:K]
      
      pred_log_lg = prediction_evaluation_function(train_set = D_train, test_set = D_test_i, similarity_vector = B_lira_gauss_log)
      mae_lg_log_nn[j] = pred_log_lg[1]; 
      mae_lg_log_knn[j] = pred_log_lg[2];
      
      ####### Prediction f()
      
      
    }else{mae_lg_log_nn[j] = NA; mae_lg_log_knn[j] = NA}
    
  }
  
  
  sim = cbind(mae_cs_nn, mae_pc_pwc_nn, mae_pc_iz_nn, mae_lu_nn, mae_lg_nn, mae_lg_log_nn,
              mae_cs_knn, mae_pc_pwc_knn, mae_pc_iz_knn, mae_lu_knn, mae_lg_knn, mae_lg_log_knn)
  sim    
  
  #}
  
}

stopCluster(cl);# dim(M)
end = Sys.time()
end - start


head(sim_matrix, n = 15) # visual inspection
nrow(sim_matrix) == nrow(D_test) 


data.frame(similarity = c("Cosine Similarity,","Pearson Correlation - PWC,", "Pearson Correlation - IZ,", "LiRa - Uniform,","LiRa - Gaussian,","LiRa - LogGaussian,",
                          "Cosine Similarity,","Pearson Correlation - PWC,", "Pearson Correlation - IZ,", "LiRa - Uniform,","LiRa - Gaussian,","LiRa - LogGaussian,"),
           method = c(rep("NN-RST,",6),rep("kNN,",6)),
           k = rep(paste0(K_global,","),12),
           mean = paste0(round(colMeans(sim_matrix, na.rm = TRUE),7),","), 
           adj_mean = paste0(round(colSums(sim_matrix, na.rm = T)/nrow(sim_matrix),7),","), 
           pct_na = rbind(round(length(which(is.na(sim_matrix[,"mae_cs_nn"])))/nrow(sim_matrix),7),
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
)




