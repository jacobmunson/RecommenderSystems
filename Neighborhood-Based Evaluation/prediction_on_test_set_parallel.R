###########################

## Relevant Packages
library(dplyr)
library(readr)
library(reshape2)
library(doParallel)
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

source('~/Documents/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')
  
cosine_similarity = function(matrix){
  cos_sim = matrix/sqrt(rowSums(matrix * matrix))
  cos_sim = cos_sim %*% t(cos_sim)
  return(cos_sim)
}

lira = function(x_u, x_v, num_ratings){
  num_diff = length(which(!is.na(abs(x_u - x_v))))
  lira_bottom = (1/num_ratings)^num_diff
  lira_top = 0.5^(num_diff)
  lira = log10(lira_top/lira_bottom)
  return(lira)
}
  
  
D <- read_csv("Documents/ml-latest-small/ml-latest-small/ratings.csv")
#D = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
#D = read_csv("Recommender Systems - Home Folder/ml-20m/ratings.csv")
#D <- read.csv("C:/Users/Jacob/Downloads/ml-20m/ml-20m/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
dim(D)
head(D)
#D_total = D
set.seed(1)
test = sample(x = seq(1,nrow(D),1), size = 0.20*nrow(D), replace = FALSE) #33098 - 50000 /  91924 - 100000

D_test = D[test,]
D_train = D[-test,]
nrow(D_test) + nrow(D_train) == nrow(D)

chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}
dim(D_test)
num_shards = detectCores()*10
shards = chunk(vector = seq(1:nrow(D_test)), num_splits = num_shards)
names(shards) = seq(1,num_shards,1)

dim(D_test)
rm(sim_matrix); gc();


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

K_global = 3 #, 40, 30, 20, 15, 10, 7, 5, 3)

rm(sim_matrix);gc()
start = Sys.time()

num_cores = detectCores(); #K = 15
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
  mae_pc_nn = c(); mae_pc_knn = c()
  mae_lg_nn = c(); mae_lg_knn = c()
  mae_lg_log_nn = c(); mae_lg_log_knn = c()
  
  #print(i)
  #i = 1
  D_subset =  D_test[shards[[i]],]
  #dim(D_subset)
  #microbenchmark(D_subset =  D_test[shards[[i]],])
  #microbenchmark(D_subset = D_test %>% slice(shards[[i]]))
  #for(k in 1:length(K_global)){
  
    for(j in 1:nrow(D_subset)){
      #j = 1
      ##print(j)
      D_test_i = D_subset[j,]
      
      #microbenchmark(D_test_i = D_subset[j,])
      #microbenchmark(D_test_i = D_subset %>% slice(j))
      
      
      #dim(D_test_i)
      potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user)
      #potential_coraters = D_train[which(D_train$item == D_test_i$item),"user"]
      
      
      #user_group = c(D_test_i$user,potential_coraters$user) # can probably put this in expression below so not saving it
      
      
      # microbenchmark(potential_coraters = D_train[which(D_train$item == D_test_i$item),"user"])
      # microbenchmark(potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user))
      # microbenchmark(list(potential_coraters = D_train %>% filter(D_train$item == D_test_i$item), potential_coraters = potential_coraters$user))
      # #dim(potential_coraters)
      
      
      # microbenchmark(A = subset(D_train, D_train$user %in% user_group))
      # microbenchmark(A = D_train %>% filter(D_train$user %in% user_group))
      
      # A = subset(D_train, D_train$user %in% user_group)
      # dim(A)
      #A = D_train %>% filter(D_train$user %in% user_group)
      
      A = D_train %>% filter(D_train$user %in% c(D_test_i$user,potential_coraters$user))
      B = dcast(data = A, formula = user~item, value.var = "rating")
      
      B = B[order(B$user),]
      # microbenchmark(B = B[order(B$user),])
      # microbenchmark(B = B %>% arrange(user))
      # 
      #B[1:5,1:5]
      # 
      rownames(B) = B$user
      B = as.matrix(B[,-1]) # unaltered from here on out
      
      ## Pearson Correlation
      B_pc = cor(t(B), use = "pairwise.complete.obs")
      
      B_pc = B_pc[which(rownames(B_pc) == D_test_i$user),] # dplyr with arrange to cover step below and filter for below that
      
      B_pc = B_pc[order(B_pc, decreasing = TRUE)]
      B_pc = B_pc[B_pc >= 0 & !is.na(B_pc)]
      B_pc = B_pc[-1]
      K = min(length(B_pc),K_global)
      B_pc = B_pc[1:K]
      
      if(any(is.na(B_pc)) & length(B_pc) == 1){mae_pc_nn[j] = NA; mae_pc_knn[j] = NA}else{
        
        neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_pc)),] # filter?
        # neighbor_ratings = D_train %>% filter(D_train$item == D_test_i$item & D_train$user %in% names(B_pc)) # filter?
        
        # microbenchmark(neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_pc)),])
        # microbenchmark(neighbor_ratings = D_train %>% filter(D_train$item == D_test_i$item & D_train$user %in% names(B_pc)))
        # 
        
        
        #microbenchmark(neighbor_ratings = inner_join(neighbor_ratings[c("user","rating")], as_tibble(data.frame(user = as.integer(names(B_pc)),B_pc)), by = "user"))
        neighbor_ratings = merge(neighbor_ratings[c("user","rating")], B_pc, by.x = "user", by.y = "row.names")
        
        pred_rating_pc_nn = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
        pred_rating_pc_knn = mean(neighbor_ratings$rating)
        
        mae_pc_nn[j] = abs(pred_rating_pc_nn - D_test_i$rating)
        mae_pc_knn[j] = abs(pred_rating_pc_knn - D_test_i$rating)
      }
      #D_train[which(D_train$user %in% names(B_sim) & D_train$item == item),]
      #D_train %>% filter(item == D_test_i$item, user %in% user_group) %>% print(n = 100)
      
      ## Cosine Similarity
      B_temp = B
      B_temp[is.na(B_temp)] = 0
      B_cs = cosine_similarity(matrix = B_temp)
      B_cs[is.nan(B_cs)] = 0
      B_cs = B_cs[which(rownames(B_cs) == D_test_i$user),]
      B_cs = B_cs[order(B_cs, decreasing = TRUE)]
      B_cs = B_cs[B_cs >= 0 & !is.na(B_cs)]
      B_cs = B_cs[-1]
      K = min(length(B_cs),K_global)
      B_cs = B_cs[1:K]
      
      if(any(is.na(B_cs)) & length(B_cs) == 1){mae_cs_knn[j] = NA; mae_cs_nn[j] = NA}else{
        neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_cs)),]
        neighbor_ratings = merge(neighbor_ratings[c("user","rating")], B_cs, by.x = "user", by.y = "row.names")
        pred_rating_cs_nn = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
        mae_cs_nn[j] = abs(pred_rating_cs_nn - D_test_i$rating)
        pred_rating_cs_knn = mean(neighbor_ratings$rating)
        mae_cs_knn[j] = abs(pred_rating_cs_knn - D_test_i$rating)
      }
      
      ## LiRa Uniform Similarity
      
      # B = dcast(data = A, formula = user~item, value.var = "rating")
      # B = B[order(B$user),]
      # rownames(B) = B$user
      # B = as.matrix(B[,-1]) # recycle this
      
      B_lirau = lapply(1:nrow(B), FUN = function(j){lira(x_u = B[which(rownames(B) == D_test_i$user),], 
                                                         x_v = B[j,], 
                                                         num_ratings = length(unique(D_test$rating)))})
      B_lirau = do.call(cbind.data.frame, B_lirau)
      colnames(B_lirau) = rownames(B)
      #colnames(B_lirau) = "lirau"
      B_lirau = as.matrix.data.frame(B_lirau[order(B_lirau, decreasing = TRUE)])
      
      B_lirau = B_lirau[,-1]
      #B_lirau = B_lirau[!is.na(B_lirau)]
      B_lirau = B_lirau[is.finite(B_lirau)]
      
      
      K = min(length(B_lirau),K_global)
      
      B_lirau = B_lirau[1:K]
      
      if(any(is.na(B_lirau)) & length(B_lirau) == 1){mae_lu_nn[j] = NA; mae_lu_knn[j] = NA}else{
        neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_lirau)),]
        neighbor_ratings = merge(neighbor_ratings[c("user","rating")], (B_lirau), by.x = "user", by.y = "row.names")
        
        pred_rating_lu_nn = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
        mae_lu_nn[j] = abs(pred_rating_lu_nn - D_test_i$rating)
        
        pred_rating_lu_knn = mean(neighbor_ratings$rating)
        mae_lu_knn[j] = abs(pred_rating_lu_knn - D_test_i$rating)
      }
      
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
        
        #B_lira_gauss = B_lira_gauss[!is.na(B_lira_gauss)]
        
        K = min(length(B_lira_gauss),K_global)
        B_lira_gauss = B_lira_gauss[1:K]
        
        #D_train[which(D_train$user == names(B_lira_gauss)),]
        #D_train %>% filter(item == D_test$item, user == names(B_lira_gauss))
        
        if(any(is.na(B_lira_gauss)) & length(B_lira_gauss) == 1){mae_lg_knn[j] = NA; mae_lg_nn[j] = NA}else{
          neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_lira_gauss)),]
          neighbor_ratings = merge(neighbor_ratings[c("user","rating")], B_lira_gauss, by.x = "user", by.y = "row.names")
          
          pred_rating_lg_nn = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
          mae_lg_nn[j] = abs(pred_rating_lg_nn - D_test_i$rating)
          
          pred_rating_lg_knn = mean(neighbor_ratings$rating)
          mae_lg_knn[j] = abs(pred_rating_lg_knn - D_test_i$rating)
          
        }
        
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
        
        #B_lira_gauss = B_lira_gauss[-which(B_lira_gauss == Inf)]
        
        #B_lira_gauss_log = B_lira_gauss_log[!is.na(B_lira_gauss_log)]
        
        K = min(length(B_lira_gauss_log),K_global)
        B_lira_gauss_log = B_lira_gauss_log[1:K]
        
        #D_train[which(D_train$user == names(B_lira_gauss)),]
        #D_train %>% filter(item == D_test$item, user == names(B_lira_gauss))
        
        if(any(is.na(B_lira_gauss_log)) & length(B_lira_gauss_log) == 1){mae_lg_log_nn[j] = NA; mae_lg_log_knn[j] = NA}else{
          neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_lira_gauss_log)),]
          neighbor_ratings = merge(neighbor_ratings[c("user","rating")], B_lira_gauss_log, by.x = "user", by.y = "row.names")
          
          pred_rating_lg_log_nn = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
          mae_lg_log_nn[j] = abs(pred_rating_lg_log_nn - D_test_i$rating)
          
          pred_rating_lg_log_knn = mean(neighbor_ratings$rating)
          mae_lg_log_knn[j] = abs(pred_rating_lg_log_knn - D_test_i$rating)
        }
        
      }else{mae_lg_log_nn[j] = NA; mae_lg_log_knn[j] = NA}
      
      
      
      
    }
    
    sim = cbind(mae_cs_nn, mae_pc_nn, mae_lu_nn, mae_lg_nn, mae_lg_log_nn,
                mae_cs_knn, mae_pc_knn, mae_lu_knn, mae_lg_knn, mae_lg_log_knn)
    sim    
    
  #}
  
}
  
stopCluster(cl);# dim(M)
end = Sys.time()
end - start


head(sim_matrix) # visual inspection
nrow(sim_matrix) == nrow(D_test)

data.frame(mean = colMeans(sim_matrix, na.rm = TRUE), 
           adj_mean = colSums(sim_matrix, na.rm = T)/nrow(sim_matrix), 
           pct_na = rbind(length(which(is.na(sim_matrix[,"mae_cs_nn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_pc_nn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_lu_nn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_lg_nn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_lg_log_nn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_cs_knn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_pc_knn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_lu_knn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_lg_knn"])))/nrow(sim_matrix),
                          length(which(is.na(sim_matrix[,"mae_lg_log_knn"])))/nrow(sim_matrix))
)




