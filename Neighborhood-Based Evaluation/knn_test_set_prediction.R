##################################
### kNN Evaluation on Test Set ###
##################################

# Data Set & Formatting
D = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
dim(D)
head(D)

# Test Set
set.seed(10)
test = sample(x = seq(1,nrow(D),1), size = 0.01*nrow(D), replace = FALSE) #33098 - 50000 /  91924 - 100000

# Test/Train Split
D_test = D[test,]
D_train = D[-test,]
nrow(D_test) + nrow(D_train) == nrow(D) # should be true
dim(D_test)

# Empty Vectors for MAE Evaluation
mae_pc = c() 
mae_cs = c()
mae_lu = c()

K_global = 3 # the 'k' in kNN

# Run Predictions
start = Sys.time()
for(i in 1:nrow(D_test)){
  print(i)
  #i = 89
  D_test_i = D_test[i,]
  potential_coraters = D_train[which(D_train$item == D_test_i$item),"user"]
  user_group = c(D_test_i$user,potential_coraters$user)
  A = subset(D_train, D_train$user %in% user_group)

  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[order(B$user),]
  rownames(B) = B$user
  B = as.matrix(B[,-1])


  # Pearson Correlation
  B_pc = cor(B, use = "pairwise.complete.obs")
  B_pc = B_pc[which(rownames(B_pc) == D_test_i$user),]
  B_pc = B_pc[order(B_pc, decreasing = TRUE)]
  B_pc = B_pc[B_pc >= 0 & !is.na(B_pc)]
  B_pc = B_pc[-1]
  K = min(length(B_pc),K_global)
  B_pc = B_pc[1:K]
  
  if(is.na(B_pc) & length(B_pc) == 1){mae_pc[i] = NA}else{
      neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_pc)),]
  neighbor_ratings = merge(neighbor_ratings[c("user","rating")], B_pc, by.x = "user", by.y = "row.names")
  pred_rating_pc = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
  mae_pc[i] = abs(pred_rating_pc - D_test_i$rating)
  }
  #D_train[which(D_train$user %in% names(B_sim) & D_train$item == item),]
  #D_train %>% filter(item == D_test_i$item, user %in% user_group) %>% print(n = 100)
  
  # Cosine Similarity
  B[is.na(B)] = 0
  B_cs = cosine_similarity(matrix = B)
  B_cs[is.nan(B_cs)] = 0
  B_cs = B_cs[which(rownames(B_cs) == D_test_i$user),]
  B_cs = B_cs[order(B_cs, decreasing = TRUE)]
  B_cs = B_cs[B_cs >= 0 & !is.na(B_cs)]
  B_cs = B_cs[-1]
  K = min(length(B_cs),K_global)
  B_cs = B_cs[1:K]
  
  if(any(is.na(B_cs)) & length(B_cs) == 1){mae_cs[i] = NA}else{
    neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_cs)),]
    neighbor_ratings = merge(neighbor_ratings[c("user","rating")], B_cs, by.x = "user", by.y = "row.names")
    pred_rating_cs = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
    mae_cs[i] = abs(pred_rating_cs - D_test_i$rating)
  }
  
  # LiRa Uniform Similarity
  
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[order(B$user),]
  rownames(B) = B$user
  B = as.matrix(B[,-1])
  
  B_lirau = lapply(1:nrow(B), FUN = function(i){lira(x_u = B[which(rownames(B) == D_test_i$user),], x_v = B[i,], num_ratings = length(unique(D_test$rating)))})
  B_lirau = do.call(cbind.data.frame, B_lirau)
  colnames(B_lirau) = rownames(B)
  B_lirau = as.matrix(B_lirau[order(B_lirau, decreasing = TRUE)])
  
  B_lirau = B_lirau[-1]
  K = min(length(B_lirau),K_global)

  B_lirau = B_lirau[1:K]
  
  if(any(is.na(B_lirau)) & length(B_lirau) == 1){mae_lu[i] = NA}else{
    neighbor_ratings = D_train[which(D_train$item == D_test_i$item & D_train$user %in% names(B_cs)),]
    neighbor_ratings = merge(neighbor_ratings[c("user","rating")], B_cs, by.x = "user", by.y = "row.names")
    pred_rating_lu = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(neighbor_ratings$y)
    mae_lu[i] = abs(pred_rating_lu - D_test_i$rating)
  }

}
end = Sys.time()
print(end - start)
mean(mae_pc, na.rm = TRUE) # 0.975, 0.975,
mean(mae_cs, na.rm = TRUE) # 
mean(mae_lu, na.rm = TRUE)

length(which(is.na(mae_pc)))/length(mae_pc)

