
# under construction
library(readr)
D <- read.csv("C:/Users/Jacob/Downloads/ml-latest-small/ml-latest-small/ratings.csv")
#D <- read.csv("C:/Users/Jacob/Downloads/ml-20m/ml-20m/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
dim(D)
#D_total = D
set.seed(2)
test = sample(x = seq(1,nrow(D),1), size = 0.2*nrow(D), replace = FALSE) #33098 - 50000 /  91924 - 100000


D_test = D[test,]
D_train = D[-test,]
nrow(D_test) + nrow(D_train) == nrow(D)

D = D_train

str(D)
head(D)
length(unique(D$user)) # 910 users 


build_comparison_list = function(dataset){
  
  cat("Start Time:", format(Sys.time(), "%a %b %d %X %Y"))
  C = data.frame(user1 = NULL, user2 = NULL)
  start = Sys.time()
  for(i in 1:length(unique(dataset$user))){
    #i = 2
    items = dataset[which(dataset$user == unique(dataset$user)[i]),"item"]
    coraters_total = c()
    for(j in 1:length(unique(items))){
      #print(j)
      #j = 1
      coraters = dataset[which(dataset$item == items[j]),"user"]
      coraters_total = c(coraters_total, coraters)
    }
    coraters_total = unique(coraters_total)
    
    if(length(coraters_total < unique(dataset$user)[i]) > 0){
      coraters_total = coraters_total[-which(coraters_total <= unique(D$user)[i])]
    }
    
    C1 = expand.grid(user1 = unique(D$user)[i], user2 = coraters_total)
    #colnames(C1) = c("user1","user2")
    #C1_rev = C1[which(C1[,1] == i),c(2:1)]
    #colnames(C1_rev) = c("user1","user2")
    #C = anti_join(C, C1_rev, by = c("user1","user2"))
    
    C = rbind(C,C1)
    cat("User:",i,"/",length(unique(D$user)), "|",
        "Comparisons:",dim(C1)[1],
        "Total Comparisons:",nrow(C),"\n")
  }
  end = Sys.time()
  print(end - start)
  return(C)
  
}

D_similarity = build_comparison_list(dataset = D)


# if(length(which(C[,1] == C[,2])) > 0){
#   C = C[-which(C[,1] == C[,2]),] # only run once
#   dim(C)
#   print("removed entry")
# }
# dim(C) # should no longer be necessary

head(M_comb)
M_comb = cbind(D_similarity, PC = NA)
M_comb = as.matrix(M_comb)
colnames(M_comb) = c("User1","User2","PC")
dim(M_comb)
#M_comb = M_comb[-which(M_comb[,1] == M_comb[,2]),] # should be able to skip
head(M_comb, n = 15)
dim(M_comb)
library(reshape2)

## Filling in Pearson Correlation
library(reshape2)
for(i in 1:nrow(M_comb)){
  A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  #cat("i",i,length(intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))), "overlap", "\n")
  cat("iteration:", i, "/", nrow(M_comb),"\n")
  B_cor = cor(t(B), use = "pairwise.complete.obs")
  if(is.na(B_cor[1,2])){M_comb[i,"PC"] = NA}else{M_comb[i,"PC"] = B_cor[1,2]}
}

head(M_comb, n = 15)

## Filling in Cosine Similarity
M_comb = cbind(M_comb, 0)
colnames(M_comb)[4] = "CS" 
str(M_comb)

for(i in 1:nrow(M_comb)){
  
  #i = 1
  A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  B = B[intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))]
  B_cs = cosine_similarity(as.matrix(B))
  #cat("i",i,length(intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))), "overlap", "\n")
  cat("iteration:", i, "/", nrow(M_comb),"\n")
  #B_cor = cor(t(B), use = "pairwise.complete.obs")
  if(is.na(B_cs[1,2])){M_comb[i,"CS"] = NA}else{M_comb[i,"CS"] = B_cs[1,2]}
}

head(M_comb, n = 15)

## Filling in LiRa Similarity with Uniform Assumption
M_comb = cbind(M_comb, 0)
colnames(M_comb)[5] = "LiRaU" 

for(i in 1:nrow(M_comb)){
  
  cat("iteration:", i, "/", nrow(M_comb),"\n")
  
  #i = 1
  A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  M_comb[i,"LiRaU"] = lira(x_u = B[1,], x_v = B[2,], num_ratings = 5)
  #B = B[intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))]
  #B_cs = cosine_similarity(as.matrix(B))
  #cat("i",i,length(intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))), "overlap", "\n")
  #B_cor = cor(t(B), use = "pairwise.complete.obs")
  #if(is.na(B_cs[1,2])){M_comb[i,"CS"] = NA}else{M_comb[i,"CS"] = B_cs[1,2]}
}


## Filling in LiRa Similarity with Gaussian Assumption
diffs = c()
iter = 5000 #overkill
for(i in 1:iter){
  ids = sample(x = unique(D$user), size = 2, replace = FALSE)
  A = rbind(D[which(D$user == ids[1]),],D[which(D$user == ids[2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  diffs = c(diffs, unlist(B[1,] - B[2,]))
  
}
(mu_pop = mean(c(diffs), na.rm = TRUE)); (v_pop = var(c(diffs), na.rm = TRUE)); (sd_pop = sd(c(diffs), na.rm = TRUE))


M_comb = cbind(M_comb, 0)
colnames(M_comb)[6] = "LiRaG" 

for(i in 1:nrow(M_comb)){
  
  cat("iteration:", i, "/", nrow(M_comb),"\n")
  
  #i = 1
  A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  B_diff = B[1,] - B[2,]
  #mean(unlist(B_diff), na.rm = TRUE)
  M_comb[i,"LiRaG"] = v_pop/var(unlist(B_diff), na.rm = TRUE)
  #B = B[intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))]
  #B_cs = cosine_similarity(as.matrix(B))
  #cat("i",i,length(intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))), "overlap", "\n")
  #B_cor = cor(t(B), use = "pairwise.complete.obs")
  #if(is.na(B_cs[1,2])){M_comb[i,"CS"] = NA}else{M_comb[i,"CS"] = B_cs[1,2]}
}

head(M_comb, n = 15)
plot(density(M_comb[,"PC"], na.rm = TRUE))
plot(density(M_comb[,"CS"], na.rm = TRUE))
plot(density(M_comb[,"LiRaU"], na.rm = TRUE))
plot(density(M_comb[,"LiRaG"], na.rm = TRUE))

head(M_comb, n = 15)

head(D_test)

D_test = as.matrix(D_test)


sim_msr = "PC"
sim_msr = "CS"
sim_msr = "LiRaU"
sim_msr = "LiRaG"

k = 15

ratings_mae = c()

for(i in 1:nrow(D_test)){ # write this as a "validation" function(k, sim_msr)
  #i = 9211
  user = D_test[i, "user"]
  item = D_test[i, "item"]
  rating = D_test[i,"rating"]
  
  M_user = M_comb[which(M_comb[,"User1"] == user | M_comb[,"User2"] == user),]
  M_user = M_user[order(M_user[,sim_msr], decreasing = TRUE),] # not needed yet, I think
  
  neighbors = unique(c(M_user[,"User1"],M_user[,"User2"]))
  possible_users = D_train[which(D_train$item == item),"user"]
  
  if(length(possible_users) != 0){
    viable_neighbors = intersect(neighbors, possible_users)
    
    if(length(viable_neighbors) != 0){
    
      M_user = subset(M_user, M_user[,"User1"] %in% viable_neighbors | M_user[,"User2"] %in% viable_neighbors)
      M_user = M_user[order(M_user[,sim_msr], decreasing = TRUE),]
      
      if(length(nrow(M_user) > 1) != 0){
        
        rows = min(nrow(M_user), k)
        top_k_users = c(M_user[1:rows,c("User1","User2")])
        top_k_users = top_k_users[-which(top_k_users == user)]
        M_user = M_user[1:rows,]
        
      }else{
        top_k_users = c(M_user[c("User1","User2")])
        top_k_users = top_k_users[-which(top_k_users == user)]
        
      }
      #top_k_users = top_k_users[-which(top_k_users == user)]
      
      #subset(D_train, (D_train$user %in% top_k_users) & (D_train$item %in% item))
      #item #6
      
      if(length(top_k_users) > 1){
        M_user = cbind(M_user, user = top_k_users)
        M_user = M_user[,c("user",sim_msr)]
      }else{
        M_user = c(top_k_users, M_user[sim_msr])
        names(M_user)[1] = "user"
        M_user = t(data.frame(M_user))
        
      }
      
      neighbor_ratings = D_train[(which(D_train$user %in% top_k_users & D_train$item %in% item)),]
      
      neighbor_ratings_df = merge(M_user, neighbor_ratings, by = "user")
      
      if(length(which(neighbor_ratings_df[,sim_msr] < 0)) > 0){
        neighbor_ratings_df = neighbor_ratings_df[-which(neighbor_ratings_df[,sim_msr] < 0),]
      }
      # NEED TO PUT RESTRICTION FOR >0 SIMILARITY
      pred_rating = sum(neighbor_ratings_df[,sim_msr] * neighbor_ratings_df[,"rating"])/sum(neighbor_ratings_df[,sim_msr])
      #rating
      print(i)
      ratings_mae[i] = abs(pred_rating - rating)
      
    }else{ratings_mae[i] = NA}
    
        
    }else{ratings_mae[i] = NA}
    
}

mean(ratings_mae, na.rm = TRUE)

#ratings_mae_cs = ratings_mae
#ratings_mae_pc = ratings_mae
#ratings_mae_lu = ratings_mae
#ratings_mae_lg = ratings_mae

# k = 3
mean(ratings_mae_pc, na.rm = TRUE) # 0.816, 0.832
mean(ratings_mae_cs, na.rm = TRUE) # 0.805, 0.824
mean(ratings_mae_lu, na.rm = TRUE) # 0.808, 0.821
mean(ratings_mae_lg, na.rm = TRUE) # 0.796, 0.826

# k = 5
mean(ratings_mae_pc, na.rm = TRUE) # 0.776, 0.797
mean(ratings_mae_cs, na.rm = TRUE) # 0.771, 0.787
mean(ratings_mae_lu, na.rm = TRUE) # 0.786, 0.795
mean(ratings_mae_lg, na.rm = TRUE) # 0.768, 0.792

# k = 7
mean(ratings_mae_pc, na.rm = TRUE) # 0.761, 0.777
mean(ratings_mae_cs, na.rm = TRUE) # 0.756, 0.770
mean(ratings_mae_lu, na.rm = TRUE) # 0.774, 0.782
mean(ratings_mae_lg, na.rm = TRUE) # 0.754, 0.777

# k = 10
mean(ratings_mae_pc, na.rm = TRUE) # 0.749, 0.767
mean(ratings_mae_cs, na.rm = TRUE) # 0.745, 0.761
mean(ratings_mae_lu, na.rm = TRUE) # 0.767, 0.775
mean(ratings_mae_lg, na.rm = TRUE) # 0.745, 0.767

# k = 15
mean(ratings_mae_pc, na.rm = TRUE) # 0.740, 0.758
mean(ratings_mae_cs, na.rm = TRUE) # 0.739, 0.757
mean(ratings_mae_lu, na.rm = TRUE) # 0.761, 0.767
mean(ratings_mae_lg, na.rm = TRUE) # 0.739, 0.761



for(i in 1:nrow(M_comb)){}

pc_function = function(i){

  A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  #cat("i",i,length(intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))), "overlap", "\n")
  cat("iteration:", i, "/", nrow(M_comb),"\n")
  B_cor = cor(t(B), use = "pairwise.complete.obs")
  if(is.na(B_cor[1,2])){M_comb[i,"PC"] = NA}else{M_comb[i,"PC"] = B_cor[1,2]}
  
}

vapply(FUN = pc_function, X = 1:nrow(M_comb))
sapply(X = 1:nrow(M_comb), FUN = pc_function)







diffs = c()
iter = 5000 #overkill
for(i in 1:iter){
  ids = sample(x = unique(D$user), size = 2, replace = FALSE)
  A = rbind(D[which(D$user == ids[1]),],D[which(D$user == ids[2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  diffs = c(diffs, unlist(B[1,] - B[2,]))
}
(mu_pop = mean(c(diffs), na.rm = TRUE)); (v_pop = var(c(diffs), na.rm = TRUE)); (sd_pop = sd(c(diffs), na.rm = TRUE))



## Filling in Similarity

M_comb = cbind(D_similarity, PC = NA, CS = NA, LiRaU = NA, LiRaG = NA)

head(M_comb, n = 15)

start = Sys.time()
for(i in 1:nrow(M_comb)){
  
  cat("iteration:", i, "/", nrow(M_comb),"\n")
  A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  B_fixed = dcast(data = A, formula = user~item, value.var = "rating")
  B = B_fixed[,-1]
  B_cor = cor(t(B), use = "pairwise.complete.obs")
  if(is.na(B_cor[1,2])){M_comb[i,"PC"] = NA}else{M_comb[i,"PC"] = B_cor[1,2]}
  
  #B = dcast(data = A, formula = user~item, value.var = "rating")
  #B = B[,-1]
  B = B_fixed[intersect(which(!is.na(B_fixed[1,])),which(!is.na(B_fixed[2,])))]
  B_cs = cosine_similarity(as.matrix(B))
  if(is.na(B_cs[1,2])){M_comb[i,"CS"] = NA}else{M_comb[i,"CS"] = B_cs[1,2]}
  
  #A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  #B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B_fixed[,-1]
  M_comb[i,"LiRaU"] = lira(x_u = B[1,], x_v = B[2,], num_ratings = 5)
  #cat("iteration:", i, "/", nrow(M_comb),"\n")
  #i = 1
  #A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  #B = dcast(data = A, formula = user~item, value.var = "rating")
  #B = B[,-1]
  B_diff = B[1,] - B[2,]
  #mean(unlist(B_diff), na.rm = TRUE)
  M_comb[i,"LiRaG"] = v_pop/var(unlist(B_diff), na.rm = TRUE)

}
end = Sys.time()
print(end - start)

head(M_comb, n = 15)



  



















