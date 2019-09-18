# this file is under construction still, but the idea is to simply determine what users need to be measured as similar
# this is only useful when a full "ratings matrix" cannot be explicitly stored
# I also explored sparse matrices in R and couldn't get correlation (for example) to behave
# I verified this approach be using a smaller dataset (the ML 100k dataset), converting to a ratings matrix R, then 
# taking R %*% t(R), replacing the lower triangle (and diag) with 0, replacing positive elements with 1, then summing
# This is how many similarities need to be computed 
# The chunk of code between here and the "verification" chunk around line 100 reproduces that (albeit very slowly).
library(readr)
D <- read.csv("C:/Users/Jacob/Downloads/ml-latest-small/ml-latest-small/ratings.csv")
D <- read.csv("C:/Users/Jacob/Downloads/ml-20m/ml-20m/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
dim(D)
#D_total = D
#set.seed(1)
#id = sample(x = seq(1,nrow(D),1), size = 50000, replace = FALSE) #33098 - 50000 /  91924 - 100000
#D = D[id,]
#D

head(D)
length(unique(D$user)) # 138493 users 


cat("Start Time:", format(Sys.time(), "%a %b %d %X %Y"))

C = data.frame(user1 = 0, user2 = 0)
coraters_total = c()

for(i in 1:length(unique(D$user))){
  #i = 1
  items = D[which(D$user == unique(D$user)[i]),"item"]
  coraters_total = c()
  for(j in 1:length(unique(items$item))){
    #print(j)
    #j = 2
    coraters = D[which(D$item == items$item[j]),"user"]
    coraters_total = c(coraters_total, coraters$user)
  }
  coraters_total = unique(coraters_total)
  C1 = expand.grid(unique(D$user)[i],coraters_total)
  colnames(C1) = c("user1","user2")
  
  C1_rev = C1[which(C1[,1] == i),c(2:1)]
  colnames(C1_rev) = c("user1","user2")
  
  C = anti_join(C, C1_rev, by = c("user1","user2"))
  C = rbind(C,C1)
  cat("User:",i,"/",length(unique(D$user)), "|",
      "Comparisons:",dim(C1)[1],
      "Total Comparisons:",nrow(C),"\n")
}

if(length(which(C[,1] == C[,2])) > 0){
  C = C[-which(C[,1] == C[,2]),] # only run once
  dim(C)
  print("")
}


M_comb = cbind(C, 0)
colnames(M_comb) = c("User1","User2","Sim")
dim(C)
M_comb = M_comb[-which(M_comb[,1] == M_comb[,2]),] # should be able to skip
head(M_comb, n = 15)
dim(M_comb)
library(reshape2)

for(i in 1:nrow(M_comb)){
  A = rbind(D[which(D$user == M_comb[i,1]),],D[which(D$user == M_comb[i,2]),])
  B = dcast(data = A, formula = user~item, value.var = "rating")
  B = B[,-1]
  #cat("i",i,length(intersect(which(!is.na(B[1,])),which(!is.na(B[2,])))), "overlap", "\n")
  cat("iteration:", i, "/", nrow(M_comb))
  B_cor = cor(t(B), use = "pairwise.complete.obs")
  if(is.na(B_cor[1,2])){M_comb[i,3] = NA}else{M_comb[i,3] = B_cor[1,2]}
}


M_comb = M_comb[order(M_comb$User1,M_comb$Sim, decreasing = TRUE),]


user = 1
N = 10
user_i = M_comb[which(M_comb$User1 == user | M_comb$User2 == user),][1:N,]


user1 = 1
user2 = 7
M_comb[which(M_comb$User1 == user1 & M_comb$User2 == user2),]

library(dplyr)
M_comb %>% filter(User1 == user1, User2 == user2)

# A = rbind(D[which(D$user == user1),],D[which(D$user == user2),])
# B = dcast(data = A, formula = user~item, value.var = "rating")
# B = B[,-1]
# cor(t(B), use = "pairwise.complete.obs")


# verification
R = dcast(data = D, formula = user~item, value.var = "rating")
R_corr = cor(t(R), use = "pairwise.complete.obs")
dim(R_corr)
R_corr[1:10,1:10]
R[1:10,1:10]
R[is.na(R)] = 0
R = R[,-1]
dim(R); str(R)
V = as.matrix(R) %*% t(R)
dim(V)
V[1:10,1:10]
V[lower.tri(V, diag = TRUE)] = 0
V[V > 0] = 1
sum(V)


