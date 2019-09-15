library(readr)
D <- read.csv("C:/Users/Jacob/Downloads/ml-latest-small/ml-latest-small/ratings.csv")
head(D)

library(reshape2)
R = dcast(D, userId~movieId, value.var = "rating", na.rm=FALSE)
R[1:15,1:15]
R = as.matrix.data.frame(R)
rownames(R) = R[,1]
user_list = rownames(R)
R = R[,-1]
R[1:15,1:15]
dim(R)

R_pc = cor(t(R), use = "pairwise.complete.obs")
dim(R_pc)
R_pc[1:15,1:5]

user = 1
R_pc[user,1:5]

# Select Item for Prediction - this is important for larger datasets



#customer_item_recommendations = function(i){}

cbr_recommendations = c()

start = Sys.time()
for(j in 1:length(unique(user_list))){
  user = j
    
  items_to_rate = which(is.na(R[user,]))
  customer_prediction = matrix(data = 0, nrow = length(items_to_rate), ncol = 2)
  customer_prediction[,1] = user
  rownames(customer_prediction) = seq(1, length(items_to_rate), 1)
  colnames(customer_prediction) = c("userId","movieId")
  
  for(i in 1:length(items_to_rate)){
    
    #i = 100#95
    item = which(is.na(R[user,]))[i] #[item_num] # item choices
    possible_users = names(R[,item][which(!is.na(R[,item]))]) # people who have actually rated the item of interest
    
    # Selecting Most Correlated Users
    topN = 2 # looking at two most closely correlated users
    nn = R_pc[user,]
    nn = nn[intersect(names(nn), possible_users)]
    nn = nn[-which(is.na(nn))]
    if(length(nn) > topN){
      nn = sort(nn, decreasing = TRUE)[2:(topN+1)]
    }else{nn = sort(nn, decreasing = TRUE)}
    #nn
    
    # Predicted Rating for Item 1 by User 3
    r = sum((R[names(nn),item] * nn))/sum(nn) # rating for user1 on item 1 
    customer_prediction[i,2] = r
    #r # 2.667
    #item  
    #print(i)
    
  }
  
  customer_prediction = customer_prediction[-which(is.nan(customer_prediction[,"movieId"])),]
  cbr_recommendations = rbind(cbr_recommendations, customer_prediction)
  cat("user", j, "completed...","\n")
  
}
end = Sys.time()
end - start

dim(cbr_recommendations) # 240
length(unique(cbr_recommendations[,"userId"]))


lapply()