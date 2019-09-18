#################################
### User Base Recommendations ###
#################################
# For all items that a user has NA, going to predict a rating
# Obviously this is far more computationally expensive (nevermind inefficient) than just "get topN" recommendations
# The point here is that I want to do a lot of post-recommendation analysis
# Also, will likely make this run in parallel in future iterations. 

# 100k MovieLense dataset
library(readr)
D <- read.csv("C:/Users/Jacob/Downloads/ml-latest-small/ml-latest-small/ratings.csv")
head(D); dim(D)
D %>% group_by(userId) %>% 
  summarize(NumRec = n()) %>% summarize(max(NumRec), min(NumRec), mean(NumRec), sd(NumRec), median(NumRec))


# Matrix format
library(reshape2)
R = dcast(D, userId~movieId, value.var = "rating", na.rm=FALSE)
R[1:15,1:15]
R = as.matrix.data.frame(R)
rownames(R) = R[,1]
user_list = rownames(R)
R = R[,-1]
R[1:15,1:15]
dim(R)

# Similarity Measure - using Pearson
R_pc = cor(t(R), use = "pairwise.complete.obs")
R[is.na(R)] = 0
R_pc = cosine_similarity(matrix = R)
dim(R_pc)
R_pc[1:15,1:5]

# Build Recommendations
cbr_recommendations = c()
new_ratings = TRUE # toggle for produced prediction evaluation vs providing recommendations

start = Sys.time()
for(j in 1:length(unique(user_list))){
  user = j
  
  if(new_ratings){
    items_to_rate = which(is.na(R[user,]))
  }else{
    items_to_rate = which(!is.na(R[user,]))    
  }

  customer_prediction = matrix(data = 0, nrow = length(items_to_rate), ncol = 3)
  customer_prediction[,1] = user
  rownames(customer_prediction) = seq(1, length(items_to_rate), 1)
  colnames(customer_prediction) = c("userId","rating","movieId")
  
  for(i in 1:length(items_to_rate)){
    #i = 1
    
    if(new_ratings){
      item = which(is.na(R[user,]))[i] #[item_num] # item choices
    }else{
      item = which(!is.na(R[user,]))[i] #[item_num] # item choices
    }
    
    possible_users = names(R[,item][which(!is.na(R[,item]))]) # people who have actually rated the item of interest
    
    # Selecting Most Correlated Users
    topN = 2 # looking at two most closely correlated users
    nn = R_pc[user,]
    nn = nn[intersect(names(nn), possible_users)]
    
    
    if(length(which(is.na(nn))) > 0){nn = nn[-which(is.na(nn))]}
    
    if(length(nn) > topN){
      nn = sort(nn, decreasing = TRUE)[2:(topN+1)]
    }else{nn = sort(nn, decreasing = TRUE)}
    nn = nn[which(nn > 0)] # added since ratings can get drastically inflated by a nearest neighbor being negatively correlated
    
    # Predicted Rating for Item 1 by User 3
    r = sum((R[names(nn),item] * nn))/sum(nn) # rating for user1 on item 1 
    customer_prediction[i,2] = r
    customer_prediction[i,3] = item
    
  }
  
  #customer_prediction = customer_prediction[-which(is.nan(customer_prediction[,"rating"])),]
  cbr_recommendations = rbind(cbr_recommendations, customer_prediction)
  cat("user", j, "completed...","\n")
  
}
end = Sys.time()
end - start # 38.12 minutes for 610 users
dim(cbr_recommendations) # 1341976 total recommendations -> down to 1251797 after ** adjustment
length(unique(cbr_recommendations[,"userId"])) # 609/610 users received recommendations 

# Some Post-Recommendation Analysis
library(dplyr)
cbr_recommendations = as_tibble(cbr_recommendations)
cbr_recommendations %>% group_by(userId) %>% 
  summarize(NumRec = n()) %>% summarize(max(NumRec), min(NumRec), mean(NumRec), sd(NumRec), median(NumRec))
# Most Recommendations: 5174 -> 4901 (i.e. at least one user got 5174 recommendations)
# Minimum Recommendations: 5 -> 5
# Mean Recommendations: 2204 -> 2055
# Mediam Recommendations: 2236 -> 2136
# Recommendations sd: 1293  -> 1165
## values following "->" are after ** adjustment

# If we want to give TopN Recommendations for a single user
head(cbr_recommendations)
N = 10
cbr_topN_recommendation = cbr_recommendations %>% group_by(userId) %>% arrange(desc(rating)) %>% top_n(n = N) 
# noticeable early on, some people have *very* large recommendation ratings
# for example, user 386 on item 9691 has a rating prediction of 196 -> going to dig deeper on this
# follow-up: the two "closest" users to user 386 are 417 (pc = 0.4308202) & 338 (pc = -0.4264014)
# this making the sum that get divided by 0.004418786, thus inflating the rating drastically
# solution: restrict nearest neighbors to those with positive values (nn = nn[which(nn > 0)]) (hereby: "** adjustment")
## after ** adjustment, user 386 on item 9691 has a rating of 3!!! Much better. 

# Some Evaluation Measures - Recommender Systems Textbook (pg. 231)

### Coverage Metric
## Two varieties
## User-Space Coverage - fraction of all users such that at least k recommendations per user can be predicted
## Item-Space Coverage - fraction of all items such that at least k recommendations (to users) per item can be predicted
# Let's say k=1 is sufficient for our concerns for each Coverage Metric
## How many movies got recommended? 
length(unique(cbr_topN_recommendation$movieId)) # 6252
length(unique(cbr_recommendations$movieId)) # 6252
## How many movies are there in the dataset? 
length(unique(D$movieId)) # 9724
# So the Item-Space Coverage is 6252/9724 => 64.3% coverage
# For the User-Space Coverage, 609/610 => 99.8% coverage
# It is worth noting that either of these measures can be artificially inflated by sacrificing performance measures (like accuracy)
# to attain higher coverage by providing seemingly less suitable recommendations (this might also then be in conflict with other measures like Diversity)

ratings_comparison = merge(D, cbr_recommendations, by = c("userId","movieId"))
mean(abs(ratings_comparison$rating.x - ratings_comparison$rating.y), na.rm = TRUE)
