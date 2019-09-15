##############################################
### Neighborhood-based Ratings Predictions ###
# Example from pg.34 of Recommender System Textbook
###############################################
source('~/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

# Sample data from text 
u1 = c(7,6,7,4,5,4)
u2 = c(6,7,NA,4,3,4)
u3 = c(NA,3,3,1,1,NA)
u4 = c(1,2,2,3,3,4)
u5 = c(1,NA,1,2,3,3)
R = rbind(u1,u2,u3,u4,u5)

# Means to use later...
mu1 = mean(u1, na.rm = TRUE)
mu2 = mean(u2, na.rm = TRUE)
mu3 = mean(u3, na.rm = TRUE)
mu4 = mean(u4, na.rm = TRUE)
mu5 = mean(u5, na.rm = TRUE)
mu = c(mu1,mu2,mu3,mu4,mu5)

# User-User Correlations
R_pc = cor(t(R), use = "pairwise.complete.obs")
R_pc
user = 1
R_pc[user,]

# Select Item for Prediction - this is important for larger datasets
item_num = 1
item = which(is.na(R[user,]))[item_num] # item choices
possible_users = names(R[,item][which(!is.na(R[,item]))]) # people who have actually rated the item of interest

# Selecting Most Correlated Users
topN = 2 # looking at two most closely correlated users
nn = R_pc[user,]
nn = nn[intersect(names(nn), possible_users)]
nn = sort(nn, decreasing = TRUE)[2:(topN+1)]
nn

# Predicted Rating for Item 1 by User 3
r31 = sum((R[names(nn),item] * nn))/sum(nn) # rating for user3 on item 1 
r31 # 6.479
item_num = 2
item = which(is.na(R[user,]))[item_num] # item choices
r36 = sum((R[names(nn),item] * nn))/sum(nn)  # rating for user3 on item 6 
r36 # 4


# Mean Centered Approach
# Same exact process following the mean centering
# Idea is to remove personal bias
# For User 3, top 2 closed neighbors are 1,2 (who happen to consistently rate things highly)
R = t(mean_center_matrix(R))
R

# User-User Correlations
R_pc = cor(t(R), use = "pairwise.complete.obs")
user = 3
R_pc[user,]

# Selecting Most Correlated Users
topN = 2
nn = sort(R_pc[user,], decreasing = TRUE)[2:(topN+1)]
nn

# Predicted Rating for Item 1 by User 3 after Mean-Centering
item_num = 1
item = which(is.na(R[user,]))[item_num] # item choices
r31 = mu[user] + sum((R[names(nn),item] * nn))/sum(nn)  # rating for user3 on item 1 
r31 # 3.343
item_num = 2
item = which(is.na(R[user,]))[item_num] # item choices
r36 = mu[user] + sum((R[names(nn),item] * nn))/sum(nn)  # rating for user3 on item 6 
r36 # 0.86







