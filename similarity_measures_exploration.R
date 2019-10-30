###################################
### Similarity Measures ###########
# Cosine, Pearson, Jaccard ########
# Non-thorough LiRa exploration ### 
# 8/26/2019 #######################
###################################

### Two vectors to play with ###
# designed to be very similar
y_u = c(1,1,0,0,0,2)
y_v = c(1,1,3,0,0,2)
# Means of each vector
mu_yu = mean(y_u)
mu_yv = mean(y_v)


### Cosine Similarity ###
# Relatively efficient implementation
# Note: Default R function cos()
# accepts only one vector

cosine_similarity = function(matrix){
  cos_sim = matrix/sqrt(rowSums(matrix * matrix))
  cos_sim = cos_sim %*% t(cos_sim)
  return(cos_sim)
}

# function-wise
cosine_similarity(rbind(y_u,y_v)) # 0.6324555

# "by hand" 
(cs = (y_u %*% y_v)/(sqrt(sum(y_u^2))*sqrt(sum(y_v^2)))) # 0.6324555


### Pearson Correlation ###
# This one is a *little* tricky
# Three Situations:
# (1) PC between two vectors
# (2) Mean-centering vectors
# (3) Missing Values 

## Let's start simple
# (1) By hand
(((y_u-mu_yu) %*% (y_v-mu_yv)))/sqrt(sum((y_u-mu_yu)^2) %*% sum((y_v-mu_yv)^2)) # 0.2793721
# Canned function in R 
cor(y_u, y_v) # 0.2793721


# (2) Row Normalizing for PC
y_u = y_u - mean(y_u)
y_v = y_v - mean(y_v)
pc = (((y_u-mu_yu) %*% (y_v-mu_yv)))/sqrt(sum((y_u-mu_yu)^2) %*% sum((y_v-mu_yv)^2)) # 0.6324555
cor(y_u,y_v)
pc
pc == cs # TRUE, that is: 0.6324555 = 0.6324555
# Mean-centering rows of vector makes CS = PC (but only if no missing values in vectors)
# This is important as to to how PC is defined 
# For example, if we force missing values to be 0, this does not hold
# Akin to that, if we only look at common items rated/bought, then equality holds

# (3) Missing Values & Only Looking at Co-rated Items 
# new vectors 
cor(c(1,2,2),c(1,3,2)) # PC = 0.866 *

x1 = c(1,2,2,2)
x2 = c(1,3,2,NA)

cor(x1,x2) # NA
cor(c(1,2,2,2),c(1,3,2,0)) # Forcing NA -> 0, PC = 0.258 (not the same as excluding the last observation of each as in *)
cor(x1,x2, use = "na.or.complete") # Only looking at co-rated items, PC = 0.866

# Takeaway: 
## PC is the same as CS if vectors are user-centered prior to PC AND no missing values (i.e. same set of items rated)
## If we have missing values and exclude those observations for the appropriate user then that is different than imputing a 0

### LiRa Similarity ###
# LiRa = log10(A/B)
# A = prob(diff{x_u,x_v} | same cluster)
# B = prob(diff{x_u,x_v} | pure chance)
# A,B are given as lira_bottom,lira_top in following function

# Assumptions
# d = number of ratings
d = 5 # scores 1,2,3,4,5

# Data Points 
# (Nearly) Same vectors, but with missing values instead of 0s
y_u = c(1,1,0,0,0,5); y_u[y_u == 0] = NA
y_v = c(2,1,3,0,0,2); y_v[y_v == 0] = NA
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 1.19382

# Example set 1 from paper
y_u = c(1,1,0,0,0,2); y_u[y_u == 0] = NA
y_v = c(1,1,0,0,0,2); y_v[y_v == 0] = NA
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 1.19381
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 1 (perfect "match")

# Example set 2 from paper
y_u = c(1,1,5,4,4,2); y_u[y_u == 0] = NA
y_v = c(1,1,5,4,4,2); y_v[y_v == 0] = NA
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 2.38764
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 1 (perfect "match" still - but literally twice as much observed data)

# Additional example sets
# What happens as vector length increases?

y_u = c(1,1,5,4,0,0,0); y_u[y_u == 0] = NA
y_v = c(1,1,5,5,0,0,1); y_v[y_v == 0] = NA
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 1.59
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 0.98 

y_u = c(1,1,5,4,0,0,3); y_u[y_u == 0] = NA
y_v = c(1,1,5,5,0,0,1); y_v[y_v == 0] = NA
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 1.98
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 0.86

y_u = c(1,1,5,4,0,0,3,0,5,0); y_u[y_u == 0] = NA
y_v = c(1,1,5,5,0,0,1,1,3,1); y_v[y_v == 0] = NA
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 2.38
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 0.79