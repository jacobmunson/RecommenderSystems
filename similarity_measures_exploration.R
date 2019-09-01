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
# 

df_test = data.frame(x1,x2)

cor(df_test, use = "na.or.complete")


