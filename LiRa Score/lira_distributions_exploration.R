########################################
### LiRa - Distribution Explorations ###
# 10/30/19 #############################

library(dplyr)
library(reshape2)
library(readr)
# Data Set & Formatting
D = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")


## Visually examining data
dim(D)
head(D)
table(D$rating)


## Sampling differences 
iter = 2500
diff_vector = c()
for(i in 1:iter){
  user_pair = sample(x = unique(D$user), size = 2, replace = F) # force difference users
  user_pair_data = D %>% filter(user == user_pair[1] | user == user_pair[2])
  user_pair_matrix = dcast(data = user_pair_data, formula = user~item, value.var = "rating")
  user_pair_matrix = user_pair_matrix[,-1]
  differences = as.numeric(user_pair_matrix[1,] - user_pair_matrix[2,])
  diff_vector = c(diff_vector, differences[is.finite(differences)])
}
plot(table(abs(diff_vector)), main = "Distribution of Absolute Difference Samples")

num_ratings = length(unique(abs(diff_vector)))

## Pure Chance Distribution
diff = seq(1:num_ratings)
num_ratings = length(unique(diff))
num_diff = length(diff)
plot(diff, rep((1/num_diff), num_diff), type = 'h', main = "Distribution - Pure Chance")


## Same Cluster Distribution
delta = diff 
d = max(delta)
c_delta = (1/2)^delta[1:(length(delta) - 1)]
c_delta_minus_1 = 1/(2^(d-1))
pdf_same_cluster = c(c_delta,c_delta_minus_1)
sum(pdf_same_cluster)
plot(pdf_same_cluster, type = 'h', main = "Distribution - Same Cluster")


delta = diff
d = max(delta)
c_delta = (1/2)^delta[1:(length(delta) - 1)]
c_delta_minus_1 = 1/(2^(d-1))
pdf_same_cluster = c(c_delta,c_delta_minus_1)
sum(pdf_same_cluster)
plot(pdf_same_cluster, type = 'h', main = "Distribution - Same Cluster - Original: k = 2")


delta = diff 
d = max(delta)
c_delta = (1/3)^delta[1:(length(delta) - 1)]
c_delta_minus_1 = 1 - sum(c_delta)
pdf_same_cluster = c(c_delta,c_delta_minus_1)
sum(pdf_same_cluster)
plot(pdf_same_cluster, type = 'h', main = "Distribution - Same Cluster - k = 3")


delta = diff
d = max(delta)
c_delta = (1/3)^delta[1:(length(delta) - 1)]
c_delta_remaining = 1 - sum(c_delta)
pdf_same_cluster = c(c_delta,0) + c_delta_remaining/d
sum(pdf_same_cluster)
plot(pdf_same_cluster, type = 'h', main = "Distribution - k = 3 - Uniformly Redistributed")


par(mfcol = c(3,1))
plot(table(abs(diff_vector)), main = "Distribution of Absolute Difference Samples")
plot(diff, rep((1/num_diff), num_diff), type = 'h', main = "Distribution - Pure Chance")
plot(pdf_same_cluster, type = 'h', main = "Distribution - Same Cluster")




diff = c()
V = seq(0.5,5,0.5)
V = seq(1,5,1)
V = unique(D$ratings)
for(i in 1:100000){
  diff[i] = abs(sample(x = V, size = 1, replace = T) - sample(x = V, size = 1, replace = T))
}
table(diff)/100000
sum(table(diff)/100000)
plot(table(diff)/100000, xlab = "", ylab = "P(|diff|)", main = "Distribution - Pure Chance")


lira_pure_chance_pdf = matrix(table(diff)/100000)
rownames(lira_pure_chance_pdf) = V - 1
rownames(lira_pure_chance_pdf) = V - 0.5
colnames(lira_pure_chance_pdf) = "prob"
lira_pure_chance_pdf

d = length(V)

# x_u = sample(x = V, size = 10, replace = T) 
# x_v = sample(x = V, size = 10, replace = T)

table(abs(x_u - x_v))


prod(L[names(table(abs(x_u - x_v))),]^table(abs(x_u - x_v)))


x_u = c(1,1,5,4,4,2); x_v = c(1,1,5,4,4,2)
x_u = c(1,1,NA,NA,NA,2); x_v = c(1,1,NA,NA,NA,2);


lira(x_u = x_u, x_v = x_v, num_ratings = d, lira_pure_chance_pdf = lira_pure_chance_pdf)


diff = abs(x_u - x_v)
diff = diff[!is.na(diff)]
num_diff = length(diff)


# same cluster
if(any(diff == d - 1)){
  lira_top = c()
  for(i in 1:num_diff){
    if(diff[i] == d - 1){
      lira_top[i] = 1/(2^(d - 1))    
    }else{
      lira_top[i] = (1/2)^(1)
    }
  }
  lira_top = prod(lira_top)
}else{
  lira_top = 0.5^(num_diff)
}


lira_top
lira_bottom = prod(lira_pure_chance_pdf[names(table(abs(x_u - x_v))),]^table(abs(x_u - x_v)))

log10(lira_top/lira_bottom)





