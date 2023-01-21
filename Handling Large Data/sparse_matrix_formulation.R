D = read_csv("Recommender Systems - Home Folder/ml-latest-small/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")

library(Matrix)

num_rows = length(unique(D$user))
num_cols = length(unique(D$item))

#1:num_rows
#1:num_cols
user_df = data.frame(user = sort(unique(D$user)), user_adj = 1:length(unique(D$user)))
item_df = data.frame(item = sort(unique(D$item)), item_adj = 1:length(unique(D$item)))

library(tidyverse)

#inner_join()
D = inner_join(D, user_df, by = c("user" = "user"))
D = inner_join(D, item_df, by = c("item" = "item"))

D
length(unique(D$item))

R = sparseMatrix(i = D$user_adj, j = D$item_adj, x = D$rating, dims = c(num_rows, num_cols))


dim(R %*% t(R))

dim(t(R) %*% R)


cosine_similarity = function(matrix){
  cos_sim = matrix/sqrt(rowSums(matrix * matrix))
  cos_sim = cos_sim %*% t(cos_sim)
  return(cos_sim)
}



cosine_similarity(R)
