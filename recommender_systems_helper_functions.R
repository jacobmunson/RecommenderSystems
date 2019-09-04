### Recommender Systems Helper Functions ###
# This file is meant to house reusable
# Base R solutions to common problems 


impute_mean <- function(x){ifelse (is.na(x), mean(x, na.rm = TRUE), x)}
impute_median <- function(x){ifelse (is.na(x), median(x, na.rm = TRUE), x)}
impute_min <- function(x){ifelse (is.na(x), min(x, na.rm = TRUE), x)}
impute_max <- function(x){ifelse (is.na(x), max(x, na.rm = TRUE), x)}

cosine_similarity = function(matrix){
  cos_sim = matrix/sqrt(rowSums(matrix * matrix))
  cos_sim = cos_sim %*% t(cos_sim)
  return(cos_sim)
}

sparsity = function(matrix){
  sparsity = length(which(matrix == 0))/(nrow(matrix)*ncol(matrix))
  return(sparsity)
}

mean_center_matrix <- function(D){
  apply(X = D, MARGIN = 1, FUN = function(row){row - mean(row)})
}

lira = function(x_u, x_v, num_ratings){
  num_diff = length(which(!is.na(abs(x_u - x_v))))
  lira_bottom = (1/num_ratings)^num_diff
  lira_top = 0.5^(num_diff)
  lira = log10(lira_top/lira_bottom)
  return(lira)
}