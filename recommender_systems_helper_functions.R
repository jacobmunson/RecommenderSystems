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
  apply(X = D, MARGIN = 1, FUN = function(row){row - mean(row, na.rm = TRUE)})
}

lira = function(x_u, x_v, num_ratings){
  num_diff = length(which(!is.na(abs(x_u - x_v))))
  lira_bottom = (1/num_ratings)^num_diff
  lira_top = 0.5^(num_diff)
  lira = log10(lira_top/lira_bottom)
  return(lira)
}

prediction_evaluation_function = function(train_set, test_set, similarity_vector){
  
  #mae_nn = c(); mae_knn = c()
  if(length(similarity_vector) == 0 | any(is.na(similarity_vector))){mae_nn = NA; mae_knn = NA}else{
    
    neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
    neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
    
    pred_rating_nn = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(abs(neighbor_ratings$y)) # is this supposed to be /|abs(sim)|
    mae_nn = abs(pred_rating_nn - test_set$rating)
    
    pred_rating_knn = mean(neighbor_ratings$rating)
    mae_knn = abs(pred_rating_knn - test_set$rating)
  }
  return(c(mae_nn, mae_knn))
}

