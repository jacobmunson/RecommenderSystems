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

lira = function(x_u, x_v, num_ratings, lira_pure_chance_pdf){
  
  diff = abs(x_u - x_v)
  diff = diff[!is.na(diff)]
  num_diff = length(diff)
  
  d = num_ratings
  
  # pure chance
  lira_bottom = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))


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

