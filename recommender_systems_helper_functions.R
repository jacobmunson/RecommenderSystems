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
#  if(any(diff == d - 1)){
    lira_top = c()
    for(i in 1:num_diff){
      if(diff[i] == d - 1){
        lira_top[i] = 1/(2^(d - 1))    
      }else{
        lira_top[i] = (1/2)^(diff[i] + 1)
      }
    }
    lira_top = prod(lira_top)
#  }
  
  lira = log10(lira_top/lira_bottom)
  return(lira)
}

lira = function(x_u, x_v, lira_pure_chance_pdf, lira_same_cluster_pdf){
  
  diff = abs(x_u - x_v)
  diff = diff[!is.na(diff)]
  num_diff = length(diff)
  
  # same cluster
  lira_top = prod(lira_same_cluster_pdf[names(table(diff)),]^table(diff))
  
  # pure chance
  lira_bottom = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))

  lira = log10(lira_top/lira_bottom)
  return(lira)
}

dnorm_diff <- function(x, mu, sigma){sqrt(2 / pi) / sigma * cosh(x * mu / sigma^2) * exp(-(x^2 + mu^2)/(2*sigma^2))}


# Lira gaussian variants 
lira_gaussian = function(x_u, x_v, sd_sc, sd_pc){
  
  z_u = (x_u - mean(x_u, na.rm = T))/sd(x_u, na.rm = T); 
  z_v = (x_v - mean(x_v, na.rm = T))/sd(x_v, na.rm = T)
  
  diff_z = abs(z_u - z_v)
  diff_z = diff_z[!is.na(diff_z)]
  num_diff_z = length(diff_z)
  
  # same cluster
  lira_top = prod(dnorm_diff(x = diff_z, mu = 0, sigma = sqrt(sum(c(sd_sc,sd_sc)^2))))
  # pure chance
  lira_bottom = prod(dnorm_diff(x = diff_z, mu = 0, sigma = sqrt(sum(c(sd_pc,sd_pc)^2))))
  
  lira = log10((lira_top/lira_bottom)^1)
  
  return(lira)
}

lira_gaussian = function(x_u, x_v, sd_sc, lira_pure_chance_pdf){
  
  z_u = (x_u - mean(x_u, na.rm = T))/sd(x_u, na.rm = T); 
  z_v = (x_v - mean(x_v, na.rm = T))/sd(x_v, na.rm = T)
  
  
  diff = abs(x_u - x_v)
  diff = diff[!is.na(diff)]
  num_diff = length(diff)
  diff_z = abs(z_u - z_v)
  diff_z = diff_z[!is.na(diff_z)]
  num_diff_z = length(diff_z)
  
  # same cluster
  lira_top = prod(dnorm_diff(x = diff_z, mu = 0, sigma = sqrt(sum(c(sd_sc,sd_sc)^2))))
  # pure chance
  #lira_bottom = prod(dnorm_diff(x = diff_z, mu = 0, sigma = sqrt(sum(c(sd_pc,sd_pc)^2))))
  
  # pure chance
  lira_bottom = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))
  
  
  lira = log10((lira_top/lira_bottom))
  
  return(lira)
}

lira_gaussian = function(x_u, x_v, sd_pc, lira_same_cluster_pdf){
  
  z_u = (x_u - mean(x_u, na.rm = T))/sd(x_u, na.rm = T); 
  z_v = (x_v - mean(x_v, na.rm = T))/sd(x_v, na.rm = T)
  
  
  diff = abs(x_u - x_v)
  diff = diff[!is.na(diff)]
  num_diff = length(diff)
  diff_z = abs(z_u - z_v)
  diff_z = diff_z[!is.na(diff_z)]
  num_diff_z = length(diff_z)
  
  # same cluster
  #lira_top = prod(dnorm_diff(x = diff_z, mu = 0, sigma = sqrt(sum(c(sd_sc,sd_sc)^2))))
  lira_top = prod(lira_same_cluster_pdf[names(table(diff)),]^table(diff))
  
  # pure chance
  lira_bottom = prod(dnorm_diff(x = diff_z, mu = 0, sigma = sqrt(sum(c(sd_pc,sd_pc)^2))))
  
  # pure chance
  #lira_bottom = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))
  
  
  lira = log10((lira_top/lira_bottom))
  
  return(lira)
}



lira_multinomial = function(x_u, x_v, multinomial_pure_chance_pdf, lira_same_cluster_pdf){
  
  diff = abs(x_u - x_v)
  diff = diff[!is.na(diff)]
  num_diff = length(diff)
  
  y_j = rep(0,nrow(lira_same_cluster_pdf)); #table(diff)
  y_j[as.numeric(names(table(diff)))+1] = table(diff)
  
  # same cluster
  g_vec_bottom = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))
  # pure chance
  g_vec_top = prod(gamma(y_j + alpha_star))/gamma(num_diff + sum(alpha_star))
  
  lira_multinomial = log10(g_vec_top/g_vec_bottom)
  
  return(lira_multinomial)
}

lira_pure_chance_distribution = function(V){
  V_grid = expand.grid(V, V)
  V_grid$diff = abs(V_grid$Var1 - V_grid$Var2)
  
  if(range(diff(V))[1] != range(diff(V))[2]){
    warning("Uneven spaced ratings")
  }
  
  stopifnot(sum(table(V_grid$diff)/length(V)^2) == 1)
  pcd = matrix(table(V_grid$diff)/length(V)^2)
  rownames(pcd) = V - rep(diff(V)[1],length(V))
  colnames(pcd) = "prob"
  
  return(pcd)
  
}

lira_same_cluster_distribution = function(V){
  
  if(range(diff(V))[1] != range(diff(V))[2]){
    warning("Uneven spaced ratings")
  }
  
  d = max(V)
  del_max = d - 2
  c_del = c()
  for(del in 0:del_max){
    #print((1/2)^(del + 1))  
    c_del[del+1] = (1/2)^(del + 1)
  }
  c_del[del_max+2] = 1 - sum(c_del)
  stopifnot(sum(c_del) == 1)
  
  scd = matrix(c_del, dimnames = list(V - rep(diff(V)[1],length(V)),"prob"))
  return(scd)
  
}

lira_binary_pure_chance_distribution = function(V = c(0,1)){
  V_grid = expand.grid(V, V)
  V_grid$diff = abs(V_grid$Var1 - V_grid$Var2)
  
  if(range(diff(V))[1] != range(diff(V))[2]){
    warning("Uneven spaced ratings")
  }
  
  stopifnot(sum(table(V_grid$diff)/length(V)^2) == 1)
  pcd = matrix(table(V_grid$diff)/length(V)^2)
  rownames(pcd) = c(0,1)
  colnames(pcd) = "prob"
  
  return(pcd)
}

lira_binary_same_cluster_distribution = function(V = c(0,1)){
  
  if(range(diff(V))[1] != range(diff(V))[2]){
    warning("Uneven spaced ratings")
  }
  del0 = (1/2)^(0+1)
  del1 = (1/2)^(1+1)  
  
  scd = c(del0, del1)
  leftover_prob = 1 - sum(scd)
  #leftover_prob/length(scd)
  
  scd = scd + leftover_prob/length(scd)
  # d = max(V)
  # del_max = d - 2
  # c_del = c()
  # for(del in 0:del_max){
  #   #print((1/2)^(del + 1))  
  #   c_del[del+1] = (1/2)^(del + 1)
  # }
  # c_del[del_max+2] = 1 - sum(c_del)
  stopifnot(sum(scd) == 1)
  
  scd = matrix(scd, dimnames = list(c(0,1),"prob"))
  return(scd)
  
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

