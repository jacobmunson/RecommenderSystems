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
  diff_table = table(diff)
  # same cluster
  lira_top = prod(lira_same_cluster_pdf[names(diff_table),]^diff_table)
  
  # pure chance
  lira_bottom = prod(lira_pure_chance_pdf[names(diff_table),]^diff_table)
  
  lira = log10(lira_top/lira_bottom)
  return(lira)
}

dnorm_diff <- function(x, mu, sigma){sqrt(2 / pi) / sigma * cosh(x * mu / sigma^2) * exp(-(x^2 + mu^2)/(2*sigma^2))}


# Lira LRT Variant
lira_lrt = function(x_u, x_v, sd_pop){
  diff = (x_u - x_v)
  #n = length(diff);
  sd_diff = sd(diff, na.rm = TRUE)
  #sqrt(n/2)*(v/v_pop - 1 - log(v/v_pop))
  
  return((sd_pop/sd_diff)) # ^n
}


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



lira_multinomial = function(x_u, x_v, alpha_star, lira_same_cluster_pdf){
  
  diff = abs(x_u - x_v)
  diff = diff[!is.na(diff)]
  num_diff = length(diff)
  
  y_j = rep(0,nrow(lira_same_cluster_pdf)); #table(diff)
  y_j[as.numeric(names(table(diff)))+1] = table(diff)
  
  # same cluster
  g_vec_sc = prod(gamma(y_j + alpha_star))/gamma(num_diff + sum(alpha_star))
  #g_vec_bottom = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))
  
  # pure chance
  g_vec_pc = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))
  #g_vec_top = prod(gamma(y_j + alpha_star))/gamma(num_diff + sum(alpha_star))
  
  lira_multinomial = log10(g_vec_sc/g_vec_pc) #log10(g_vec_bottom/g_vec_top)
  
  return(lira_multinomial)
}

lira_multinomial_gaussian = function(x_u, x_v, alpha_star, lira_same_cluster_pdf){
  
  
  z_u = (x_u - mean(x_u, na.rm = T))/sd(x_u, na.rm = T); 
  z_v = (x_v - mean(x_v, na.rm = T))/sd(x_v, na.rm = T)
  
  diff = abs(x_u - x_v)
  diff = diff[!is.na(diff)]
  num_diff = length(diff)
  
  diff_z = abs(z_u - z_v)
  diff_z = diff_z[!is.na(diff_z)]
  
  
  y_j = rep(0,nrow(lira_same_cluster_pdf)); #table(diff)
  y_j[as.numeric(names(table(diff)))+1] = table(diff)
  
  # same cluster
  g_vec_sc = prod(gamma(y_j + alpha_star))/gamma(num_diff + sum(alpha_star))
  
  # pure chance
  lira_bottom = prod(dnorm_diff(x = diff_z, mu = 0, sigma = sqrt(sum(c(sd_pc,sd_pc)^2))))
  
  
  lira_mng = log10(g_vec_sc/lira_bottom) #log10(g_vec_bottom/g_vec_top)
  
  return(lira_mng)
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
  
  d = length(V) # changed from max(V) to work with non-integer ratings
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
  similarity_vector = similarity_vector[!is.na(similarity_vector)]
  #mae_nn = c(); mae_knn = c()
  if(length(similarity_vector) == 0 | any(is.na(similarity_vector))){mae_nn = NA; mae_knn = NA}else{
    
    neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
    neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
    
    pred_rating_nn = sum(neighbor_ratings$rating * neighbor_ratings$y)/sum(abs(neighbor_ratings$y)) # is this supposed to be /|abs(sim)|
    mae_nn = abs(pred_rating_nn - test_set$rating)
    
    pred_rating_knn = mean(neighbor_ratings$rating)
    mae_knn = abs(pred_rating_knn - test_set$rating)
  }
  return(c(mae_nn, mae_knn)) # weighted, unweighted
}

nearest_neighbors_trimming_function = function(similarity_vector_with_self_similarity, 
                                               positive_only = F, scale_similarity_by_max = F, 
                                               normalize_similarity = F,
                                               min_similarity = NA, 
                                               mean_scaling = F, sd_scaling = F, quantile_trim = F,
                                               mu_scale = NA, sd_scale = NA, quantile_scale = NA){
  sv = similarity_vector_with_self_similarity
  sv = sv[,order(sv, decreasing = TRUE)]
  sv = sv[-1]
  
  if(positive_only){sv = sv[sv > 0]}
  
  if(scale_similarity_by_max){sv = sv/max(sv)}
  
  if(normalize_similarity){sv = (sv - mean(sv))/sd(sv)}  
  
  if(quantile_trim){sv = sv[sv > quantile(sv, probs = quantile_scale, na.rm = T)]}
  
  if(is.numeric(min_similarity)){
    if(any(sv) > min_similarity){
      
      if(mean_scaling | sd_scaling){
        if(mean_scaling & sd_scaling){
          stopifnot(is.numeric(mu_scale))
          stopifnot(is.numeric(sd_scale))
          sv = sv[sv > mu_scale*mean(sv) + sd_scale*sd(sv)]
        }else{
          sv = sv[sv > mu_scale*mean(sv)]
        }
      }
      
      
      
      # operations on vector but only if there's meeting of a threshold  
    }
    
  }else{
    
    if(mean_scaling | sd_scaling){
      if(mean_scaling & sd_scaling){
        stopifnot(is.numeric(mu_scale))
        stopifnot(is.numeric(sd_scale))
        sv = sv[sv > mu_scale*mean(sv) + sd_scale*sd(sv)]
      }else{
        sv = sv[sv > mu_scale*mean(sv)]
      }
    }
    
    
  }
  
  
  sv = sv[is.finite(sv)]
  
  return(sv)
}


compute_neighbor_similarity = function(user_item_matrix, test_observation, similarity_measure){
  similarity_matrix = matrix(data = NA, nrow = 1, ncol = nrow(user_item_matrix))
  
  stopifnot(similarity_measure %in% c("lira_uniform",
                                      "lira_gaussian_pure_chance",
                                      "pearson_pwc",
                                      "pearson_impute_zero",
                                      "cosine"))
  
  if(similarity_measure == "lira_uniform"){
    for(u in 1:nrow(user_item_matrix)){
      similarity_matrix[1,u] = lira(x_u = user_item_matrix[which(rownames(user_item_matrix) == test_observation$user),], 
                                    x_v = user_item_matrix[u,], 
                                    lira_same_cluster_pdf = lira_same_cluster_pdf, 
                                    lira_pure_chance_pdf = lira_pure_chance_pdf)
    }
    colnames(similarity_matrix) = rownames(user_item_matrix)
  }
  
  if(similarity_measure == "lira_gaussian_pure_chance"){
    for(u in 1:nrow(user_item_matrix)){
      similarity_matrix[1,u] = lira_gaussian(x_u = user_item_matrix[which(rownames(user_item_matrix) == test_observation$user),], 
                                             x_v = user_item_matrix[u,], 
                                             lira_same_cluster_pdf = lira_same_cluster_pdf, 
                                             sd_pc = sd_pc)
    }
    colnames(similarity_matrix) = rownames(user_item_matrix)
  }
  
  if(similarity_measure == "pearson_pwc"){
    similarity_matrix = cor(t(user_item_matrix), use = "pairwise.complete.obs")
    similarity_matrix = similarity_matrix[which(rownames(similarity_matrix) == test_observation$user),]
  }
  
  if(similarity_measure == "pearson_impute_zero"){
    user_item_matrix[is.na(user_item_matrix)] = 0
    similarity_matrix = cor(t(user_item_matrix))
    similarity_matrix = similarity_matrix[which(rownames(similarity_matrix) == test_observation$user),]
  }
  
  if(similarity_measure == "cosine"){
    user_item_matrix[is.na(user_item_matrix)] = 0
    similarity_matrix = cosine_similarity(matrix = user_item_matrix)
    similarity_matrix[is.nan(similarity_matrix)] = 0
    similarity_matrix = similarity_matrix[which(rownames(similarity_matrix) == test_observation$user),]
    similarity_matrix = t(similarity_matrix) # just for formatting where this gets consumed elsewhere
  }
  
  
  
  
  
  return(similarity_matrix)
}

lira_lrt_sd_sampling = function(dataset, iter){
  
  if(!("tidyverse" %in% (.packages()))){
    library(tidyverse) 
  }
  if(!("reshape2" %in% (.packages()))){
    library(tidyverse) 
  }
  
  diff_vector = c()
  start = Sys.time()
  for(i in 1:iter){
    user_pair = sample(x = unique(dataset$user), size = 2, replace = FALSE)
    user_pair_data = dataset %>% filter(user == user_pair[1] | user == user_pair[2])
    user_pair_matrix = dcast(data = user_pair_data, formula = user ~ item, value.var = "rating")
    user_pair_matrix = user_pair_matrix[,-1]
    diff_vector = c(diff_vector,as.numeric(user_pair_matrix[1,] - user_pair_matrix[2,]))
    
  }
  end = Sys.time()
  print(end - start)
  
  sd_pop = sd((diff_vector), na.rm = TRUE)
  print(sd_pop)
  
  return(sd_pop)
  
}

