### this file has largely turn into scraps for now
# working on this file stored locally - so it will be a mess often
# Sample fictitious dataset
u1 = c(1,3,NA,NA,4,5,NA)
u2 = c(2,NA,5,NA,NA,NA,NA)
u3 = c(NA,4,NA,4,NA,2,NA)
u4 = c(2,4,NA,NA,NA,NA,NA)
u5 = c(2,1,4,NA,NA,NA,NA)
u6 = c(NA,NA,NA,NA,NA,2,4)
ratings_matrix = t(data.frame(u1,u2,u3,u4,u5,u6))
ratings_matrix[1:15,1:15] # Customer by ratings matrix 
ratings_matrix[is.na(ratings_matrix)] = 0
ratings_matrix[1:15,1:15]

coraters = as.matrix(ratings_matrix) %*% as.matrix(t(ratings_matrix))

coraters[coraters > 0] = 1 # reassigning positives to unity - this will operate as a indicator 

#coraters = ratings_matrix %*% t(ratings_matrix) # these have to be matrices
coraters[coraters > 0] = 1 # reassigning positives to unity

isSymmetric(coraters) # sanity check
coraters[1:15,1:15]

coraters[1:15,1:15]


# Changing NAs to zeroes so routine similarity measures work as expected
ratings_matrix[is.na(ratings_matrix)] = 0 #NA

#ratings_matrix[is.na(ratings_matrix)] = 0 #NA
#ratings_matrix %>% rowwise() %>% summarise(sum())
#length(ratings_matrix)
#?rowwise

sparsity = function(matrix){
  sparsity = length(which(matrix == 0))/(nrow(matrix)*ncol(matrix))
  return(sparsity)
}

sparsity(ratings_matrix)

cosine_similarity = function(matrix){
  cos_sim = matrix/sqrt(rowSums(matrix * matrix))
  cos_sim = cos_sim %*% t(cos_sim)
  return(cos_sim)
}

#ratings_matrix[which(coraters[1,] == 1),] %*% t(ratings_matrix[which(coraters[1,] == 1),])
#ratings_matrix[which(coraters == 1),]

#cosine_similarity(ratings_matrix[which(coraters[1,] == 1),1],ratings_matrix[which(coraters[1,] == 1),2])

#apply(ratings_matrix[which(coraters == 1),], MARGIN = 1, FUN = cosine_similarity())
#apply(ratings_matrix, MARGIN = c(1,2), FUN = cosine_similarity())


# Users that have co-rated items (on a per-user basis)
user_ratings = apply(coraters, MARGIN = 1, FUN = function(x)(which(x == 1)))
user_ratings$`1`

# Those above users evaluated in the ratings matrix
corated_vectors = sapply(X = user_ratings, FUN = function(x){ratings_matrix[x,]})

corated_vectors

#sapply(corated_vectors, FUN = function(x){nrow(x)*ncol(x)})



cat(sum(sapply(corated_vectors, FUN = function(x){nrow(x)*ncol(x)})), 
    "elements to evaluate out of", 
    nrow(ratings_matrix)*ncol(ratings_matrix),"total elements") 
Sys.sleep(1)
# for small datasets this is redundant, but becomes advantageous with large sparse matrices

#user_ratings = apply(coraters, MARGIN = 1, FUN = function(x)(unique(which(x == 1))))


corated_vectors = sapply(X = user_ratings, FUN = function(x){ratings_matrix[x,]})
corated_vectors$`1`[1:10,1:16]
#corated_vectors = sapply(X = user_ratings, FUN = function(x){ratings_matrix[x,]})


# Cosine Similarity
# check in RS Textbook, but usually on all users (not just those co-rating user, but non-corating users just go to 0 anyways)
# This is fine though, also helps with sparsity problems - in the really large scale at least
user_cosine_similarity = lapply(corated_vectors, FUN = cosine_similarity)



user_cosine_similarity$`2`[1:15,1:10]

lapply(corated_vectors, FUN = dim)


test_ps = cor(t(ratings_matrix))
test_cs = cosine_similarity(matrix = ratings_matrix)
test_ps[1:5,1:13]

dim(test_ps); dim(test_cs)

### Pearson Similarity  
# this isn't correct - should be as-in LiRa paper (also RS Textbook)
# i.e. get mean value for each user
# Correct: 
pearson_similarity_matrix = matrix(data = 0, 
                                    nrow = length(unique(D$userId)), 
                                    ncol = length(unique(D$userId)), 
                                    dimnames = list(unique(D$userId),unique(D$userId)))

# User Averages

user_averages = apply(ratings_matrix, MARGIN = 1, FUN = function(x){mean(x[which(x != 0)])})
user_averages[1:10]

for(u in 1:length(unique(D$userId))){ # first time ever indexing for loops on u,v
  for(v in 1:length(unique(D$userId))){ # just following notation from RS Textbook
    if(u > v){
      user_u_ratings = ratings_matrix[which(rownames(ratings_matrix) == u),]
      user_v_ratings = ratings_matrix[which(rownames(ratings_matrix) == v),]
      
      user_uv_ratings = which(user_u_ratings %*% t(user_v_ratings) != 0) # nonzero elements at places both have rated
      
      r_uk = user_u_ratings[user_uv_ratings]
      r_vk = user_v_ratings[user_uv_ratings]
      
      pc_num = sum((r_uk - user_averages[which(names(user_averages) == u)]) * (r_vk - user_averages[which(names(user_averages) == v)]))
      
      pc_denom = sqrt(sum((r_uk - user_averages[which(names(user_averages) == u)])^2)) * sqrt(sum((r_vk - user_averages[which(names(user_averages) == v)])^2))
      
      
      pc = pc_num/pc_denom
      pearson_similarity_matrix[u,v] = pc
      cat("\n","u:",u,"v:",v)
    }
  }
}


pc = matrix(data = 0, 
            nrow = length(unique(D$userId)), 
            ncol = length(unique(D$userId)), 
            dimnames = list(unique(D$userId),unique(D$userId)))




pearson_similarity_matrix[1:50,1:70]

test_pc = lapply(1:length(unique(D$userId)), outer_function)

outer_function = function(u){ # first time ever indexing for loops on u,v
  
  lapply(1:length(unique(D$userId)), inner_function)
}

inner_function = function(v){ # just following notation from RS Textbook
  if(u > v){
    user_u_ratings = ratings_matrix[which(rownames(ratings_matrix) == u),]
    user_v_ratings = ratings_matrix[which(rownames(ratings_matrix) == v),]
    
    user_uv_ratings = which(user_u_ratings %*% t(user_v_ratings) != 0) # nonzero elements are places both have rated
    
    r_uk = user_u_ratings[user_uv_ratings]
    r_vk = user_v_ratings[user_uv_ratings]
    
    pc_num = sum((r_uk - user_averages[which(names(user_averages) == u)]) * (r_vk - user_averages[which(names(user_averages) == v)]))
    
    pc_denom = sqrt(sum((r_uk - user_averages[which(names(user_averages) == u)])^2)) * sqrt(sum((r_vk - user_averages[which(names(user_averages) == v)])^2))
    
    
    pc = pc_num/pc_denom
    return(pc)
    #pearson_similarity_matrix[u,v] = pc
    #cat("\n","u:",u,"v:",v)
  }
}




inner_function = function(v){      
    user_u_ratings = ratings_matrix[which(rownames(ratings_matrix) == u),]
    user_v_ratings = ratings_matrix[which(rownames(ratings_matrix) == v),]
    
    user_uv_ratings = which(user_u_ratings %*% t(user_v_ratings) != 0) # nonzero elements are places both have rated
    
    r_uk = user_u_ratings[user_uv_ratings]
    r_vk = user_v_ratings[user_uv_ratings]
    
    pc_num = sum((r_uk - user_averages[which(names(user_averages) == u)]) * (r_vk - user_averages[which(names(user_averages) == v)]))
    
    pc_denom = sqrt(sum((r_uk - user_averages[which(names(user_averages) == u)])^2)) * sqrt(sum((r_vk - user_averages[which(names(user_averages) == v)])^2))
    
    
    pc = pc_num/pc_denom
    return(pc)
    #pearson_similarity_matrix[u,v] = pc
    #cat("\n","u:",u,"v:",v)
}

outer_function = function(u){inner_function(v)}

sapply(X = )


sapply(X = 1:length(unique(D$userId)), FUN = inner_function)
Vectorize(inner_function, SIMPLIFY = TRUE)

inner_function(pearson_similarity_matrix)

apply(X = pearson_similarity_matrix, MARGIN = 1:2, FUN = inner_function)

outer_function = function(u){}

sapply(1:length(unique(D$userId)), inner_function)


user_averages[1:5]

mean(ratings_matrix[2,][which(ratings_matrix[2,] != 0)])

pearson_similarity_matrix[1:15,1:15]




test = lapply(corated_vectors, FUN = t)

user_pearson_similarity = lapply(test, FUN = cor)
user_pearson_similarity = lapply(lapply(corated_vectors, FUN = t), FUN = cor) # this needs to be totally scrapped, working on a better implementation

user_pearson_similarity$`1`[1:5,1:10]

str(user_cosine_similarity)

sim_msr = user_cosine_similarity

sort(user_cosine_similarity$u1[1,], decreasing = TRUE)[-1] # [1:k]

users = names(sim_msr)

extract_users = function(i){lapply(sim_msr[i], FUN = function(x){which(users[i] == rownames(x))})}
user_rows = sapply(1:length(users), extract_users)
user_rows = unlist(user_rows)

extract_user_rows = function(i){lapply(sim_msr[i], FUN = function(x){x[user_rows[i],]})}
extracted_user_similarities = sapply(1:length(user_rows), extract_user_rows)

extracted_user_similarities
lapply(extracted_user_similarities, FUN = sort, decreasing = TRUE)



# LiRa Score

# d = number of discrete values
# del = 1,..,d-1
#p(diff x_u,x_v | pure chance) = 
#p(diff x_u,x_v | same cluster) = 


log10((1/2)^3/(1/5)^3)


sapply(X = rating_vectors, FUN = function(x){(x %*% t(x))}) # / sqrt(sum(x^2) * sum(t(x)^2))

sqrt(rowSums(x^2)*rowSums(t(x)^2))
#sapply(rating_vectors, MARGIN = 1, FUN = cosine_similarity(vec1 = rating_vectors,vec2 = t(rating_vectors)))




# On a Per Item Basis, Extract Users that have Co-rated  
rated_items = apply(X = ratings_matrix, MARGIN = 1, function(x) (which(!is.na(x) == TRUE)))
#corated_items = apply(X = ratings_matrix, MARGIN = 2, function(x) (intersect(x,)))



normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
                    
                    
                    
                    
                    
                    customer_pairs = t(combn(unique(df$users), m = 2))
#customer_pairs = expand.grid(unique(df$users),unique(df$users))
#customer_pairs = customer_pairs[customer_pairs$Var1 != customer_pairs$Var2,]
A = sapply(1:nrow(customer_pairs), FUN = pairwise_similarities) # sapply worked
cbind(customer_pairs,unlist(A))

pairwise_similarities = function(i){
  #i = 4
  u1_i = dcast(data = df[which(df$users == customer_pairs[i,1]),], formula = users~movies, value.var = "ratings")
  u2_i = dcast(data = df[which(df$users == customer_pairs[i,2]),], formula = users~movies, value.var = "ratings")
  u1_i = u1_i[,-1]; u2_i = u2_i[,-1]; #
  (items_common = intersect(names(u1_i),names(u2_i)))

  if(length(items_common) != 0){
    
    u1_i = u1_i[,items_common]; u2_i = u2_i[,items_common]
    u1_i_mu = mean(as.numeric(u1_i)); #print(u1_i_mu)
    u2_i_mu = mean(as.numeric(u2_i)); #print(u2_i_mu)
    u1_i = u1_i - u1_i_mu; u2_i = u2_i - u2_i_mu
    
    cor = sum((u1_i) * (u2_i))/(sqrt(sum(u1_i^2))*sqrt(sum(u2_i^2)))

    return(cor)
  }else(return(NULL))

} 
