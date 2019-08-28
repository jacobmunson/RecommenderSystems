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

coraters = as.matrix(ratings_matrix) %*% as.matrix(t(ratings_matrix))

coraters[coraters > 0] = 1 # reassigning positives to unity - this will operate as a indicator 
isSymmetric(coraters) # sanity check

coraters[1:15,1:15]


# Changing NAs to zeroes so routine similarity measures work as expected
ratings_matrix[is.na(ratings_matrix)] = 0 #NA
#ratings_matrix %>% rowwise() %>% summarise(sum())
#length(ratings_matrix)
#?rowwise

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

sapply(corated_vectors, FUN = function(x){nrow(x)*ncol(x)})



cat(sum(sapply(corated_vectors, FUN = function(x){nrow(x)*ncol(x)})), 
    "elements to evaluate out of", 
    nrow(ratings_matrix)*ncol(ratings_matrix),"total elements") 
# for small datasets this is redundant, but becomes advantageous with large sparse matrices


# Cosine Similarity
# check in RS Textbook, but usually on whole users (not just those co-rating)
# I think this is fine though, also helps with sparsity problems 
user_cosine_similarity = lapply(corated_vectors, FUN = cosine_similarity)

# Pearson Similarity uses 
# this isn't correct - should be as-in LiRa paper (also RS Textbook)
# i.e. get mean value for each user
user_pearson_similarity = lapply(lapply(corated_vectors, FUN = t), FUN = cor)

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
