
# Sample fictitious dataset
u1 = c(1,3,NA,NA,4,5,NA)
u2 = c(2,NA,5,NA,NA,NA,NA)
u3 = c(NA,4,NA,4,NA,2,NA)
u4 = c(2,4,NA,NA,NA,NA,NA)
u5 = c(2,1,4,NA,NA,NA,NA)
u6 = c(NA,NA,NA,NA,NA,2,4)
ratings_matrix = data.frame(u1,u2,u3,u4,u5,u6)
print(ratings_matrix)


cor(ratings_matrix, use = 'pairwise.complete.obs')

### A Variety of Basic Imputation Techniques 


# Replace NA with 0 
ratings_matrix_zero_impute = ratings_matrix
ratings_matrix_zero_impute[is.na(ratings_matrix_zero_impute)] = 0
cor(ratings_matrix_zero_impute, use = 'pairwise.complete.obs')


# Replace NA with User-Mean
ratings_matrix_mean_impute = ratings_matrix
ratings_matrix_mean_impute = apply(X = ratings_matrix_mean_impute, MARGIN = 2, FUN = impute_mean)
cor(ratings_matrix_mean_impute)


# Replace NA with User-Median
ratings_matrix_median_impute = ratings_matrix
ratings_matrix_median_impute = apply(X = ratings_matrix_median_impute, MARGIN = 2, FUN = impute_median)
cor(ratings_matrix_median_impute)


# Replace NA with User-Min
ratings_matrix_min_impute = ratings_matrix
ratings_matrix_min_impute = apply(X = ratings_matrix_min_impute, MARGIN = 2, FUN = impute_min)
cor(ratings_matrix_min_impute)


# Replace NA with User-Max (I can't think of a justifiable reason for actually doing this)
ratings_matrix_max_impute = ratings_matrix
ratings_matrix_max_impute = apply(X = ratings_matrix_max_impute, MARGIN = 2, FUN = impute_max)
cor(ratings_matrix_max_impute)

