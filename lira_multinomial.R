## Relevant Packages
library(dplyr)
library(reshape2)

D = D_train

start = Sys.time()
iter = 10000
diff_vector = c()
for(i in 1:iter){

  user_pair = sample(x = unique(D$user), size = 2, replace = FALSE)
  user_pair_data = D %>% filter(user == user_pair[1] | user == user_pair[2])
  user_pair_matrix = dcast(data = user_pair_data, formula = user~item, value.var = "rating")
  user_pair_matrix = user_pair_matrix[,-1]
  
  diff = as.numeric(user_pair_matrix[1,] - user_pair_matrix[2,])
  diff = diff[!is.na(diff)]
  diff_vector = c(diff_vector,diff)
  
}

table(abs(diff_vector))/sum(table(abs(diff_vector)))
end = Sys.time()
print(end - start)

freq = table(abs(diff_vector))
freq = as.numeric(freq)
rating = sort(unique(D$rating), decreasing = F) - 1 

rating_data <- tibble(Rating = as.factor(rating),Freq = freq)

amplication_factor = 20
dirichlet_prior_values <- freq/sum(freq)*amplication_factor
multinomial_parameter_values <- rating_data$Freq/sum(rating_data$Freq)
alpha_star = dirichlet_prior_values + multinomial_parameter_values
