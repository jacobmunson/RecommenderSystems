## Relevant Packages
library(dplyr)
library(readr)
library(reshape2)
library(doParallel)
library(ggplot2)


D <- read.table("Recommender Systems - Home Folder/ml-100k/u1.base")
colnames(D) = c("user","item","rating","timestamp")

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

diff_100 = c(0.31993569, 0.42845659, 0.19453376, 0.04421222, 0.01286174)
diff_500 = c(0.29827819, 0.43585415, 0.18686698, 0.06245780, 0.01654288) 
diff_1000 = c(0.29327416, 0.44281325, 0.18982588, 0.06051553, 0.01357118) 
diff_10000 = c(0.30707282, 0.43407234, 0.18742931, 0.05774251, 0.01368302) 
diff_100000 = c(0.30371389, 0.43270437, 0.18996273, 0.05957268, 0.01404633) 

tibble(diff = sort(unique(D$rating), decreasing = F) - 1,diff_100,diff_500,diff_1000,diff_10000,diff_100000)


freq = table(abs(diff_vector))
freq = as.numeric(freq)
rating = sort(unique(D$rating), decreasing = F) - 1 

rating_data <- tibble(Rating = as.factor(rating),
                      Freq = freq)
rating_data
rating_data %>%
  ggplot(aes(x = Rating, y = Freq)) +
  geom_col()

#### Changing "Basis" of Ratings ####

altered_basis_rating_data <- rating_data %>%
  mutate(Rating = rating) #as.factor(2*as.numeric(rating) - 1))

altered_basis_rating_data %>%
  ggplot(aes(x = Rating, y = Freq)) +
  geom_col()

#### Overlay Binomial Distributed Sample ####

simulated_ratings <- tibble(SimulatedRating = rbinom(n = sum(freq), size = length(rating)-1, prob = 0.3)) %>%
  group_by(SimulatedRating) %>%
  tally(name = 'Freq')

altered_basis_rating_data %>%
  ggplot(aes(x = Rating, y = Freq)) +
  geom_col() +
  geom_col(aes(x = SimulatedRating, y = Freq), data=simulated_ratings, fill='Blue', alpha=0.25)

#### Fitting Data to Multinomial Distribution ####

dirichlet_prior_values <- freq/sum(freq)#rep(1, 5)/5
multinomial_parameter_values <- rating_data$Freq/sum(rating_data$Freq)
updated_multinomial_parameter_values <- dirichlet_prior_values + multinomial_parameter_values

#### Overlay Multinomial Distributed Sample ####

simulated_multinomial_rating_values <-
  rmultinom(n = sum(freq), 1, updated_multinomial_parameter_values) %>%
  t() %>%
  colSums()

simulated_multinomial_ratings <-
  tibble(Rating = rating,#as.factor(2*as.numeric(rating) - 1),
         Freq = simulated_multinomial_rating_values)

altered_basis_rating_data %>%
  ggplot(aes(x = Rating, y = Freq)) +
  geom_col() +
  geom_col(aes(x = Rating, y = Freq), data=simulated_multinomial_ratings, fill='Blue', alpha=0.25)


bind_cols(altered_basis = altered_basis_rating_data, multi_sim = simulated_multinomial_ratings)



bind_cols(ratings = simulated_multinomial_ratings$Rating, 
          multi_sim_ratings = simulated_multinomial_ratings$Freq/sum(freq),
          binom_sim_ratings = simulated_ratings$Freq/sum(simulated_ratings$Freq),
          original_freq = freq/sum(freq))


####

l_vec = c(); g_vec = c()
alpha_star = updated_multinomial_parameter_values
lira_pure_chance_pdf = lira_pure_chance_distribution(V = seq(1:5))

for(i in 1:500){

  print(i)
  user_pair = sample(x = unique(D$user), size = 2, replace = FALSE)
  user_pair_data = D %>% filter(user == user_pair[1] | user == user_pair[2])
  user_pair_matrix = dcast(data = user_pair_data, formula = user~item, value.var = "rating")
  user_pair_matrix = user_pair_matrix[,-1]
  ####
  
  # diff = abs(user_pair_matrix[1,] - user_pair_matrix[2,])
  # diff = diff[!is.na(diff)]
  # n = length(diff)
  # 
  # y_j = rep(0,5); #table(diff)
  # y_j[as.numeric(names(table(diff)))+1] = table(diff)
  # 
  # g_vec_bottom = prod(lira_pure_chance_pdf[names(table(diff)),]^table(diff))
  # g_vec_top = prod(gamma(y_j + alpha_star))/gamma(n + sum(alpha_star))
  # 
  # g_vec[i] = log10(g_vec_top/g_vec_bottom)

  g_vec[i] = lira_multinomial(x_u = user_pair_matrix[1,], 
                              x_v = user_pair_matrix[2,],
                              multinomial_pure_chance_pdf = alpha_star, 
                              lira_same_cluster_pdf = lira_same_cluster_distribution(V = seq(1:5)))
     
  l_vec[i] = lira(x_u = user_pair_matrix[1,], 
                  x_v = user_pair_matrix[2,], 
                  lira_pure_chance_pdf = lira_pure_chance_distribution(V = seq(1:5)),
                  lira_same_cluster_pdf = lira_same_cluster_distribution(V = seq(1:5)))
            
  
}
round(cbind(l_vec, g_vec,abs(l_vec - g_vec)),3)
plot(abs(l_vec - g_vec))


##############################

