############################
# LiRa - Gaussian Clusters # 
# 9/10/19 ################################
# A first legitimate computational pass ##
# LRT-based "proof of concept" to follow #
##########################################
source('~/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

# Sample vectors
x_1 = c(3,5,2,1,NA,3,1)
x_2 = c(3,5,2,2,0,2,1)
x_3 = c(0,1,3,2,0,1,4)
x_4 = c(5,1,2,3,4,1,6)
x_5 = c(1,2,3,4,5,5,2)
x_6 = c(1,2,1,1,NA,1,1)
x_7 = c(2,2,1,3,4,1,6)
x_8 = c(3,NA,NA,4,0,2,5)
x_9 = c(1,2,NA,3,3,4,2)
x_10 = c(5,5,5,5,5,2,2)
x_11 = c(5,5,5,5,5,2,2)
user_df = rbind(x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9,x_10,x_11)

#var(c(x_1,x_2,x_3,x_4), na.rm = TRUE);var(c(c(x_1,x_2,x_3,x_4),-c(x_1,x_2,x_3,x_4)), na.rm = TRUE)

# All pairwise differences - very, very inefficient implementation

# differences = c(x_1 - x_2, x_1 - x_3, x_2 - x_3, x_1 - x_4, x_2 - x_4, x_3 - x_4)
# (v1 = var(differences, na.rm = TRUE)) # "population" variance


iter = 500000
diff_df = matrix(data = 0, nrow = iter, ncol = ncol(user_df))
for(i in 1:iter){
  x = user_df[sample(x = 1:nrow(user_df), size = 2, replace = FALSE),]
  diff_df[i,1:ncol(x)] = x[1,] - x[2,]
}
(mu_pop = mean(c(diff_df), na.rm = TRUE)); (v_pop = var(c(diff_df), na.rm = TRUE)); (sd_pop = sd(c(diff_df), na.rm = TRUE))

# Plotting Distribution of all pairwise differences -
plot(density(rnorm(n = 100000, mean = mu_pop, sd = sd_pop)), ylim = c(0,1))


# Selecting two vectors for a single pairwise comparison

# 7,1 is interesting
random_users = sample(1:nrow(user_df), size = 2, replace = FALSE)
x_u = user_df[random_users[1],]; x_v = user_df[random_users[2],]
x_u = user_df[10,]; x_v = user_df[11,]; 
(diff = x_u - x_v)
var(diff, na.rm = TRUE)


# Overlay of Gaussians 
lines(density(diff, na.rm = TRUE), col = "red")


# Summary & Similarity Measures
cat("\n","User u:", x_u ,"\n",
    "User v:", x_v ,"\n",
    "Pop Var:", v_pop, "|",
    "Pairwise Var:", var(diff, na.rm = TRUE), "\n",
    "Cosine:", cosine_similarity(rbind(x_u,x_v))["x_u","x_v"], "\n",
    "Pearson:", cor(x_u, x_v, use = "pairwise.complete.obs"), "\n",
    "LiRa Unif:",lira(x_u, x_v, num_ratings = 5), "\n",
    "LiRa Gauss:", (v_pop/var(diff, na.rm = TRUE)), "\n") #^length(diff)


