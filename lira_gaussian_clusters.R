############################
# LiRa - Gaussian Clusters # 
# 9/10/19 ################################
# A first legitimate computational pass ##
# LRT-based "proof of concept" to follow #
##########################################
source('~/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

# Sample vectors
x_1 = c(5,5,2,1,3,3,1)
x_2 = c(3,5,2,2,0,2,1)
x_3 = c(0,1,3,2,3,1,4)
x_4 = c(5,1,2,3,4,1,6)
rbind(x_1,x_2,x_3,x_4)

#var(c(x_1,x_2,x_3,x_4), na.rm = TRUE);var(c(c(x_1,x_2,x_3,x_4),-c(x_1,x_2,x_3,x_4)), na.rm = TRUE)

# All pairwise differences - very, very inefficient implementation
differences = c(x_1 - x_2, x_1 - x_3, x_2 - x_3, x_1 - x_4, x_2 - x_4, x_3 - x_4)
(v1 = var(differences, na.rm = TRUE)) # "population" variance


# Plotting Distribution of all pairwise differences -
plot(density(differences, na.rm = TRUE), ylim = c(0,1)); mean(differences, na.rm = TRUE)


# Selecting two vectors for a single pairwise comparison
x_u = x_1 
x_v = x_2 
(diff = x_u - x_v)
var(diff, na.rm = TRUE)


# Overlay of Gaussians 
lines(density(diff, na.rm = TRUE), col = "red")


# Summary & Similarity Measures
cat("\n", "Pop Var:", v1, "|", "Pairwise Var:", var(diff, na.rm = TRUE), "\n",
    "Cosine:", cosine_similarity(rbind(x_u,x_v))["x_u","x_v"], "\n",
    "Pearson:", cor(x_u, x_v, use = "pairwise.complete.obs"), "\n",
    "LiRa Unif:",lira(x_u, x_v, num_ratings = 5), "\n",
    "LiRa Gauss:", (v1/var(diff, na.rm = TRUE)), "\n") #^length(diff)



