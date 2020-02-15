################################
### Further LiRa Exploration ###
# 9/3/18 #######################
# Manipulating base score definition ###
# LiRa = log10(A/B)
# A = prob(diff{x_u,x_v} | same cluster)
# B = prob(diff{x_u,x_v} | pure chance)
# Idea: from original score (above)
# What if we mean-center the vectors? 
# Eventually: Redefine "cluster(s)" to be Guassian
################################

# Locally stored helper functions 
source('~/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

V = seq(0:5)
d = length(V)

pcd = pdf_on_differences_on_V(V)
# Some sample vectors 
# Data Points 
# (Nearly) Same vectors, but with missing values instead of 0s
y_u = c(1,1,0,0,0,5); y_u[y_u == 0] = NA; #y_u = mean_center_matrix(t(data.frame(y_u)))
y_v = c(2,1,3,0,0,2); y_v[y_v == 0] = NA; #y_v = mean_center_matrix(t(data.frame(y_v)))
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 0.5 PC
lira(x_u = y_u, x_v = y_v, num_ratings = d, lira_pure_chance_pdf = pcd) # 1.209

# Example set 1 from paper
y_u = c(1,1,0,0,0,2); y_u[y_u == 0] = NA; #y_u = mean_center_matrix(t(data.frame(y_u)))
y_v = c(1,1,0,0,0,2); y_v[y_v == 0] = NA; #y_v = mean_center_matrix(t(data.frame(y_v)))
V = seq(1:5);d = length(V)
pcd = pdf_on_differences_on_V(V)
lira(x_u = y_u, x_v = y_v, num_ratings = d, lira_pure_chance_pdf = pcd) # 1.19381
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 1 (perfect "match")

# Example set 2 from paper
y_u = c(1,1,5,4,4,2); y_u[y_u == 0] = NA; #y_u = mean_center_matrix(t(data.frame(y_u)))
y_v = c(1,1,5,4,4,2); y_v[y_v == 0] = NA; #y_v = mean_center_matrix(t(data.frame(y_v)))
lira(x_u = y_u, x_v = y_v, num_ratings = 5, lira_pure_chance_pdf = pcd) # 2.38764
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 1 (perfect "match" still - but literally twice as much observed data)

# Another Example with Big Differences
y_u = c(1,5,3,4,4,5); y_u[y_u == 0] = NA; #y_u = mean_center_matrix(t(data.frame(y_u)))
y_v = c(1,1,5,4,4,1); y_v[y_v == 0] = NA; #y_v = mean_center_matrix(t(data.frame(y_v)))
lira(x_u = y_u, x_v = y_v, num_ratings = 5, lira_pure_chance_pdf = pcd) # 1.298159 
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # -0.04756515




