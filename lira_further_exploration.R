################################
### Further LiRa Exploration ###
# 9/3/18 #######################
# Manipulating base score definition ###
# LiRa = log10(A/B)
# A = prob(diff{x_u,x_v} | same cluster)
# B = prob(diff{x_u,x_v} | pure chance)
# Idea: from original score (above)
# What if we mean-center the vectors? 
# 
# Redefine "cluster(s)" to be Guassian
################################

# Locally stored helper functions 
source('~/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

# Some sample vectors 
# Data Points 
# (Nearly) Same vectors, but with missing values instead of 0s
y_u = c(1,1,0,0,0,5); y_u[y_u == 0] = NA; y_u = mean_center_matrix(t(data.frame(y_u)))
y_v = c(2,1,3,0,0,2); y_v[y_v == 0] = NA; y_v = mean_center_matrix(t(data.frame(y_v)))
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 0.5 PC
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 1.19382

# Example set 1 from paper
y_u = c(1,1,0,0,0,2); y_u[y_u == 0] = NA; y_u = mean_center_matrix(t(data.frame(y_u)))
y_v = c(1,1,0,0,0,2); y_v[y_v == 0] = NA; y_v = mean_center_matrix(t(data.frame(y_v)))
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 1.19381
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 1 (perfect "match")

# Example set 2 from paper
y_u = c(1,1,5,4,4,2); y_u[y_u == 0] = NA; y_u = mean_center_matrix(t(data.frame(y_u)))
y_v = c(1,1,5,4,4,2); y_v[y_v == 0] = NA; y_v = mean_center_matrix(t(data.frame(y_v)))
lira(x_u = y_u, x_v = y_v, num_ratings = d) # 2.38764
cor(x = y_u, y = y_v, use = "pairwise.complete.obs") # 1 (perfect "match" still - but literally twice as much observed data)


### Difference in Normal Distributions
# Reference materials
# https://stats.stackexchange.com/questions/186463/distribution-of-difference-between-two-normal-distributions
# http://mathworld.wolfram.com/NormalDifferenceDistribution.html

# Specify parameters
mu <- c(0, 0)
sigma <- c(1, 1)
# Simulate data
n.sim <- 1e5; set.seed(17)
x.sim <- matrix(rnorm(n.sim*2, mu, sigma), nrow=2)
x <- abs(x.sim[2, ] - x.sim[1, ])
# Display the results
hist(x, freq=FALSE)
f <- function(x, mu, sigma) {sqrt(2 / pi) / sigma * cosh(x * mu / sigma^2) * exp(-(x^2 + mu^2)/(2*sigma^2))}
curve(f(x, abs(diff(mu)), sqrt(sum(sigma^2))), lwd=2, col="orange", add=TRUE, lty = 3)
lines(density(abs(rnorm(n = 1e5, mean = 0, sd = sqrt(2))), from = 0), lty = 2, col = 'green')

#abline(v = dnorm(x = 0.05, mean = 0, sd = sqrt(2)), col = "orange")
#norm(x = 0.5, mean = 0, sd = sqrt(2))
#abline(v = qnorm(p = 0.05, mean = 0, sd = sqrt(2), lower.tail = FALSE), col = "pink")

qnorm(p = 0.5, mean = 0, sd = sqrt(2), lower.tail = FALSE)

val = 0.05

abline(h = dnorm(x = 0.05, mean = 0, sd = sqrt(2)), col = "purple")
abline(v = val, col = "purple")
dnorm(x = 0.1, mean = 0, sd = sqrt(2))
dnorm(x = )
pnorm()

x_u = c(1,1,2,2,NA,NA,2,2,NA,3,NA,3,3,1,2,1)
x_v = c(5,1,2,2,5,1,2,5,2,5,4,5,4,5,4,5)
lira(x_u = x_u, x_v = x_v, num_ratings = 5) # 1.98
cor(x_u, x_v, use = "pairwise.complete.obs")
x_u = (x_u - mean(x_u, na.rm = TRUE))/sd(x_u, na.rm = TRUE)
x_v = (x_v - mean(x_v, na.rm = TRUE))/sd(x_v, na.rm = TRUE)

x_diff = abs(x_u - x_v)
#(lira_top = sum(dnorm(x = x_diff, mean = 0, sd = sqrt(2)), na.rm = TRUE))
(lira_top = prod(dnorm(x = x_diff, mean = 0, sd = sqrt(2)), na.rm = TRUE))

num_ratings = 5
num_diff = length(which(!is.na(abs(x_u - x_v))))
(lira_bottom = (1/num_ratings)^num_diff)
(lira_score = log10(lira_top/lira_bottom)) 

#lira_top = 0.5^(num_diff)


#qnorm(p = c(0.01,0.1), mean = 0, sd = sqrt(2), lower.tail = FALSE) # No
#pnorm(q = 0.05, mean = 0, sd = sqrt(2))
#abline(v = pnorm(q = 0.05, mean = 0, sd = sqrt(2)), col = "orange")






