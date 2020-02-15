####################################################
### PDF for 'Pure Chance' Distribution for LiRa ####
### PDF for 'Same Cluster' Distribution for LiRa ###
# Ratings (pick one or make a better one) ##########
####################################################
V = seq(0.5,5,0.5)
V = seq(1,5,1)
V = unique(D$rating)

V = unique(D_test$rating)
V = V[order(V, decreasing = F)]
#unique(D_train$rating)

d = length(V) # number of unique values 

lira_pure_chance_pdf = lira_pure_chance_distribution(V = V)
lira_same_cluster_pdf = lira_same_cluster_distribution(V = V)

lira_binary_pure_chance_pdf = lira_binary_pure_chance_distribution()
lira_binary_same_cluster_pdf = lira_binary_same_cluster_distribution()


#####################################
# Numerically Doing the Same Things #
#####################################

# Determine Distribution
diff = c()
for(i in 1:100000){
  diff[i] = abs(sample(x = V, size = 1, replace = T) - sample(x = V, size = 1, replace = T))
}

# Table Results
table(diff)/100000
stopifnot(sum(table(diff)/100000) == 1)
plot(table(diff)/100000, xlab = "", ylab = "P(|diff|)", main = "Distribution - Pure Chance")

# Formating
lira_pure_chance_pdf = matrix(table(diff)/100000)
colnames(lira_pure_chance_pdf) = "prob"

# Select Appropriately
rownames(lira_pure_chance_pdf) = V - 1
rownames(lira_pure_chance_pdf) = V - 0.5

# Visually examine
lira_pure_chance_pdf

