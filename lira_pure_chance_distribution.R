###################################################
### PDF for 'Pure Chance' Distribution for LiRa ###

# Ratings (pick one or make a better one)
V = seq(0.5,5,0.5)
V = seq(1,5,1)
V = unique(D$rating)

V = unique(D_test$rating)
V = V[order(V, decreasing = F)]
#unique(D_train$rating)

d = length(V) # number of unique values 

pdf_on_differences_on_V = function(V){
  V_grid = expand.grid(V, V)
  V_grid$diff = abs(V_grid$Var1 - V_grid$Var2)
  
  if(range(diff(V))[1] != range(diff(V))[2]){
    warning("Uneven spaced ratings")
  }

  stopifnot(sum(table(V_grid$diff)/length(V)^2) == 1)
  pcd = matrix(table(V_grid$diff)/length(V)^2)
  rownames(pcd) = V - rep(diff(V)[1],length(V))
  colnames(pcd) = "prob"
  
  return(pcd)
  
}

pdf_on_differences_on_V(V = seq(0.5,5,0.5))

plot(pdf_on_differences_on_V(V))

#####################################
# Numerically Doing the Same Things #

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

