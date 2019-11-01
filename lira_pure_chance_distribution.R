###################################################
### PDF for 'Pure Chance' Distribution for LiRa ###

# Ratings (pick one or make a better one)
V = seq(0.5,5,0.5)
V = seq(1,5,1)
V = unique(D$ratings)

# Determine Distribution
for(i in 1:100000){
  diff[i] = abs(sample(x = V, size = 1, replace = T) - sample(x = V, size = 1, replace = T))
}
table(diff)/100000
sum(table(diff)/100000)
plot(table(diff)/100000, xlab = "", ylab = "P(|diff|)", main = "Distribution - Pure Chance")


lira_pure_chance_pdf = matrix(table(diff)/100000)
rownames(lira_pure_chance_pdf) = V - 1
rownames(lira_pure_chance_pdf) = V - 0.5
colnames(lira_pure_chance_pdf) = "prob"
lira_pure_chance_pdf

d = length(V)
