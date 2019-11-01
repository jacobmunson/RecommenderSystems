D_train <- read.table("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u1.base")
colnames(D_train) = c("user","item","rating","timestamp")

D_test <- read.table("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u1.test")
colnames(D_test) = c("user","item","rating","timestamp")
