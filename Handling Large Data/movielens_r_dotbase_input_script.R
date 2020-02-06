
#########################
#### Mystery Machine ####
#########################
D_train <- read.table("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u1.base")
colnames(D_train) = c("user","item","rating","timestamp")

D_test <- read.table("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u1.test")
colnames(D_test) = c("user","item","rating","timestamp")
########################




########################
#### Corner Machine ####
########################
D_train <- read.table("~/Recommender Systems - Corner/ml-100k/u1.base")
colnames(D_train) = c("user","item","rating","timestamp")

D_test <- read.table("~/Recommender Systems - Corner/ml-100k/u1.test")
colnames(D_test) = c("user","item","rating","timestamp")
#####################



#####################
#### Red Machine ####
#####################

## 100k ML Dataset
D = read_csv("~/Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")

## 100k ML Benchmark
D_train <- read.table("~/Recommender Systems - Home Folder/ml-100k/u1.base")

######################
### Corner Machine ###
######################
D_train <- read.table("~/Recommender Systems - Corner/ml-100k/u1.base")
colnames(D_train) = c("user","item","rating","timestamp")

D_test <- read.table("~/Recommender Systems - Home Folder/ml-100k/u1.test")
colnames(D_test) = c("user","item","rating","timestamp")
