
#########################
#### Mystery Machine ####
#########################
set = 5
D_train <- read.table(file = paste0("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u",set,".base"))
colnames(D_train) = c("user","item","rating","timestamp")

D_test <- read.table(file = paste0("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u",set,".test"))
colnames(D_test) = c("user","item","rating","timestamp")



D_train <- read.table("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u1.base")
colnames(D_train) = c("user","item","rating","timestamp")

D_test <- read.table("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u1.test")
colnames(D_test) = c("user","item","rating","timestamp")



#path = paste0("~/Documents/Recommender Systems - Mystery Machine/ml-100k/u",set,".base")


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
D_train <- read.table("~/Recommender Systems - Home Folder/ml-100k/u5.base")
colnames(D_train) = c("user","item","rating","timestamp")

D_test <- read.table("~/Recommender Systems - Home Folder/ml-100k/u5.test")
colnames(D_test) = c("user","item","rating","timestamp")
