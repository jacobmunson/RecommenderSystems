setwd("~/Documents")



if(dataset == "ML1M"){
  ratings <- read.table("Recommender Systems - Home Folder/ml-1m/ratings.dat")
}
if(dataset == "ML10M"){
  ratings <- read.table("Recommender Systems - Home Folder/ml-10M/ratings.dat")
}

#ratings <- read.table("~/Documents/Recommender Systems - Mystery Machine/ml-10M/ratings.dat")

library(tidyverse)
# Format user::item::rating::timestamp
D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
colnames(D) = c("user","item","rating","timestamp")
D = as_tibble(D)
head(D) # visual check
dim(D) # how many? 


# ratings <- read.table("Recommender Systems - Home Folder/ml-1m/ratings.dat")
# ratings <- read.table("Recommender Systems - Home Folder/ml-10M/ratings.dat")
# ratings <- read.table("Documents/Recommender Systems - Home Folder/ml-1m/ratings.dat")

# Format user::item::rating::timestamp
#D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
#D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
head(D) # visual check
