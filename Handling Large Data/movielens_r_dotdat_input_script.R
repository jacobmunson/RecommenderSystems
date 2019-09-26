ratings <- read.table("Recommender Systems - Home Folder/ml-1m/ratings.dat")
ratings <- read.table("Recommender Systems - Home Folder/ml-10M/ratings.dat")
# Format user::item::rating::timestamp
D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
head(D) # visual check
dim(D) # how many? 
