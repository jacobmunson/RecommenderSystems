### User Rating Variability ###
# 


# Examining how many new users rate an item through time
# A proxy for "is this item more/less popular through time?"
# Could give an indication of stale content, recommendable items, etc 
library(tidyverse)

# Larger data sets (or else this isn't super interesting)
##ratings <- read.table("~/Recommender Systems - Home Folder/ml-1m/ratings.dat")
ratings <- read.table("~/Recommender Systems - Home Folder/ml-10M/ratings.dat")

# Format user::item::rating::timestamp
D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
head(D) # visual check
dim(D) # how many? 

colnames(D) = c("user","item","rating","timestamp")
head(D)

item_back = 5

D_user_ratings = D %>% group_by(user) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>%
  summarize(mu_rating = mean(rating, na.rm = T), var_rating = var(rating, na.rm = T))

D_user_ratings %>% 
  ggplot(aes(x = mu_rating, y = var_rating)) + 
  geom_point() + 
  ggtitle(label = paste("10M dataset - last ", item_back, " ratings"))

D_user_ratings %>% filter(mu_rating < 1.5, var_rating < 2)

D %>% filter(user == 160) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back))


D_user_ratings %>% filter(mu_rating > 4, var_rating < 2)

D %>% filter(user == 5) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) #%>%


D_user_ratings %>% filter(mu_rating < 1.5, var_rating < 2)

D %>% filter(user == 160) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) #%>%










