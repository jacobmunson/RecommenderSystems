###########################
### User Level Analysis ###
# Data preparation
###########################

# reshaping benchmarks - skip - just putting here for records
library(reshape2)
library(microbenchmark)
microbenchmark(
  M = D %>% filter(user %in% (D_item %>% .$user)) %>% dcast(user~item, value.var = "rating")#, item == item_pred)
)

microbenchmark(
  M = D %>% filter(user %in% (D_item %>% .$user)) %>% dcast(user~item, value.var = "rating")#, item == item_pred)
)

microbenchmark(
  M = D %>% filter(user %in% (D_item %>% .$user)) %>% select(user, item, rating) %>% spread(item, rating)#, item == item_pred)
)

# start here

library(readr)
D = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
head(D)

D_movies = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/movies.csv")
D_tags = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/tags.csv")
D_links = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/links.csv")


D %>% filter(user == 2) %>% arrange(timestamp) %>% left_join(y = D_movies,by = c("item" = "movieId"))

D %>% group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user) %>% select(user, genres) %>% unique() %>% print(n = 20)

D %>% group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user)


D_movies %>% select(genres) %>% unique() %>% nrow()
l = D_movies %>% select(genres) %>% unique()
l = c(l)
unique(unlist(lapply(1:length(l), FUN = function(i){strsplit(l[[i]], split = "[|]")})))
