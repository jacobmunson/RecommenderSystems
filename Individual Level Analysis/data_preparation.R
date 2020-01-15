###########################
### User Level Analysis ###
# Data preparation
###########################
library(readr)
D = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
head(D)

D_movies = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/movies.csv")
D_tags = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/tags.csv")
D_links = read_csv("Recommender Systems - Home Folder/ml-latest-small-100k/links.csv")


#D %>% filter(user == 2) %>% arrange(timestamp) %>% left_join(y = D_movies,by = c("item" = "movieId"))

# For IET Exploration
library(lubridate)
library(scales)

# User level IET
D %>% 
  group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  arrange(user) %>% 
  select(user, timestamp) %>% # data readme says "Timestamps represent seconds since midnight Coordinated Universal Time (UTC) of January 1, 1970."
  group_by(user) %>%
  mutate(timestamp_lag = lag(timestamp), timestamp_diff = timestamp - timestamp_lag) %>% 
  select(user, timestamp_diff) %>% na.omit() %>% 
  ggplot(aes(x = timestamp_diff, group = user, color = user)) + 
  geom_density() + 
  scale_x_continuous(limits = c(0, 1000), oob = squish) + ggtitle(label = "IETs")
# graphically nothing standing out here - most people 

# Average IET per User
D %>%
  group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  arrange(user) %>% 
  select(user, timestamp) %>% # data readme says "Timestamps represent seconds since midnight Coordinated Universal Time (UTC) of January 1, 1970."
  group_by(user) %>%
  mutate(timestamp_lag = lag(timestamp), timestamp_diff = timestamp - timestamp_lag) %>% 
  select(user, timestamp_diff) %>% na.omit() %>% group_by(user) %>% summarize(IET_mu = mean(timestamp_diff)) %>% 
  ggplot(aes(x = user, y = IET_mu)) + 
  geom_point()

# Density of Average IET per User
D %>% #arrange(user) %>% 
  group_by(user) %>% 
  arrange(timestamp) %>% 
  ungroup() %>% 
  arrange(user) %>% 
  select(user, timestamp) %>% # data readme says "Timestamps represent seconds since midnight Coordinated Universal Time (UTC) of January 1, 1970."
  group_by(user) %>%
  mutate(timestamp_lag = lag(timestamp), timestamp_diff = timestamp - timestamp_lag) %>% 
  select(user, timestamp_diff) %>% na.omit() %>% group_by(user) %>% summarize(IET_mu = mean(timestamp_diff)) %>% 
  ggplot(aes(x = IET_mu)) + 
  geom_density() + 
  scale_x_continuous(limits = c(0, 100000), oob = squish) + ggtitle(label = "IETs", subtitle = "Aggregate per User")





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




#################

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
