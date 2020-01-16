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

# User IETs
user_iet = D %>% 
  group_by(user) %>% 
  arrange(timestamp) %>% 
  ungroup() %>% 
  arrange(user) %>% 
  select(user, timestamp) %>% # data readme says "Timestamps represent seconds since midnight Coordinated Universal Time (UTC) of January 1, 1970."
  group_by(user) %>%
  mutate(timestamp_lag = lag(timestamp), timestamp_diff = timestamp - timestamp_lag) %>% 
  select(user, timestamp_diff) %>% na.omit() %>% group_by(user) %>% summarize(IET_mu = mean(timestamp_diff))


# User Age
user_age = D %>% 
  group_by(user) %>% 
  arrange(timestamp) %>% 
  summarize(events = n(), lifetime_sec = last(timestamp) - first(timestamp)) %>% 
  mutate(lifetime_days = lifetime_sec/(60*60*24))

user_age$events[user_age$events > 200] = 200 # to make visual better

# Plotting age against IET
left_join(user_iet, user_age, by = "user") %>% 
  mutate(IET_days = IET_mu/(60*60*24)) %>% 
  ggplot(aes(x = IET_days, y = lifetime_days, color = events)) + 
  geom_point(size = 2) + scale_fill_gradient(low = ("red"), high = muted("blue"), aesthetics = "color", space = "Lab")


# For Genre Exploration
D %>% group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user) %>% select(user, genres) %>% print(n = 20)


# Genre movements
# How do users float around genres?
genres = c(D_movies %>% select(genres) %>% unique() %>% .$genres, "N") #%>% nrow() # 951 unique
genre_matrix = matrix(data = 0, nrow = length(genres), ncol = length(genres), dimnames = list(genres, genres))
str(genre_matrix)
genre_transitions = D %>% group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user) %>% 
  select(user, genres) %>% 
  group_by(user) %>% 
  mutate(next_genre = lead(genres))
genre_transitions$next_genre = genre_transitions$next_genre %>% replace_na("N")



for(i in 1:nrow(genre_transitions)){
  genre_matrix[as.character(genre_transitions[i,"genres"]),as.character(genre_transitions[i,"next_genre"])] = 
    genre_matrix[as.character(genre_transitions[i,"genres"]),as.character(genre_transitions[i,"next_genre"])] + 1
  print(i)
}

genre_matrix = genre_matrix/rowSums(genre_matrix)
rowSums(genre_matrix)

i = 2
rownames(genre_matrix)[i]
A = sort(genre_matrix[i,], decreasing = T)
A[which(A > 0)][1:10]

# Additional idea: break up compound genres and make a transition from all components to all component of the next genre

# Ratings 
D %>% select(user, rating) %>% 
  group_by(user) %>% 
  summarize(mu = mean(rating), sd = sd(rating)) %>% 
  ggplot(aes(x = mu)) + geom_density()

D %>% select(user, rating) %>% 
  group_by(user) %>% 
  summarize(mu = mean(rating), sd = sd(rating)) %>% 
  ggplot(aes(x = sd)) + geom_density()

D %>% select(user, rating) %>% 
  group_by(user) %>% 
  mutate(next_rating = lead(rating), rating_diff = rating - next_rating) %>% 
  group_by(user) %>% 
  summarize(mu_diff = mean(rating_diff, na.rm = T), sd_diff = sd(rating_diff, na.rm = T)) %>%
  ggplot(aes(x = mu_diff)) + geom_density() # and sd_diff

D %>% select(user, rating) %>% 
  group_by(user) %>% 
  mutate(next_rating = lead(rating), rating_diff = rating - next_rating) %>% 
  group_by(user) %>% 
  summarize(mu_diff = mean(rating_diff, na.rm = T), sd_diff = sd(rating_diff, na.rm = T)) %>%
  ggplot(aes(x = mu_diff)) + geom_density() # and sd_diff


# Everything
# User,item,rating,timestamp,title,genres,tag
D %>% group_by(user) %>% 
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user)


D_movies %>% select(genres) %>% unique() %>% nrow() # 951 unique
l = D_movies %>% select(genres) %>% unique()
l = c(l)
unique(unlist(lapply(1:length(l), FUN = function(i){strsplit(l[[i]], split = "[|]")})))
length(unique(unlist(lapply(1:length(l), FUN = function(i){strsplit(l[[i]], split = "[|]")})))) # 20 unique when split up



#################
# reshaping benchmarks - skip - just putting here for records right now
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