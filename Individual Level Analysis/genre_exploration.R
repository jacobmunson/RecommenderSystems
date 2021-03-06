###########################
### User Level Analysis ###
# Genre Breakdowns ########
###########################
 
head(D)

D_movies = read_csv("~/Recommender Systems - Home Folder/ml-latest-small-100k/movies.csv")
D_tags = read_csv("~/Recommender Systems - Home Folder/ml-latest-small-100k/tags.csv")
D_links = read_csv("~/Recommender Systems - Home Folder/ml-latest-small-100k/links.csv")


#############################


# verify below when done with 10M



# For Genre Exploration
D %>% group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user) %>% select(user, genres) %>% print(n = 20)

# maybe edit this to decompose composite genres
D %>% group_by(user) %>% #filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user) %>% select(user, genres) %>% 
  unique() %>% 
  group_by(user) %>% 
  summarize(unique_genres = n()) %>% 
  left_join(user_age, by = "user") %>% 
  select(genre_count = unique_genres, age = lifetime_days, events) %>%
  ggplot(aes(x = genre_count, y = age, color = events)) + 
  geom_point(size = 2) + scale_fill_gradient(low = ("red"), high = muted("blue"), aesthetics = "color", space = "Lab")


D %>% group_by(user) %>% filter(user == 2) %>%
  arrange(timestamp) %>% 
  ungroup() %>% 
  left_join(y = D_movies,by = c("item" = "movieId")) %>% 
  left_join(y = D_tags,by = c("item" = "movieId", "user" = "userId", "timestamp" = "timestamp")) %>% 
  arrange(user) %>% select(user, genres)


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

###############
### Ratings ###
###############

# Mean Rating density 
D %>% select(user, rating) %>% 
  group_by(user) %>% 
  summarize(mu = mean(rating), sd = sd(rating)) %>% 
  ggplot(aes(x = mu)) + geom_density()
# SD Rating Density
D %>% select(user, rating) %>% 
  group_by(user) %>% 
  summarize(mu = mean(rating), sd = sd(rating)) %>% 
  ggplot(aes(x = sd)) + geom_density()
# Mean and SD
D %>% select(user, rating) %>% 
  group_by(user) %>% 
  summarize(mu = mean(rating), sd = sd(rating)) %>% 
  ggplot(aes(x = mu, y = sd)) + geom_point() #density()

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

D %>% select(user, rating) %>% 
  group_by(user) %>% 
  mutate(next_rating = lead(rating), rating_diff = rating - next_rating) %>% 
  group_by(user) %>% 
  summarize(mu_diff = mean(rating_diff, na.rm = T), sd_diff = sd(rating_diff, na.rm = T)) %>%
  ggplot(aes(x = mu_diff, y = sd_diff)) + geom_point() #density() # and sd_diff


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
# Break up genres
unique(unlist(lapply(1:length(l), FUN = function(i){strsplit(l[[i]], split = "[|]")})))
length(unique(unlist(lapply(1:length(l), FUN = function(i){strsplit(l[[i]], split = "[|]")})))) # 20 unique when split up

