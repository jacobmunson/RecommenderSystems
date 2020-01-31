# Item-by-item similarity at the user-level
D_test = D %>% select(user, item,rating) %>% spread(item, rating) %>% select(-user)

str(D_test)
dim(D_test)

D_test[1:5,1:5]

# Making similarities
D_test_cor = cor(D_test, use = "pairwise.complete.obs")
item_names = D %>% select(item) %>% unique() %>% .$item
colnames(D_test_cor) = item_names
rownames(D_test_cor) = item_names

dim(D_test_cor)
str(D_test_cor)

D_test_cor[1:5,1:5]

#D %>% select(user) %>% unique()

Dx = D %>% group_by(user) %>%
  #filter(user == 2) %>%
  arrange(timestamp) %>% 
  select(item) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% slice(1:(n()- 1)) %>%  
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% ungroup()

Dx_diff = Dx %>% group_by(user) %>% #filter(user == 2) %>% 
  #select(user, item_sim) %>% 
  mutate(lead_item_sim = lead(item_sim), sim_diff = item_sim - lead_item_sim) %>% na.omit()

Dx_diff %>% 
  group_by(user) %>% 
  mutate(count = seq(1:n())) %>% 
  ggplot(aes(x = count, y = sim_diff, col = user)) + geom_line()

D %>% group_by(user) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - 5)) %>% 
  select(item) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% slice(1:(n()- 1)) %>%  
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% ungroup() %>% group_by(user) %>% 
  summarize(mu_sim = mean(item_sim, na.rm = T), var_sim = var(item_sim, na.rm = T)) %>%
  ggplot(aes(y = mu_sim, x = var_sim)) + 
  geom_point(alpha = 0.5) + 
  theme_light() + geom_hline(yintercept = 0, col = 'red') + geom_vline(xintercept = 1, col = 'red') + 
  ggtitle(label = "10M dataset - last 5 ratings")



item_back = 5

D_user_item_item = D %>% group_by(user) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  select(item) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% slice(1:(n()- 1)) %>%  
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% ungroup() %>% group_by(user) %>% 
  summarize(mu_sim = mean(item_sim, na.rm = T), var_sim = var(item_sim, na.rm = T))


D_user_item_item %>%
  ggplot(aes(y = mu_sim, x = var_sim)) + 
  geom_point(alpha = 0.15) + 
  theme_light() + geom_hline(yintercept = 0, col = 'red') + geom_vline(xintercept = 1, col = 'red') + 
  ggtitle(label = paste("10M dataset - last ", item_back, " ratings"))

str(D_user_item_item)

# 4 "Quandrants"
## Top Left

# "high" mean, low variance
D_user_item_item %>% filter(mu_sim > 0, var_sim < 1)

# Making it a little higher 
D_user_item_item %>% filter(mu_sim > 0.5, var_sim < 0.5)

D %>% filter(user == 50) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId")) #%>%

D %>% filter(user == 96) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

D_user_item_item %>% filter(mu_sim > 0.5, var_sim < 1, var_sim > 0.5)

D %>% filter(user == 4146) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

D %>% filter(user == 16342) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

## Top Right
D_user_item_item %>% filter(mu_sim > 0, var_sim > 1)

D %>% filter(user == 12952) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

## Bottom Left

D_user_item_item %>% filter(mu_sim < 0, var_sim < 1)

D %>% filter(user == 35) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

D_user_item_item %>% filter(mu_sim < 0.5, var_sim < 1)

D %>% filter(user == 1) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

D_user_item_item %>% filter(mu_sim < 0.5, var_sim < 1, var_sim > 0.5)

D %>% filter(user == 11) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

## Bottom Right
D_user_item_item %>% filter(mu_sim < 0, var_sim > 1)

D %>% filter(user == 4282) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

D %>% filter(user == 23277) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

D_user_item_item %>% filter(mu_sim < 0, var_sim > 1.25)

D %>% filter(user == 71030) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))

D %>% filter(user == 8348) %>% 
  arrange(timestamp) %>% 
  filter(row_number() >= (n() - item_back)) %>% 
  mutate(next_item = as.character(lead(item)), item = as.character(item)) %>% 
  slice(1:(n()- 1)) %>% 
  rowwise() %>% mutate(item_sim = D_test_cor[item,next_item]) %>% 
  mutate(item = as.numeric(item)) %>%
  left_join(y = D_movies,by = c("item" = "movieId"))


