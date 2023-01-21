library(tidyverse)


### Hold out the TopN samples per user
N = 3 # number of hold out samples per user - these are their highest ratings
D_test = D %>% group_by(user) %>% slice_max(rating, n = N) %>% ungroup() # %>% select(rating) %>% summary()
D_test %>% select(rating) %>% summary()

D_test = D_test %>% filter(rating > 3) # only keep test ratings if they are above 3
D_train = anti_join(D, D_test)


nrow(D_train) + nrow(D_test) == nrow(D)



### Hold out TopX Percentage of samples per user
X = 0.2 # 20% of highest ratings per user are for hold out
D_test = D %>% group_by(user) %>% slice_max(rating, prop = X) %>% ungroup()
D_test %>% select(rating) %>% summary()

D_test = D_test %>% filter(rating > 3) # only keep test ratings if they are above 3
D_train = anti_join(D, D_test)


nrow(D_train) + nrow(D_test) == nrow(D)
