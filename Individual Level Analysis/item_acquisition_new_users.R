### Item "Acquisition" Rates ###
# Examining how many new users rate an item through time
# A proxy for "is this item more/less popular through time?"
# Could give an indication of stale content, recommendable items, etc 
library(tidyverse)

# Larger data sets (or else this isn't super interesting)
ratings <- read.table("~/Recommender Systems - Home Folder/ml-1m/ratings.dat")
ratings <- read.table("~/Recommender Systems - Home Folder/ml-10M/ratings.dat")

# Format user::item::rating::timestamp
D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
head(D) # visual check
dim(D) # how many? 

library(readr)
#D = read_csv("~/Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")
head(D)

D = as_tibble(D)

D %>% group_by(user,item) %>% summarize(num = n()) %>% filter(num > 1) # if this comes back with rows then some users have double-rated items

D_temp = D %>% mutate(date  = as.Date(as.POSIXct(timestamp, origin="1970-01-01"))) %>% 
            group_by(item) %>% 
            select(user, date) %>% 
            group_by(item, user) %>% 
            filter(row_number() >= (n() - 1)) %>% # this should remove all but most recent rating of a user (if duplicates), but I haven't actually found any, so unsure of performance - just a note
            group_by(item, date) %>% 
            summarize(num_users = n()) 

head(D_temp)

i = 4
D_temp %>% 
  filter(item == unique(D_temp$item)[i]) %>%  # 44, 39, 32, 260, 356, 3578, 2762 - some interesting items
  ggplot(aes(x = date, y = num_users)) + 
  geom_point(size = 1) + # geom_line()
  geom_hline(yintercept = 0) + geom_linerange(aes(x = date, ymax = num_users, ymin = 0)) + 
  ggtitle(label = "Item-based Acquisition-style plot", subtitle = paste("Item:",i)) + 
  xlab("Date") + ylab("Number of first time users")

