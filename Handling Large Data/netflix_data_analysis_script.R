# Identifying the effects in the Koren (2009) paper

library(lubridate)
library(ggplot2)
library(tidyverse)

netflix_data <- read_csv("NetflixPrizeData/netflix_data.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))

chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}
#netflix_data = netflix_data %>% mutate(date = as_date(date))


netflix_data %>% group_by(item) %>% tally() %>% summary()


netflix_data %>% 
  select(user, date) %>% 
  group_by(date) %>% 
  summarize(num_user = length(unique(user))) %>% ggplot(aes(x = date, y = num_user)) + geom_point()

netflix_data %>% 
  select(item, date) %>% 
  group_by(date) %>% 
  summarize(num_item = length(unique(item))) %>% ggplot(aes(x = date, y = num_item)) + geom_point()

netflix_data %>% 
  select(date) %>% 
  group_by(date) %>% tally() %>% 
  ggplot(aes(x = date, y = n)) + geom_point()


nf_by_date = netflix_data %>% group_by(date) %>% summarize(mu = mean(rating))




######################################
### Effect 1 #########################
# Showing mean rating through time ###
######################################

nf_by_date %>% 
  ggplot(aes(x = date, y = mu)) + 
  geom_point(alpha = 0.35) + theme_bw() +
  ylab("Mean Rating") + 
  xlab("Date") + 
  ggtitle(subtitle = "Netflix Data", "Mean Rating through Time") + 
  #geom_smooth() + 
  ylim(3,3.9) + 
  theme(text = element_text(size=20))
# looks about right

####################
### End Effect 1 ###
####################


########################################################
### Effect 2 ###########################################
# Showing that movies get higher ratings as they age ###
########################################################

# need mean rating per date, per item
library(lubridate)
item_df = data.frame()

index = sample(1:nrow(netflix_data), size = 100000) # dealing with a subset of the data 
nf_subset = netflix_data[index,] # repeat this process several time

num_items = length(unique(nf_subset$item)) 
#num_items = length(unique(netflix_data$item))

start = Sys.time() # compute rolling mean rating by age of item - "age" = time since first rating
for(i in 1:num_items){
  #item_i = unique(netflix_data$item)[i]
  item_i = unique(nf_subset$item)[i]
  
  item_i_data = nf_subset %>% # netflix_data %>% # 
    filter(item == item_i) %>% 
    arrange(date) %>%
    mutate(first_rating = min(date), days_old = date - first_rating) %>% 
    group_by(days_old) %>% summarize(rating = mean(rating)) 

  item_i_data = item_i_data %>% mutate(roll_mean = NaN)
  
  # rolling me by age
  for(j in 1:nrow(item_i_data)){
    
    #item_i_mu[j] = mean(item_i_data[1:j,"rating"] %>% .$rating)
    item_i_mu = mean(item_i_data[1:j,"rating"] %>% .$rating)
    item_i_data[j,"roll_mean"] = item_i_mu
  }
  
  item_df = bind_rows(item_df, item_i_data)
  print(i)
}
end = Sys.time()
print(end - start); beepr::beep(3)


item_df %>% group_by(days_old) %>% summarize(mu_rating = mean(roll_mean)) %>% 
  ggplot(aes(x = days_old, y = mu_rating)) + geom_point(alpha = 0.25) + 
  ylim(3.2,3.9) + 
  #geom_smooth() + 
  theme_bw() + 
  ylab("Mean Rating") + xlab("Film Age (days)") + 
  ggtitle("Rating by Film Age", subtitle = "Netflix Data") + 
  theme(text = element_text(size=20))

####################
### End Effect 2 ###
####################


############################################
### Looking at Bins ########################
# Want Item Bias and Item Bias in Bin(t) ###
############################################
nf_dates = sort(unique(netflix_data$date))

date_first = first(nf_dates)
date_last = last(nf_dates)

(date_last - date_first)/30

#nf_dates

print(paste0("Years of data:", length(nf_dates)/365))


date_range = seq.Date(from = date_first, to = date_last, by = "day")

length(date_range) - length(nf_dates) # dates with no observation, apparently

date_chunks = chunk(vector = date_range, num_splits = 30)

# selecting a bin to move forward with example numbers
bin_i = 20

# Mean rating of Netflix rating
mu_nf = mean(netflix_data$rating)

###########################
### End Setting Up Bins ###
###########################


#######################
### Item Bias Terms ###
#######################

## Select an item
item_i = 985

## Static Bias for Item ##

# Mean rating for the item
mu_i = netflix_data %>% 
  filter(item == item_i) %>% 
  summarize(mu_i = mean(rating)) %>% .$mu_i

print(paste0("Mean for item ", item_i, " is ", mu_i)) # mean of item i

# Bias for item i
b_i = mu_i - mu_nf 
print(paste0("Bias for item ", item_i, " is ", b_i)) # bias of item i

## End Static Bias for Item ##

## Time Bias for Item ##

# Mean rating for the item during Bin(t)
mu_bin_it = netflix_data %>% 
  filter(date %in% date_chunks[[bin_i]], item == item_i) %>% 
  summarize(bin_mu_i = mean(rating)) %>% .$bin_mu_i

print(paste0("Mean for item ", item_i, " during Bin ", bin_i, " is ", mu_bin_it)) # mean of item i

# Bias for item i during Bin(t)
b_bin_it = mu_bin_it - mu_nf
print(paste0("Bias for item ", item_i, " during Bin ", bin_i, " is ", b_bin_it)) # bias of item i


## Item Time Bias Term
b_it = b_i + b_bin_it
print(paste0("Total Bias for item ", item_i, " during Bin ", bin_i, " is ", b_it)) # total bias of item i

## End Time Bias for Item ##


###########################
### End Item Bias Terms ###
###########################



#######################
### User Bias Terms ###
#######################


# Select a user
#users = unique(netflix_data$user)
#u = 8
#user_u = users[u]

# to get a user that is definitely applicable with above item
users = netflix_data %>% 
  filter(date %in% date_chunks[[bin_i]], 
         item == item_i) %>% 
  select(user) %>% .$user %>% unique() 
u = 1
user_u = users[u]

## Static Bias for User ##

# Mean rating for the user
mu_u = netflix_data %>% 
  filter(user == user_u) %>% 
  summarize(mu_u = mean(rating)) %>% .$mu_u

print(paste0("Mean for user ", user_u, " is ", mu_u)) # mean of user_u


user_u_count = netflix_data %>% 
  filter(user == user_u) %>% 
  select(item) %>% tally() %>% .$n
print(paste0("User ", user_u, " has rated ", user_u_count, " items.")) # 

user_u_ratings = inner_join((netflix_data %>% 
  filter(user == user_u)), 
  movie_titles, by = c("item" = "item_id"))


# Bias for User u
b_u = mu_u - mu_nf 
print(paste0("Bias for user ", user_u, " is ", b_u)) # bias of user u

## End Static Bias for User ##

## Time Bias for User ##

# Get mean date of rating for user u

t_u = netflix_data %>% 
  filter(user == user_u) %>% arrange(date) %>% 
  summarize(first_date = first(date), 
            last_date = last(date), 
            t_u = first_date + ((last_date - first_date)/2)) %>% .$t_u

print(paste0("Mean date of rating is ", t_u))


#first(date_range) + ((last(date_range) - first(date_range))/2)
#last(date_range) - ((last(date_range) - first(date_range))/2)  
  
# Get date of rating
t = netflix_data %>% 
  filter(date %in% date_chunks[[bin_i]], 
         user == user_u, item == item_i) %>% 
  select(date) %>% .$date 
print(paste0("Date of user rating is ", t))

# Deviation of User u at time t
# dev_u (t) = sign(t - t_u) * |t - t_u|^beta
beta = 0.4 # Set for all
dev_u_t = as.numeric(sign(t - t_u)) * abs(as.numeric(t - t_u))^beta

alpha_u = 0.001 # Set for all, but adjusted in SGD
print(paste0("Deviation for user ", user_u, " during time ", t, " is ", dev_u_t)) # deviation of user u during time t


# Bias of User u at time t
mu_u_t = netflix_data %>% filter(user == user_u, date == t) %>% summarize(mu_u_t = mean(rating)) %>% .$mu_u_t
b_u_t = mu_u_t - mu_nf 


# Bias for User u at time t
b_ut = b_u + alpha_u*dev_u_t + b_u_t
print(paste0("Bias for user ", user_u, " during time ", t, " is ", b_ut)) # bias of user u during time t

## End Time Bias for User ##

####################################
### Baseline Estimate vs. Rating ###
####################################

# actual rating value
r_ui = netflix_data %>% 
  filter(date %in% date_chunks[[bin_i]],
         user == user_u, item == item_i) %>% 
  filter(row_number() == 1) %>% .$rating

# baseline estimate involving mean term, time-dependent item bias, and time-dependent user bias

b_ui = mu_nf + b_i + b_u
b_ui_t = mu_nf + b_it + b_ut

movie_titles <- read_csv("NetflixPrizeData/movie_titles.csv", col_names = FALSE)
colnames(movie_titles) = c("item_id", "year", "title")
movie_name = movie_titles %>% filter(item_id == item_i) %>% .$title

print(paste0("User = ", user_u, " | Item = ", item_i, " | ", movie_name ," | r_ui = ", r_ui))
print(paste0("Actual test value is ", r_ui, " and baseline estimate is ", b_ui))
print(paste0("Actual test value is ", r_ui, " and time-dependent baseline estimate is ", b_ui_t))



