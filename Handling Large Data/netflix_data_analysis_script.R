# Identifying the effects in the Koren (2009) paper

library(lubridate)

netflix_data <- read_csv("NetflixPrizeData/netflix_data.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))

#netflix_data = netflix_data %>% mutate(date = as_date(date))


netflix_data %>% group_by(item) %>% tally() %>% summary()


nf_by_date = netflix_data %>% group_by(date) %>% summarize(mu = mean(rating))

mean(netflix_data$item)




library(ggplot2)
nf_by_date %>% dim()

nf_by_date %>% 
  ggplot(aes(x = date, y = mu)) + 
  geom_point(alpha = 0.35) + theme_bw() +
  ylab("Mean Rating") + 
  xlab("Date") + 
  ggtitle("Netflix Data", subtitle = "Mean rating through time")
# looks about right