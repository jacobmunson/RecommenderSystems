library(lubridate)

netflix_data = netflix_data %>% mutate(date = as_date(date))
nf_by_date = netflix_data %>% group_by(date) %>% summarize(mu = mean(rating))

mean(netflix_data$rating)

library(ggplot2)
nf_by_date %>% dim()

nf_by_date %>% 
  ggplot(aes(x = date, y = mu)) + 
  geom_point(alpha = 0.35) + theme_bw() +
  ylab("Mean Rating") + xlab("Date") + ggtitle("Netflix Data", subtitle = "Mean rating through time")
