##################################################
### Evaluation Metrics for Recommender Systems ###
##################################################
#write.csv(cbr_recommendations, file = "customer_base_recommendations_20m_ml.csv")
#customer_base_recommendations_20m_ml <- read_csv("GitHub/RecommenderSystems/Neighborhood-Based Evaluation/customer_base_recommendations_20m_ml.csv")


# If we want to give TopN Recommendations for a single user
head(cbr_recommendations)
N = 10
cbr_topN_recommendation = cbr_recommendations %>% group_by(userId) %>% arrange(desc(rating)) %>% top_n(n = N) #%>% filter(userId == 386)

# Some Evaluation Measures - Recommender Systems Textbook (pg. 231)

### Coverage Metric
## Two varieties
## User-Space Coverage - fraction of all users such that at least k recommendations per user can be predicted
## Item-Space Coverage - fraction of all items such that at least k recommendations (to users) per item can be predicted
# Let's say k=1 is sufficient for our concerns for each Coverage Metric
## How many movies got recommended? 
length(unique(cbr_topN_recommendation$movieId)) # 6252
length(unique(cbr_recommendations$movieId)) # 6252
## How many movies are there in the dataset? 
length(unique(D$movieId)) # 9724
# So the Item-Space Coverage is 6252/9724 => 64.3% coverage
# For the User-Space Coverage, 609/610 => 99.8% coverage
# It is worth noting that either of these measures can be artificially inflated by sacrificing performance measures (like accuracy)
# to attain higher coverage by providing seemingly less suitable recommendations (this might also then be in conflict with other measures like Diversity)
