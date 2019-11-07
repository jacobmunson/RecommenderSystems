library(readr)
D1 <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set1.csv")
D2 <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set2.csv")
D3 <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set3.csv")
D4 <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set4.csv")
D5 <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set5.csv")

library(dplyr)
D = bind_rows(D1,D2,D3,D4,D5)

D = D %>% group_by(`Similarity Measure`, Method, k) %>% summarize(MAE_mu = mean(MAE))

head(D)
unique(D$`Similarity Measure`)
unique(D$k)
# 
# D %>% filter(`Similarity Measure` == "Pearson Correlation - PWC" , Method == "kNN") %>% group_by(k) %>% summarize(MAE = mean(MAE)) %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "Pearson Correlation - IZ", Method == "kNN") %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "Cosine Similarity", Method == "kNN") %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "LiRa - Uniform", Method == "kNN") %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "LiRa - Gaussian", Method == "kNN") %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "LiRa - LogGaussian", Method == "kNN") %>% select(k, MAE)
# 
# D %>% filter(`Similarity Measure` == "Pearson Correlation - PWC" , Method == 'NN-RST') %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "Pearson Correlation - IZ", Method == 'NN-RST') %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "Cosine Similarity", Method == 'NN-RST') %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "LiRa - Uniform", Method == 'NN-RST') %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "LiRa - Gaussian", Method == 'NN-RST') %>% select(k, MAE)
# D %>% filter(`Similarity Measure` == "LiRa - LogGaussian", Method == 'NN-RST') %>% select(k, MAE)

library(ggplot2)
D %>% filter(Method == 'kNN') %>% ggplot(aes(x = k, y = MAE_mu, group = `Similarity Measure`, color = `Similarity Measure`))  +
    geom_line(linetype="dashed", size=1.0) +
    geom_point(color="black", size=1.2) + ggtitle("kNN Average") + theme_grey() + ylim(0.802,0.83)
D %>% filter(Method == 'NN-RST') %>% ggplot(aes(x = k, y = MAE_mu, group = `Similarity Measure`, color = `Similarity Measure`))  +
    geom_line(linetype="dashed", size=1.0) +
    geom_point(color="black", size=1.2) + ggtitle("Neighbor Approach") + theme_grey()

library(cowplot)
plot_grid(A, B)
