library(readr)
D <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_80_20_split.csv")
head(D)

unique(D$k)
D %>% filter(`Similarity Measure` == "Pearson Correlation", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "Cosine Similarity", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - Uniform", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - Gaussian", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - LogGaussian", Method == "kNN") %>% select(k, MAE)


A = D %>% filter(Method == 'kNN') %>% ggplot(aes(x = k, y = MAE, group = `Similarity Measure`, color = `Similarity Measure`))  +
    geom_line(linetype="dashed", size=1.2) +
    geom_point(color="red", size=1.2) + ggtitle("kNN Average")
B = D %>% filter(Method == 'NN-RST') %>% ggplot(aes(x = k, y = MAE, group = `Similarity Measure`, color = `Similarity Measure`))  +
    geom_line(linetype="dashed", size=1.2) +
    geom_point(color="red", size=1.2) + ggtitle("Neighbor Approach")

library(cowplot)
plot_grid(A, B)
