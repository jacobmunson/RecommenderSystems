library(readr)
D <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_80_20_split_positive_neighbors.csv")
D <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_80_20_split_by_definition.csv")

head(D)
unique(D$`Similarity Measure`)
unique(D$k)

D %>% filter(`Similarity Measure` == "Pearson Correlation - PWC" , Method == "kNN") %>% group_by(k) %>% summarize(MAE = mean(MAE)) %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "Pearson Correlation - IZ", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "Cosine Similarity", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - Uniform", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - Gaussian", Method == "kNN") %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - LogGaussian", Method == "kNN") %>% select(k, MAE)

D %>% filter(`Similarity Measure` == "Pearson Correlation - PWC" , Method == 'NN-RST') %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "Pearson Correlation - IZ", Method == 'NN-RST') %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "Cosine Similarity", Method == 'NN-RST') %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - Uniform", Method == 'NN-RST') %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - Gaussian", Method == 'NN-RST') %>% select(k, MAE)
D %>% filter(`Similarity Measure` == "LiRa - LogGaussian", Method == 'NN-RST') %>% select(k, MAE)


A = D %>% filter(Method == 'kNN') %>% ggplot(aes(x = k, y = MAE, group = `Similarity Measure`, color = `Similarity Measure`))  +
    geom_line(linetype="dashed", size=1.0) +
    geom_point(color="black", size=1.2) + ggtitle("kNN Average") + theme_grey() + ylim(0.74,0.78)
B = D %>% filter(Method == 'NN-RST') %>% ggplot(aes(x = k, y = MAE, group = `Similarity Measure`, color = `Similarity Measure`))  +
    geom_line(linetype="dashed", size=1.0) +
    geom_point(color="black", size=1.2) + ggtitle("Neighbor Approach") + theme_grey()

library(cowplot)
plot_grid(A, B)
