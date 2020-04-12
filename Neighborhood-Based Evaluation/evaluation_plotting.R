library(readr)

D1 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set1_lira_gauss_pc_sd2.csv")
D2 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set2_lira_gauss_pc_sd2.csv")
D3 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set3_lira_gauss_pc_sd2.csv")
D4 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set4_lira_gauss_pc_sd2.csv")
D5 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set5_lira_gauss_pc_sd2.csv")



D1 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set1.csv")
D1_test <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set1_test.csv")
D1_test <- read_csv("GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set1_multinomial.csv")
D1 = sim_matrix

D2 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set2.csv")
D2_test <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set2_test.csv")
D2 = sim_matrix

D3 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set3.csv")
D3_test <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set3_test.csv")
D3 = sim_matrix

D4 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set4.csv")
D4_test <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set4_test.csv")
D4 = sim_matrix


D5 <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set5.csv")
D5_test <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/ml_100k_benchmark_set5_test.csv")
D5 = sim_matrix


library(dplyr)
D = bind_rows(D1,D2,D3,D4,D5)
D = bind_rows(D1_test,D2_test,D3_test,D4_test,D5_test)


# D = bind_rows(D1,D1_test)
# D = bind_rows(D2,D2_test)
# D = bind_rows(D3,D3_test)
# D = bind_rows(D4,D4_test)
# D = bind_rows(D5,D5_test)

D %>% filter(Method == 'kNN' & `Similarity Measure` == "LiRa - Uniform") %>% group_by(k) %>% summarize(first(MAE) - last(MAE))

D = D %>% group_by(`Similarity Measure`, Method, k) %>% summarize(MAE_mu = mean(MAE))
library(tidyverse)
head(D)
unique(D$Method)
unique(D$`Similarity Measure`)
x_breaks = unique(D$k)

D %>% select(sim = `Similarity Measure`, K = k, MAE_mu) %>% filter(Method == "kNN") %>% 
  group_by(sim, K) %>% filter(sim != "lira_bin", sim!= "LiRa - LogGaussian") %>%
  #summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_mu, group = sim, color = sim)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1)  + 
  ggtitle("kNN Average") + theme_grey() + 
  scale_y_continuous(limits = c(0.75, 0.90), breaks = seq(0.75, 1, by = 0.05)) + scale_x_continuous(breaks = x_breaks)


library(ggplot2)
D %>% filter(Method == 'kNN') %>% 
  ggplot(aes(x = k, y = MAE_mu, group = `Similarity Measure`, color = `Similarity Measure`, shape = `Similarity Measure`), size = 5)  +
    geom_line(linetype="dashed", size=1) +
    geom_point(color="black", size=2) + 
  ggtitle("kNN Average") + theme_grey() + 
  scale_y_continuous(limits = c(0.75, 0.90), breaks = seq(0.75, 1, by = 0.05)) + scale_x_continuous(breaks = x_breaks)

head(D)
head(knn_variant_mu_threshold_lira)
knn_variant_mu_threshold_lira <- read_csv("~/GitHub/RecommenderSystems/MovieLens Evaluation/knn_variant_mu_threshold_lira.csv")

D %>% select(`Similarity Measure`, k, MAE_mu) %>% filter(`Similarity Measure` == "LiRa - Uniform") %>% print(n=24)


bind_rows(D %>% filter(Method == "kNN") %>% select(`Similarity Measure`, k, MAE_mu) ,
          knn_variant_mu_threshold_lira %>% 
            select(`Similarity Measure` = sim, k = K, MAE_mu = MAE) %>% 
            filter(`Similarity Measure` == "lirau_test")) %>% 
  filter(`Similarity Measure` %in% c("Cosine Similarity", "LiRa - Uniform", "lirau_test", "LiRa - Gaussian1")) %>% 
  group_by(`Similarity Measure`) %>% summarize(min(MAE_mu))


bind_rows(D %>% filter(Method == "kNN") %>% select(`Similarity Measure`, k, MAE_mu) ,
          knn_variant_mu_threshold_lira %>% 
            select(`Similarity Measure` = sim, k = K, MAE_mu = MAE) %>% 
            filter(`Similarity Measure` == "lirau_test")) %>% 
  filter(`Similarity Measure` %in% c("Cosine Similarity", "LiRa - Uniform", "lirau_test", "LiRa - Gaussian1")) %>% 
  ggplot(aes(x = k, y = MAE_mu, group = `Similarity Measure`, color = `Similarity Measure`, shape = `Similarity Measure`), size = 5)  +
  geom_line(linetype="dashed", size=1) +
  geom_point(color="black", size=2) + 
  ggtitle("kNN Average", subtitle = "require > mu for nn") + theme_grey() + 
  scale_y_continuous(limits = c(0.75, 0.90), breaks = seq(0.75, 1, by = 0.05)) + scale_x_continuous(breaks = x_breaks)


#+ ylim(0.802,0.83)
D %>% filter(Method == 'NN-RST') %>% ggplot(aes(x = k, y = MAE_mu, group = `Similarity Measure`, color = `Similarity Measure`))  +
    geom_line(linetype="dashed", size=1.0) +
    geom_point(color="black", size=1.2) + ggtitle("Neighbor Approach") + theme_grey()

library(cowplot)
plot_grid(A, B)
