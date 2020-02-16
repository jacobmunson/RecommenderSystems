###########################

## Relevant Packages
library(dplyr)
library(readr)
library(reshape2)
library(doParallel)

source('~/GitHub/RecommenderSystems/recommender_systems_helper_functions.R')

# ratings <- read.table("Recommender Systems - Home Folder/ml-1m/ratings.dat")
# ratings <- read.table("Recommender Systems - Home Folder/ml-10M/ratings.dat")
# ratings <- read.table("Documents/Recommender Systems - Home Folder/ml-1m/ratings.dat")

# Format user::item::rating::timestamp
D = gsub(x = ratings$V1, pattern = "::", replacement = " ") # general substitution
D = matrix(data = as.numeric(unlist(strsplit(D, "\\s+"))), ncol = 4, byrow = TRUE) # filling in matrix
head(D) # visual check
#dim(D) # how many? 
#head(D)
D = as_tibble(D)


D <- read_csv("~/Documents/ml-latest-small/ml-latest-small/ratings.csv")
colnames(D) = c("user","item","rating","timestamp")


set.seed(1)

train_index = sample(x = 1:nrow(D), size = 0.8*nrow(D), replace = F)
D_train = D[train_index,]
D_test = D[-train_index,]

lira_same_cluster_pdf = lira_same_cluster_distribution(V = sort(unique(D_train$rating))) 
lira_pure_chance_pdf = lira_pure_chance_distribution(V = sort(unique(D_train$rating))) 
lira_binary_same_cluster_pdf = lira_binary_same_cluster_distribution()
lira_binary_pure_chance_pdf = lira_binary_pure_chance_distribution()

nrow(D_test)
nrow(D_train)

## Splitting the dataset into smaller peices for parallel processing
chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}
dim(D_test)
num_shards = detectCores()*50
shards = chunk(vector = seq(1:nrow(D_test)), num_splits = num_shards)
names(shards) = seq(1,num_shards,1)

## Estimating variance for  LiRa LRT Similarity
sd_pop = lira_lrt_sd_sampling(dataset = D_train, iter = 1500)
sd_pc = 2

## Setting k for kNN 
K = c(3,5,7,10,15,20,30,40,50,60,80,160)

## Start Evaluation


# rm(sim_matrix);gc() # 
start = Sys.time()

# num_cores = detectCores();
# cl <- makePSOCKcluster(num_cores)
# registerDoParallel(cl)
cat(format(Sys.time(), "%a %b %d %X %Y"), "\n")

sim_matrix = foreach(i = 1:num_shards, .combine = rbind, .packages = c("dplyr","reshape2")) %do% {
  print(i)
  k_error_df_total = c()
  
  D_subset =  D_test[shards[[i]],]
  #stored_similarity = c()
  
  
  for(j in 1:nrow(D_subset)){
    #print(j) # j = 11
    D_test_i = D_subset[j,]
    
    potential_coraters = D_train %>% filter(D_train$item == D_test_i$item) %>% select(user)
    if(nrow(potential_coraters) > 0){
      A = D_train %>% filter(D_train$user %in% c(D_test_i$user,potential_coraters$user))
      B = dcast(data = A, formula = user~item, value.var = "rating")
      B = B[order(B$user),]
      rownames(B) = B$user
      B = as.matrix(B[,-1]) # unaltered from here on out
      

      
      ## done
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gaussian_quant0.1", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        

        
        
        # stored_similarity = bind_rows(stored_similarity,
        #                               data.frame(user1 = as.character(D_test_i$user), 
        #                                          user2 = names(similarity_vector), 
        #                                          sim = similarity_vector, 
        #                                          stringsAsFactors = F))
        # 
        # stored_similarity %>% head()
        # about_to_compute = data.frame(user1 = as.character(D_test_i$user), user2 = as.character(rownames(B)), stringsAsFactors = F)
        # 
        # a = about_to_compute
        # b = about_to_compute %>% mutate(user2t = user2, user1t = user1, user1 = user2t, user2 = user1t) %>% select(user1, user2)
        # str(a)
        # str(b)
        # c = bind_rows(a,b)
        # 
        # str(stored_similarity)
        # 
        # anti_join(c, stored_similarity)
        # 
        # inner_join(d, stored_similarity, by = c("user1" = "user1", "user1" = "user2"))
        # 
        # stored_similarity
        # length(rownames(B))
        # length(similarity_vector)

        
        similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                              test_observation = D_test_i,
                                              similarity_measure = "lira_gaussian_pure_chance")
        
        similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
 
        train_set = D_train 
        test_set = D_test_i

        neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
        neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
        
        neighbor_ratings = neighbor_ratings %>% arrange(desc(y))

        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          
          k_current = min(length(similarity_vector), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors

          prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()

          sim = prediction_neighbors$y
          sim = sim[sim > quantile(sim, probs = 0.1)]
          #sim = sim[sim > quantile(sim, probs = 0.25)]
          
          #sim = sim[sim > 1*mean(sim) - 1.5*sd(sim)]
          #sim =  sim[sim > 0*mean(sim) + 0.5*sd(sim)]
          
          prediction_neighbors = neighbor_ratings[1:length(sim),]# %>% na.omit()
          
          k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
          
          pred_rating_nn = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
          k_error_df_lirau[k,"ae_nn"] = abs(pred_rating_nn - test_set$rating)
          
          pred_rating_knn = mean(prediction_neighbors$rating)
          k_error_df_lirau[k,"ae_knn"] = abs(pred_rating_knn - test_set$rating)
          
          # mae_nn #pred_lirau[1]
          # mae_knn #pred_lirau[2]
        }
      }
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      
      k_error_df_lirau = data.frame(K, k_current = NA, sim = "lira_gaussian", ae_nn = NA, ae_knn = NA)
      if(nrow(B) > 1){
        
        similarity_vector = compute_neighbor_similarity(user_item_matrix = B,
                                                        test_observation = D_test_i,
                                                        similarity_measure = "lira_gaussian_pure_chance")
        
        similarity_vector = nearest_neighbors_trimming_function(similarity_vector_with_self_similarity = similarity_vector)
        
        train_set = D_train 
        test_set = D_test_i
        
        neighbor_ratings = train_set[which(train_set$item == test_set$item & train_set$user %in% names(similarity_vector)),]
        neighbor_ratings = merge(neighbor_ratings[c("user","rating")], similarity_vector, by.x = "user", by.y = "row.names")
        
        neighbor_ratings = neighbor_ratings %>% arrange(desc(y))
        
        # columns: k, k_current, ae_nn, ae_knn
        # dimensions: K * 4
        for(k in 1:length(K)){
          
          k_current = min(length(similarity_vector), K[k])
          k_current = max(k_current, min(K)) # to enforce minimum number of neighbors
          
          prediction_neighbors = neighbor_ratings[1:k_current,] %>% na.omit()
          
          # sim = prediction_neighbors$y
          # sim = sim[sim > quantile(sim, probs = 0.1)]
          # 
          # prediction_neighbors = neighbor_ratings[1:length(sim),]
          
          k_error_df_lirau[k,"k_current"] = nrow(prediction_neighbors)
          
          pred_rating_nn = sum(prediction_neighbors$rating * prediction_neighbors$y)/sum(abs(prediction_neighbors$y)) # is this supposed to be /|abs(sim)|
          k_error_df_lirau[k,"ae_nn"] = abs(pred_rating_nn - test_set$rating)
          
          pred_rating_knn = mean(prediction_neighbors$rating)
          k_error_df_lirau[k,"ae_knn"] = abs(pred_rating_knn - test_set$rating)
          
          # mae_nn #pred_lirau[1]
          # mae_knn #pred_lirau[2]
        }
      }
      k_error_df_total = bind_rows(k_error_df_total, k_error_df_lirau)
      
      

    }
    
  }
  
  k_error_df_total
  
}

#stopCluster(cl);# dim(M)
end = Sys.time()
end - start # 1.02  

discordr::send_message(message = paste0("Mystery Machine done, took ", 
                                        round(end - start,2)," ",
                                        units(end - start), " "))

head(sim_matrix, n = 15) # visual inspection


sim_matrix %>% 
  group_by(sim, K) %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + geom_line(linetype="dashed", size=1)

D_sim = sim_matrix


D_sim %>% 
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>% #ungroup() %>% 
  ggplot(aes(x = K, y = MAE_nn, group = `sim`, color = `sim`)) + 
  geom_point(color="black", size=2) + 
  geom_line(linetype="dashed", size=1) + 
  ggtitle("MovieLens - 100k - latest")

send_current_ggplot()

#send_console(
D_sim %>% 
  group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
  summarize(MAE_nn = mean(ae_knn, na.rm = T)) %>%
  group_by(sim) %>% 
  summarize(min_mae = min(MAE_nn)) %>%
  inner_join(D_sim %>% 
               group_by(sim, K) %>% #filter(sim != "lira_bin", sim != "lira_mn") %>%
               summarize(MAE_nn = mean(ae_knn, na.rm = T)), by = c("sim" = "sim", "min_mae" = "MAE_nn"))




D_total_test
D_total_test %>% 
  filter(sim %in% c("lira_gauss",
                    "cosine",
                    "lira_gauss_mean0_sd1", 
                    "lira_gauss_mean0_sd2",
                    "lira_gauss_mean0_sd2.5",
                    "lirau")) %>%
  group_by(K, sim) %>% 
  summarize(MAE = mean(ae_knn, na.rm = T), sim_sd = sd(ae_knn, na.rm = T)) %>% 
  ggplot(aes(x = K, y = sim_sd, group = `sim`, color = `sim`)) + geom_line() + geom_point() + ggtitle(label = "SD against K value")
send_current_ggplot()



sim_matrix %>%
  group_by(K, sim) %>% 
  summarize(mean_k = mean(k_current), sd_k = sd(k_current)) %>%
  ggplot(aes(x = K, y = mean_k, col = `sim`)) + 
  geom_line() + geom_point() + ggtitle(label = "Average Actual Used K against Prescribed K")

###################################
