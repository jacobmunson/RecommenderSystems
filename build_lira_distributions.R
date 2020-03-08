lira_same_cluster_pdf = lira_same_cluster_distribution(V = sort(unique(D_train$rating))) 
lira_pure_chance_pdf = lira_pure_chance_distribution(V = sort(unique(D_train$rating))) 
lira_binary_same_cluster_pdf = lira_binary_same_cluster_distribution()
lira_binary_pure_chance_pdf = lira_binary_pure_chance_distribution()