## Splitting the dataset into smaller peices for parallel processing
set.seed(1)
chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}

num_shards = detectCores()*70
shards = chunk(vector = seq(1:nrow(D_test)), num_splits = num_shards)
names(shards) = seq(1,num_shards,1)