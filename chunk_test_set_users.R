## Splitting the dataset into smaller pieces for parallel processing
set.seed(1)
chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}

# num_shards = detectCores()*10
# shards = chunk(vector = seq(1:nrow(D_test)), num_splits = num_shards)
# names(shards) = seq(1,num_shards,1)


num_shards = num_cores*shard_multiplier
shards = chunk(vector = unique(D_train$user), num_splits = num_shards)
names(shards) = seq(1,num_shards,1)