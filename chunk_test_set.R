## Splitting the dataset into smaller peices for parallel processing
chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}
dim(D_test)
num_shards = detectCores()*50
shards = chunk(vector = seq(1:nrow(D_test)), num_splits = num_shards)
names(shards) = seq(1,num_shards,1)