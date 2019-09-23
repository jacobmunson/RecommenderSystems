## Evaluation Timings 

This folder is for tracking runtimes of certain evaluations

- Non-parallel results on the 100k ML dataset are used to verify that each iteration produces consistent results

## ML 20M Pairwise Comparison Counts

- "Version 1" is a first pass with two variants: shard-based and user-based
    - The difference being that shard-based splits the dataset into chunks (1:1 with cores used in parallel) and user based treats each user as individually in parallelization
    
- "Version 2" is shard-based
    - Improvements include: only carrying 2 columns of the dataframe around (user, item) for better management of RAM, randomly ordering the users (and sharding on thos reorderings), and reducing users under consideration to only those "greater than" the current user (this reduces unnecessary calculations instead of just removing them in the next step
    - Early evaluations demonstrate much better management of memory and 1-3x better runtime
            - Update: With 15% 20M ML, memory is laggy, probably try more fine shards and gc() at each end
    - "bigmemory" R package tested, keeps dataset on disk, much more efficient RAM-wise, but several times slower in my practice
