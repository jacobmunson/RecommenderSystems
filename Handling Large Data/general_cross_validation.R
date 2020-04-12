# D = read_csv("~/Recommender Systems - Home Folder/ml-latest-small-100k/ratings.csv")
# colnames(D) = c("user","item","rating","timestamp")
set.seed(1)


chunk = function(vector, num_splits){return(split(vector, factor(sort(rank(vector) %% num_splits))))}

num_cv = 5
cvs = chunk(vector = sample(seq(1:nrow(dataset))), num_splits = num_cv)
cv_df = t(combn(num_cv, num_cv - 1))


train_index1 = data.frame(index = unlist(cvs[cv_df[1,]]))
train_index2 = data.frame(index = unlist(cvs[cv_df[2,]]))
train_index3 = data.frame(index = unlist(cvs[cv_df[3,]]))
train_index4 = data.frame(index = unlist(cvs[cv_df[4,]]))
train_index5 = data.frame(index = unlist(cvs[cv_df[5,]]))

D1_train = dataset[train_index1$index,]
D1_test = dataset[-train_index1$index,]

D2_train = dataset[train_index2$index,]
D2_test = dataset[-train_index2$index,]

D3_train = dataset[train_index3$index,]
D3_test = dataset[-train_index3$index,]

D4_train = dataset[train_index4$index,]
D4_test = dataset[-train_index4$index,]

D5_train = dataset[train_index5$index,]
D5_test = dataset[-train_index5$index,]

