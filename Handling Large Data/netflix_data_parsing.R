#getwd()
#data = readLines("NetflixPrizeData/combined_data_1.txt")


library(dplyr)
df = data.frame()

for(data_id in 1:5){
  path = paste0("~/NetflixPrizeData/combined_data_", data_id ,".txt")
  data = readLines(path)
  total = length(data)
  
  start = Sys.time()
  for(i in 1:length(data)){
    
    #pb <- txtProgressBar(min = 0, max = total, style = 3)
    
    if(grepl(x = data[i], pattern = ":", fixed = TRUE)){
      num = strsplit(data[i], split = "")[[1]][1]
      num = as.numeric(num)
    }else{
      data_string = strsplit(data[i], split = ",")
      user = data_string[[1]][1]
      rating = data_string[[1]][2]
      date = data_string[[1]][3]

      df = bind_rows(df, bind_cols(item = num, user = user, rating = rating, date = date))
    }
    
    
    #setTxtProgressBar(pb, i)
    if(i %% 10000 == 0){
      cat("Dataset ", data_id, " out of ", 5, " | Percent completed: ", 100*i/total, "\n")
      #print(100*i/total)
      }
    };beepr::beep(1)
  end = Sys.time()
}


# create progress bar

total <- 20
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  # update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)



df = data.frame()


for(i in 1:length(data)){
  
  if(grepl(x = data[i], pattern = ":", fixed = TRUE)){
    num = strsplit(data[i], split = "")[[1]][1]
    num = as.numeric(num)
  }else{
    user = strsplit(data[i], split = ",")[[1]][1]
    rating = strsplit(data[i], split = ",")[[1]][2]
    date = strsplit(data[i], split = ",")[[1]][3]
    
    obs_i = data.frame(item = num, user, rating, date)
    #obs_i
    
  }
  
  df = bind_rows(df, obs_i)
};beepr::beep(1)

df

data[2]
