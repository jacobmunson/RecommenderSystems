library(dplyr)
df_total = data.frame()

start_outer = Sys.time()
for(data_id in 1:4){
  message("Reading data...")
  path = paste0("NetflixPrizeData/combined_data_", data_id ,".txt")
  data = readLines(path)
  total = length(data)
  
  start = Sys.time()
  
  item_id_loc = rep(NA, length(data))
  
  # get location of items 
  message("Getting location of item headers...")
  for(i in 1:length(data)){ 
    
    if(grepl(x = data[i], pattern = ":", fixed = TRUE)){
      item_id_loc[i] = i
    }
    
    if(i %% 10000 == 0){
      cat("Dataset ", data_id, " out of ", 4, " | Percent item ID locations found: ", round(100*i/total,3), " %", "\n")
    }
  }
  
  item_loc_actual = item_id_loc[!is.na(item_id_loc)]
  df = data.frame(item_begin = item_loc_actual + 1, 
                  item_end = lead(item_loc_actual) - 1)
  df[nrow(df),"item_end"] = length(data)  
  
  lines_per_item = df %>% mutate(rows_per_item = item_end - item_begin) 
  
  message("Determining item locations...")
  item_id = rep(NA, length(item_loc_actual))
  for(i in 1:length(item_loc_actual)){
    item_id_i = strsplit(data[item_loc_actual][i], split = ":")[[1]][1]
    #print(item_id_i)
    item_id_i = as.numeric(item_id_i)  
    item_id[i] = item_id_i
  }
  
  df_rows_per_item = cbind(item_id, lines_per_item)
  
  message("Rearranging data to final form...")
  df_empty_data_frame = data.frame()
  for(i in 1:nrow(df_rows_per_item)){
    df_empty_data_frame = bind_rows(df_empty_data_frame,
                                    data.frame(item = df_rows_per_item[i,"item_id"], 
                                               string_item = data[df_rows_per_item[i,"item_begin"]:df_rows_per_item[i,"item_end"]]))
    #print(i)
  }
  
  message("Emergency dimensions check...")
  stopifnot(nrow(df_empty_data_frame) == (length(data) - nrow(df_rows_per_item)))

  
  message("Final formatting")
  df_empty_data_frame = df_empty_data_frame %>% 
    mutate(user = sapply(strsplit(string_item, ","), function(x) x[[1]][1]),
           rating = sapply(strsplit(string_item, ","), function(x) x[[2]][1]),
           date = sapply(strsplit(string_item, ","), function(x) x[[3]][1]))

  df_total = bind_rows(df_total, df_empty_data_frame)
  
  message("Clearing garbage data frames...")
  rm(df_empty_data_frame)
  rm(df_rows_per_item)
  rm(lines_per_item)
  rm(df)
  rm(data)
  rm(item_id_loc)
  gc()
  #beepr::beep(1)
  end = Sys.time()
  print(end - start)
}
end_outer = Sys.time()

print(end_outer - start_outer)

