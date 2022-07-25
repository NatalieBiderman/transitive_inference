create_csv_from_js_data <- function(data_folder, summary_data_folder, file_name, columns, main_trial, ttype_name, save_raw_data, raw_name){
  
  # If packages are not installed, install. Then, load libraries. 
  list_of_packages <- c("dplyr") 
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(list_of_packages, require, character.only = TRUE)
  
  data_files <- list.files(sprintf("%s/",data_folder),pattern="*.csv")
  exp_data <- do.call(rbind, lapply(data_files,function(x) read.csv(sprintf("%s/%s",data_folder,x), stringsAsFactors = FALSE)))
  
  # convert rt to numeric 
  exp_data$rt <- as.numeric(exp_data$rt)
  
  # Save raw data 
  if (save_raw_data==1){
    write.csv(exp_data, file = sprintf("%s/Raw_data/%s.csv",summary_data_folder,raw_name))
  }
  
  # Choose specific columns 
  df <- exp_data[as.vector(unlist(exp_data[ttype_name]) %in% main_trial),columns]
  
  # save
  write.csv(df, file = sprintf("%s/%s",summary_data_folder, file_name))
  
  return(df)
}