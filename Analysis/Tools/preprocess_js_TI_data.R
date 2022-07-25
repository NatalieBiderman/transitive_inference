
preprocess_js_TI_data <- function(data_path,summary_data_path){
  
  # If packages are not installed, install. Then, load libraries. 
  list_of_packages <- c("dplyr","tibble","mosaic") 
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(list_of_packages, require, character.only = TRUE)
  
  # ----------------------------------------------------------------------------
  # Create csvs for the different tasks 
  # ----------------------------------------------------------------------------

  tmp_df <- create_csv_from_js_data(
    data_folder = sprintf("%s/Individual_data/",data_path),
    summary_data_folder = summary_data_path,
    file_name = "df_TI.csv",
    columns = c("ttype","PID", "index", "block","phase","pair_type","S1_stim", "S2_stim", "S1_img",	"S2_img",	"rt","S1_chosen", "S1_higher",	"accuracy",	"keypress_iti",	"keypress_isi","keypress_S2"),
    main_trial = c("response","ITI","ISI","S2"),
    ttype_name = "ttype",
    save_raw_data = 1,
    raw_name = "raw_data")
  
  
  # ----------------------------------------------------------------------------
  # Clean data frames
  # ----------------------------------------------------------------------------
  
  # create a function for zscoring rt and ratings
  compute_zscored_column <- function(data, zscored_col, new_zscored_col_name){
    # check if rt columns is char or numeric
    if (class(unlist(as.vector(data[zscored_col]))) == "character"){
      data[zscored_col] <- as.numeric(unlist(as.vector(data[zscored_col])))
      }
    data <- data %>%
      group_by(PID) %>%
      dplyr::mutate(zscored_col = zscore(!!sym(zscored_col), na.rm=TRUE)) 
    names(data)[names(data) == "zscored_col"] = new_zscored_col_name
    return(data)
    }
  
  # zscore the rt
  tmp_df <- compute_zscored_column(tmp_df, "rt", "zscored_rt")
  
  # rearrange responses outside response window 
  response <- tmp_df %>% subset(ttype == "response") %>% dplyr::select(-c(keypress_S2, keypress_iti, keypress_isi))
  s2_response <- tmp_df %>% subset(ttype == "S2") %>% dplyr::select(PID, index, keypress_S2, rt, zscored_rt) %>% rename(rt_keypress_s2 = rt, zscored_rt_keypress_s2 = zscored_rt) 
  iti_response <- tmp_df %>% subset(ttype == "ITI") %>% dplyr::select(PID, index, keypress_iti, rt, zscored_rt) %>% rename(rt_keypress_iti = rt, zscored_rt_keypress_iti = zscored_rt)
  isi_response <- tmp_df %>% subset(ttype == "ISI") %>% dplyr::select(PID, index, keypress_isi, rt, zscored_rt) %>% rename(rt_keypress_isi = rt, zscored_rt_keypress_isi = zscored_rt)
  
  df <- merge(merge(merge(response, s2_response, by=c("PID", "index")),iti_response, by=c("PID", "index")), isi_response, by=c("PID", "index")) %>%
    dplyr::arrange(PID, block, index)
  
  # save
  write.csv(df, file = sprintf("%s/df_TI.csv",summary_data_path))

  # ----------------------------------------------------------------------------
  # Create debrief csv
  # ----------------------------------------------------------------------------
  
  # load raw data 
  raw_df <- read.csv(sprintf("%s/Raw_data/raw_data.csv",summary_data_path))
  
  # get debreif questions 
  raw_debreif <- raw_df %>%
    subset(grepl("debreif", ttype)) %>% 
    dplyr::select(c("PID", "ttype", "responses"))
  
  debreif <- raw_debreif %>%
    mutate(responses = gsub("[[:space:]]", " ",str_remove(gsub("[[:punct:]]", "", responses),"Q0"))) %>%
    mutate(new_ttype = str_remove(as.character(ttype),"debreif_")) %>%
    dplyr::select(-ttype) %>%
    spread(new_ttype, responses) %>%
    dplyr::select(-c("end", "intro"))
  
  write.csv(debreif, file = sprintf("%s/debreif.csv",summary_data_path))
  #write.csv(raw_debreif, file = sprintf("%s/debreif.csv",summary_data_path))
  
  # ----------------------------------------------------------------------------
  # Create interactive data frame
  # ----------------------------------------------------------------------------
  
  int_data <- create_interactive_csv_from_js_data(
    int_data_folder = sprintf("%s//Interactive_data/",data_path), 
    exp_data_folder = sprintf("%s/Individual_data/",data_path),
    ttype_name = "ttype")
  
  write.csv(int_data, file = sprintf("%s/interaction.csv",summary_data_path))
  
}