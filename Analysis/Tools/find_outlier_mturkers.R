

find_outlier_mturkers <- function(raw_data, int_data, fd_data, non_responses, blur_focus, inst_tests, del_too_fast, chosen_bias){
  # warnings collected throughout the experiment
  warnings <- subset(raw_data, 
                     category %in% c("respond_faster",
                                     "deliberation_too_fast",
                                     "deliberation_too_slow",
                                     "missed_instruction_checkup",
                                     "no_registration_of_computer_choice")) %>% 
    group_by(PID,category) %>% dplyr::summarize(n=n())
  # interactive data (blur focus)
  interactive_data <- int_data %>% 
    group_by(PID,event) %>% 
    dplyr::summarize(n=n()) %>%
    dplyr::rename(category = event)
  
  # merge sets
  behavior <- bind_rows(warnings, interactive_data) %>% arrange(PID, category)
  
  # choices for chosen pairs
  if (length(fd_data) > 0) {
    chosen_p_gain <- subset(fd_data, chosen_trial == 1) %>%
      group_by(PID) %>%
      dplyr::summarize(p_gain = mean(higher_outcome_chosen,na.rm=1))
  } else {
    chosen_p_gain <- data.frame(PID = unique(raw_data$PID), p_gain = NaN)
  }
  behavior <- merge(behavior,chosen_p_gain,by="PID")
  
  # find subjects with above criterion events
  behavior <- mutate(behavior,
                     outlier = ifelse((category=="respond_faster" & n > non_responses) |
                                        (category=="deliberation_too_fast" & n > del_too_fast) |
                                        (category=="blur" & n > blur_focus) | 
                                        (category=="missed_instruction_checkup" & n > inst_tests) | 
                                        (p_gain < chosen_bias),1,0)) %>% arrange(PID)
  
  behavior_outliers <- subset(behavior, outlier==1)
  
  outliers <- as.character(unique(behavior_outliers$PID))
  
  outlier_list <- list(behavior,behavior_outliers,outliers)
  names(outlier_list) <- c("all_behavior","behavior_outliers","outliers")
  
  return(outlier_list)
}