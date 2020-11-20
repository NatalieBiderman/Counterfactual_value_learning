
# The following functions allow to pre-process csv files from MTurk experiments.

# If packages are not installed, install. Then, load libraries. 
list_of_packages <- c("tidyr", "dplyr", "stringr", "mosaic") 
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(list_of_packages, require, character.only = TRUE)

# -------------------
# preprocess the data
# -------------------

# This function rearranges raw csv files from MTurk experiments into lists that include the different stages of our
# experiment (e.g., Deliberation phase).

process_data <- function(Exp, all_data, all_data_int){
  
  # ================================================
  # 1. Make general adjustments to experimental data
  # ================================================
  
  # Transform all "null" values to "NA" 
  all_data[all_data == "null"] = NA
  
  # Divide rt by 1000 to get secs
  rt_cols <- colnames(all_data)[str_detect(colnames(all_data), "rt")][-1] # we also remove the first element that includes "start_time" 
  for (i in 1:length(rt_cols)){
    all_data[,rt_cols[i]] <- as.numeric(as.character(all_data[,rt_cols[i]]))/1000
  }
  
  # Add experimental group (sorted by date of experiment) to all_data 
  exp_groups <- subset(all_data[all_data$trial_index==0,c("PID","start_time")], !is.na(start_time))
  exp_groups$group <- str_sub(exp_groups$start_time, 1, 9)
  all_data <- left_join(all_data, exp_groups[,c("PID", "group")], by="PID")
  
  # Add Exp type to all_data
  all_data$Exp <- Exp
  
  # Divide trials according to phase type per subject 
  # (this will make it easier to verify where blurs and focus events occured)
  if (Exp %in% c("Pilot", "Exp4")){
    start_phase_category <- c("full_screen","rating_instructions", "deliberation_instructions", "reward_learning_instructions", "final_decisions_instructions",  "memory_test_instructions", "regret_intro","debreif_age","debreif_end")
    phase_category <- c("enter_full_screen","rating","deliberation","reward_learning","final_decisions","memory_test","regret","debreif")
  } else if (Exp %in% c("Exp2", "Exp3")){
    start_phase_category <- c("full_screen","rating_instructions", "deliberation_instructions", "reward_learning_instructions", "final_decisions_instructions", "memory_test_instructions", "final_ratings_instructions", "debreif_intro","debreif_end")
    phase_category <- c("enter_full_screen","rating","deliberation","reward_learning","final_decisions","memory_test","final_ratings","debreif")
  } else if (Exp %in% c("Exp1")){
    start_phase_category <- c("full_screen","rating_instructions", "deliberation_instructions", "reward_learning_instructions", "final_decisions_instructions", "memory_test_instructions", "final_ratings_instructions",  "outcome_evaluation_instructions" ,"debreif_intro","debreif_end")
    phase_category <- c("enter_full_screen","rating","deliberation","reward_learning","final_decisions","memory_test","final_ratings","outcome_evaluation","debreif")
  }
  
  subs <- unique(all_data$PID)
  for (s in 1:length(subs)){
    for (i in 1:length(phase_category)){
      start_ind <- which(all_data$PID==subs[s] & all_data$category==start_phase_category[i])[1]
      end_ind <- which(all_data$PID==subs[s] & all_data$category==start_phase_category[i+1])[1]
      if(!any(is.na(start_ind) | is.na(end_ind))){
        all_data[c(start_ind:end_ind),"phase"] <- phase_category[i]
      }
    }
  }
  
  # =========================================
  # 2. Include phase type in interactive data
  # =========================================
  
  all_data_int <- merge(all_data_int, exp_groups[,c("PID", "group")], by="PID")
  
  # Add phase type in interactive data
  subs <- as.character(unique(all_data_int$PID))
  for (s in 1:length(subs)){
    sub_data <- subset(all_data,PID==subs[s])
    all_data_int$phase[all_data_int$PID==subs[s]] <- 
      sub_data$phase[c(all_data_int$trial[all_data_int$PID==subs[s]])+1]
  }
  
  # ==============================
  # 3. Divide the data into phases
  # ==============================
  
  # To seperate the data file into phases, we use specific column names for each phase. 
  # Becasue the experiments include slightly different column names, we assign column names 
  # per experiment seperately. 
  
  rating_names <- c("PID","category","index","stimulus_id","response","rt","group","Exp")
  deliberation_names <- c("PID","category","block","index","reward_type","stimulus_left","stimulus_right","rating_left","rating_right","left_chosen","rt","chosen_obj","unchosen_obj","explain_trial","explain_response","group","Exp")  
  reward_names <- c("PID","category","block","index","stimulus_id","reward_type","reward_amount","see_reward_rt","register_reward_response","register_reward_rt","group","Exp")
  final_decisions_names <- c("PID","category","block","index","stimulus_left","stimulus_right", "reward_type_left", "reward_type_right","chosen_trial","left_chosen","rt","higher_outcome_chosen","group","Exp")
  memory_names <- c("PID","category","index","old_pair","stimulus_left","stimulus_right","old_response","rt_pairs","chosen_object","left_object_chosen", "rt_object","group","Exp")
  final_auction_names <- c("PID","category","index","stimulus_id","painting","rating","response","rt","group","Exp")
  final_ratings_names <- c("PID","category","index","deliberated_stim","chosen_obj","reward_type","stimulus_id","painting","initial_rating","new_rating","rt", "group","Exp")
  outcome_evaluation_names <- c("PID","category","index","stimulus_id","painting","initial_rating","chosen_obj","reward_type","outcome_eval_response","outcome_eval_gain","outcome_eval_rt", "outcome_eval_acc","outcome_eval_confidence","outcome_eval_confidence_rt","group","Exp")
  if ("same_painter" %in% colnames(all_data)){
    rating_names <- append(rating_names,c("critical","painting","painter"),after=4)
    deliberation_names <- append(deliberation_names,c("same_painter","painting_left","painting_right","painter_left","painter_right"),after=4)
    final_decisions_names <- append(final_decisions_names,c("painting_left","painting_right","painter_left","painter_right","same_painter"),after=10)
    memory_names <- append(memory_names,c("painting_left","painting_right","painter_left","painter_right", "del_same_painter","memory_same_painter"), after=6)
  } else if ("repeat_cond" %in% colnames(all_data)){
    rating_names <- append(rating_names,"painting",after=4)
    deliberation_names <- append(deliberation_names, c("repeat_cond","pair_repetitions","painting_left","painting_right"), after=4)
    final_decisions_names <- c("PID","category","block","index","repeat_cond","chosen_trial","gain_left","stimulus_left","stimulus_right","painting_left","painting_right","rating_left","rating_right","stimulus_left_del_ind","stimulus_right_del_ind","left_chosen","rt","higher_outcome_chosen","chosen_obj","unchosen_obj","group","Exp")
    final_ratings_names <- append(final_ratings_names, "repeat_cond", after=4)
    memory_names <- append(memory_names,"repeat_cond", after=6)
  } else if ("reward_cond" %in% colnames(all_data)) { 
    rating_names <- append(rating_names,"painting",after=4)
    deliberation_names <- append(deliberation_names, c("reward_cond","reward_amount","pair_repetitions","painting_left","painting_right"), after=4)
    reward_names <- append(reward_names, c("reward_cond"), after=6)
    final_decisions_names <- c("PID","category","block","index","reward_cond","chosen_trial","gain_left","stimulus_left","stimulus_right","painting_left","painting_right","rating_left","rating_right","stimulus_left_del_ind","stimulus_right_del_ind","left_chosen","rt","higher_outcome_chosen","chosen_obj","unchosen_obj","group","Exp")
    final_ratings_names <- append(final_ratings_names, "reward_cond", after=4)
    memory_names <- append(memory_names,"reward_cond", after=6)
  } else if (Exp %in% c("Exp1")){
    rating_names <- append(rating_names,"painting",after=4)
    deliberation_names <- append(deliberation_names, c("painting_left","painting_right"), after=4)
    final_decisions_names <- c("PID","category","block","index","chosen_trial","gain_left","stimulus_left","stimulus_right","painting_left","painting_right","rating_left","rating_right","stimulus_left_deliberation_ind","stimulus_right_deliberation_ind","left_chosen","rt","higher_outcome_chosen","chosen_obj","unchosen_obj","group","Exp")
  }
  
  # --------- Rating ---------
  
  rating <- all_data[all_data$category=="rating", rating_names]
  
  # z-score ratings per subject
  rating <- rating %>% group_by(PID) %>% dplyr::mutate(normalized_rating = zscore(response))
  
  # --------- Deliberation ---------
  
  deliberation_all <- all_data[all_data$category=="deliberation",deliberation_names]
  deliberation_all <- subset(deliberation_all, !is.na(left_chosen))
  
  # add explain_trial response to relevant deliberation trial
   deliberation_all$explain_response[deliberation_all$explain_trial==1] <- 
    subset(all_data,category=="explain_trial")[,"explain_response"]
  
  # add normalazied delta rate
  subs <- unique(deliberation_all$PID); deliberation_tmp <- c()
  for (s in subs){
    rating_sub <- subset(rating, PID==s)
    deliberation_sub <- subset(deliberation_all, PID==s)
    for (t in 1:nrow(deliberation_sub)){
      deliberation_sub$norm_rating_left[t] <- rating_sub$normalized_rating[rating_sub$stimulus_id == deliberation_sub$stimulus_left[t]] 
      deliberation_sub$norm_rating_right[t] <- rating_sub$normalized_rating[rating_sub$stimulus_id == deliberation_sub$stimulus_right[t]] 
      deliberation_sub$norm_delta_rate[t] <- deliberation_sub$norm_rating_left[t] - deliberation_sub$norm_rating_right[t]
    }
    deliberation_tmp <- bind_rows(deliberation_tmp,deliberation_sub)
  }
  deliberation_all <- deliberation_tmp
  deliberation_all$delta_rate <- deliberation_all$rating_left - deliberation_all$rating_right

  # add higher_rated info 
  deliberation_all <- mutate(deliberation_all,
                             left_rated_higher = ifelse(rating_left > rating_right, 1, 0),
                             higher_rated_chosen = ifelse(left_chosen==left_rated_higher, 1, 0))
  
  # For all experiments the analysis is performed on the last deliberaiton block.
    deliberation_all$number_of_no_responses <- 0
    tag_no_response <- 0
    for (i in 1:nrow(deliberation_all)-1){
      tag_no_response[i] <- ifelse(deliberation_all$index[i]==deliberation_all$index[i+1], 1, 0)
      deliberation_all$number_of_no_responses[i] <- 
        sum(tag_no_response[deliberation_all$index==deliberation_all$index[i]], na.rm = 1)
    }
    deliberation_all <- subset(deliberation_all, !is.na(left_chosen))
    
    # add a column counting how many times an object was chosen before the last block
    subs <- unique(deliberation_all$PID)
    deliberation_all$choice_consistency <- NaN
    final_block <- deliberation_all$block[nrow(deliberation_all)] # find final block for that Exp
    for (s in 1:length(subs)){
      sub_data_final_block <- deliberation_all[deliberation_all$PID==subs[s] &
                                                 deliberation_all$block==final_block,]
      sub_data_first_blocks <- deliberation_all[deliberation_all$PID==subs[s] &
                                                  deliberation_all$block!=final_block,]
      choice_consistency <- NaN
      for (i in 1:nrow(sub_data_final_block)){
        choice_consistency[i] <- sum(sub_data_final_block[i,"chosen_obj"] == sub_data_first_blocks$chosen_obj)
      }
      deliberation_all$choice_consistency[deliberation_all$PID==subs[s] & 
                                            deliberation_all$block==final_block] <- choice_consistency
    }
    # Use only the third test deliberation block
    deliberation <- subset(deliberation_all, block==final_block)

  # --------- Reward learning ---------
  
  # we use "see reward" trials, and then add to the data missing info from reward outcome trials
  reward_learning <- all_data[all_data$category=="see_reward", reward_names]
  # because the reward outcome is presented for all trials, number and order of trials for both types of 
  # trials is identical. we can just copy the rt and resposes of the reward_outcome trials
  reward_learning$register_reward_rt <- all_data[all_data$category=="reward_outcome","register_reward_rt"]
  reward_learning$register_reward_response <- all_data[all_data$category=="reward_outcome", 
                                                       "register_reward_response"]
  reward_learning$accuracy <- ifelse(reward_learning$register_reward_response==reward_learning$reward_type,1,0)
  
  # --------- memory --------- 
  
  memory <- all_data[all_data$category=="memory_pairs", memory_names]
  
  # add object choice and rt to memory matrix
  memory$left_object_chosen[memory$old_response==1] <- 
    all_data$left_object_chosen[all_data$category=="memory_chosen_object"]
  memory$rt_object[memory$old_response==1] <- all_data$rt_object[all_data$category=="memory_chosen_object"]
  
  # add accuracy columns
  # object_acc = computed only for old responses, if the pair was new, this is automatically 0.
  memory <- mutate(memory,
                   chosen_object_response = ifelse(old_response==1, 
                                          ifelse(left_object_chosen==1, stimulus_left,stimulus_right),NA),
                   pair_acc = ifelse(old_pair==old_response,1,0),
                   object_acc = ifelse(old_response==0, NA,
                                       ifelse(is.na(chosen_object) | 
                                                chosen_object_response!=chosen_object, 0, 1)))
  # choice_acc = here we compute overall choice memory, so, for new trials, if subjects chose an item that was previously chosen but not in this pair, this counts as a correct response
  subs <- unique(memory$PID); memory_tmp <- c()
  for (s in subs){
    memory_sub <- subset(memory, PID==s)
    all_chosen_objects <- memory_sub$chosen_object[!is.na(memory_sub$chosen_object)]
    memory_sub <- mutate(memory_sub, 
                         choice_acc = ifelse(old_response==0, NA,
                                             ifelse(chosen_object_response %in% all_chosen_objects,1,0)))
    memory_tmp <- bind_rows(memory_tmp,memory_sub)
  }
  memory <- memory_tmp

  # --------- Final Rating ---------
  
  if ("final_ratings" %in% unique(all_data$category)){
    
    final_ratings <- all_data[all_data$category=="final_ratings", final_ratings_names]
    
    # zscore final and initial ratings (we do that seperately for each rating stage)
    final_ratings <- final_ratings %>% group_by(PID) %>% 
      dplyr::mutate(normalized_initial_rating = zscore(initial_rating),
                    normalized_new_rating = zscore(new_rating),
                    diff_rating = new_rating - initial_rating)
    
    # compute a normalized rating difference between final and initial ratings. 
    # Note that we want to make sure they are on the same scale, so we collapse the two rating 
    # types together, and only then zscore them to get the diff rating score
    diff_norm_ratings <- final_ratings %>%
      dplyr::select(Exp, PID, index, initial_rating, new_rating) %>%
      group_by(Exp, PID) %>%
      gather(rating_type, rating, c(initial_rating,new_rating)) %>%
      mutate(norm_rating = zscore(rating)) %>%
      dplyr::select(-rating) %>%
      spread(rating_type,norm_rating) %>%
      #rename(norm_init_rating = initial_rating, norm_fin_rating = new_rating) %>%
      mutate(diff_norm_rating = new_rating-initial_rating) %>%
      dplyr::select(-c(initial_rating, new_rating))
    
    final_ratings <- merge(final_ratings, diff_norm_ratings, by=c("Exp","PID","index"))
    
  }
  
  # --------- Final decisions ---------
  
  final_decisions <- all_data[all_data$category=="final_decisions", final_decisions_names]
  final_decisions$higher_outcome_chosen[is.na(final_decisions$left_chosen)] <- NA
  
  # center chosen_trial for all exps
  final_decisions <- mutate(final_decisions,
                            chosen_trial_centered = ifelse(chosen_trial==0,-1,1),
                            chosen_trial_name = ifelse(chosen_trial==1,"Chosen","Unchosen"))
  
  # add missing variables
  if (!"gain_left" %in% colnames(all_data)){
    final_decisions <- mutate(final_decisions,
                              gain_left = ifelse(reward_type_left==1,1,0),
                              chosen_obj = ifelse(left_chosen==1,stimulus_left,stimulus_right),
                              unchosen_obj = ifelse(left_chosen==1,stimulus_right,stimulus_left))
  }
  if ("same_painter" %in% colnames(all_data)){
    final_decisions <- mutate(final_decisions,same_painter_centered = ifelse(same_painter==0,-1,1))
  }
  if ("repeat_cond" %in% colnames(all_data)){
    final_decisions <- mutate(final_decisions,repeat_cond_centered = ifelse(repeat_cond==0,-1,1))
  }
  
  # add normalazied delta rate
  subs <- unique(final_decisions$PID); final_decisions_tmp <- c()
  for (s in subs){
    rating_sub <- subset(rating, PID==s)
    final_decisions_sub <- subset(final_decisions, PID==s)
    for (t in 1:nrow(final_decisions_sub)){
      final_decisions_sub$norm_rate_left[t] <- rating_sub$normalized_rating[rating_sub$stimulus_id == final_decisions_sub$stimulus_left[t]] 
      final_decisions_sub$norm_rate_right[t] <- rating_sub$normalized_rating[rating_sub$stimulus_id == final_decisions_sub$stimulus_right[t]] 
      }
    final_decisions_tmp <- bind_rows(final_decisions_tmp,final_decisions_sub)
  }
  final_decisions <- final_decisions_tmp %>%
    mutate(norm_delta_rate = norm_rate_left - norm_rate_right,
           norm_drate_by_outcome = ifelse(gain_left==1, norm_rate_left - norm_rate_right, norm_rate_right - norm_rate_left))
  
  # Add pair acc as a subject-level predictor to final decisions matrix
  add_memory_acc <- function(cond_name){
    if (length(cond_name)!=0){
      memory_acc <- memory %>% group_by(.dots=c("PID",cond_name)) %>% 
        dplyr::summarize(pair_acc = mean(pair_acc,na.rm=1), object_acc = mean(object_acc,na.rm=1), choice_acc = mean(choice_acc,na.rm=1)) 
      memory_acc <- memory_acc %>% group_by(.dots=c(cond_name)) %>% dplyr::mutate(pair_acc_zscored = zscore(pair_acc), object_acc_zscored = zscore(object_acc,na.rm=1), choice_acc_zscored = zscore(choice_acc,na.rm=1))
      if ("same_painter" %in% colnames(all_data)){
        memory_acc <- memory_acc %>% dplyr::rename(same_painter = del_same_painter)
        cond_name <- "same_painter"
        }
    } else {
      memory_acc <- memory %>% group_by(PID) %>% 
        dplyr::summarize(pair_acc = mean(pair_acc,na.rm=1), object_acc = mean(object_acc,na.rm=1), choice_acc = mean(choice_acc,na.rm=1)) 
      memory_acc <- memory_acc %>% dplyr::mutate(pair_acc_zscored = zscore(pair_acc), object_acc_zscored = zscore(object_acc,na.rm=1), choice_acc_zscored = zscore(choice_acc,na.rm=1))
    }
    final_decisions <- merge(final_decisions, memory_acc, by=c("PID",cond_name))
    return(final_decisions)
  }
  
  if ("same_painter" %in% colnames(all_data)){final_decisions <- add_memory_acc("del_same_painter")
  } else if ("repeat_cond" %in% colnames(all_data)){final_decisions <- add_memory_acc("repeat_cond")
  } else if ("reward_cond" %in% colnames(all_data)){final_decisions <- add_memory_acc("reward_cond")
          } else {final_decisions <- add_memory_acc(c())}
  
  # add condition name per experiment
  if ("same_painter" %in% colnames(all_data)){
    final_decisions <- mutate(final_decisions, cond_name = ifelse(same_painter==1,"Same painter", "Different painter"), 
                              cond_name = factor(cond_name, levels = c("Same painter", "Different painter")),
                              cond_logical = ifelse(same_painter==1, 1, 0))
  } else if ("repeat_cond" %in% colnames(all_data)){
    final_decisions <- mutate(final_decisions, cond_name = ifelse(repeat_cond==1,"High repetition", "Low repetition"),
                              cond_name = factor(cond_name, levels = c("High repetition", "Low repetition")),
                              cond_logical = ifelse(repeat_cond==1, 1, 0))
  } else if ("reward_cond" %in% colnames(all_data)){
    final_decisions <- mutate(final_decisions, cond_name = ifelse(reward_cond==1,"High reward", "Low reward"),
                              cond_name = factor(cond_name, levels = c("High reward", "Low reward")),
                              cond_logical = ifelse(reward_cond==1, 1, 0))
  }
  
  # --------- Outcome Evaluation ---------
  
  if ("outcome_evaluation" %in% unique(all_data$category)){
    outcome_evaluation <- all_data[all_data$category=="outcome_evaluation", outcome_evaluation_names]
    outcome_evaluation_confidence <- all_data[all_data$category=="outcome_evaluation_confidence", c(outcome_evaluation_names,"rt")]
    outcome_evaluation$outcome_eval_confidence <- outcome_evaluation_confidence$outcome_eval_confidence
    outcome_evaluation$outcome_eval_confidence_rt <- outcome_evaluation_confidence$rt
  }
  
  # =====================
  # Add Demographics Info
  # =====================
  
  # create general demographics
  demographics <- all_data[all_data$category %in% c("debreif_age", "debreif_gender", "debreif_dominant_hand", "debreif_native_langugage","debreif_fluency"), 
                    c("PID","category","responses")]
  # remove non-relevant characters 
  demographics$responses <- gsub("[[:space:]]", "",str_remove(gsub("[[:punct:]]", "", demographics$responses),"Q0"))
  # (1) remove the word debreif, (2) spread categories, (3) turn numbers to numeric values
  demographics <- mutate(demographics,
                         new_category = str_remove(as.character(category),"debreif_")) %>%
    dplyr::select(-category) %>%
    spread(new_category, responses) %>%
    mutate_at(c("age","fluency"),as.numeric)
  # add experiment name
  demographics$Exp <- Exp
  
  # include regret scores for experiments that included regret qs
  if ("regret_intro" %in% all_data$category){
    # Compute regret measure
    regret_mat <- all_data[grepl("regret_qs",all_data$category),c("PID","category","responses")]
    regret_mat$responses <- as.numeric(gsub("\\D", "", regret_mat$responses)) + 1 # remove all non-numeric characters, add 1 because the scale is from 1 to 7, but in javascript it turns it to 0-6
    regret_mat <- spread(regret_mat, category, responses) # spread
    regret_mat$regret_qs1 <- 7 - regret_mat$regret_qs1 + 1 # reverse the score of the first question
    # compute average regret, and keep only that column
    regret_mat <- regret_mat %>%
      group_by(PID) %>%
      dplyr::mutate(regret_score = mean(regret_qs1:regret_qs5,na.rm=1))
    demographics <- left_join(demographics, regret_mat, by = "PID")
  }
  
  # include art experience for experiments that asked for that
  if ("debreif_art_experience" %in% all_data$category){
    art_exp <- all_data %>%
      group_by(PID) %>% 
      dplyr::summarize(art_experience = gsub("[[:space:]]", "",str_remove(gsub("[[:punct:]]", "", responses[category=="debreif_art_experience"]),"Q0")),
                       art_time_spent = as.numeric(gsub("\\D", "", responses[category=="debreif_art_time_spent"])))
    demographics <- left_join(demographics, art_exp, by="PID")
  }

  # include corona questions for experiments that included corona qs
  if ("coronavirus_intro" %in% all_data$category){
    # Compute regret measure
    corona_qs <- all_data %>%
      group_by(PID) %>% 
      dplyr::summarize(corona_anxiety = as.numeric(gsub("\\D", "", responses[category=="corona_anxiety"])),
                       corona_general_performance = as.numeric(gsub("\\D", "", responses[category=="corona_general_performance"])),
                       corona_task_performance = as.numeric(gsub("\\D", "", responses[category=="corona_task_performance"])),
                       corona_free_text = gsub("[[:punct:]]", "", responses[category=="corona_performance_text"],"Q0"))
    demographics <- left_join(demographics, corona_qs, by="PID")
  }
  
  # ========================
  # Add Strategies responses
  # ========================
  
  strategies <- all_data[grepl("debreif_strategy",all_data$category),c("PID","category","responses")]
  strategies$responses <- str_remove(gsub("[[:punct:]]", "", strategies$responses),"Q0")
  strategies$category <- str_remove(strategies$category,"debreif_")
  strategies$Exp <- Exp
  strategies <- strategies %>% spread(category, responses)
  
  # ======================
  # Create and save a list 
  # ======================
  
  # Combine all data sets to list
  data_list <- list(all_data, all_data_int, rating, deliberation_all, deliberation, reward_learning, final_decisions, memory)
  names(data_list) <- c("all_data", "interactive_data", "rating", "deliberation_all", "deliberation", "reward_learning", "final_decisions", "memory")
  if (exists("final_ratings")){
    data_list[[length(data_list)+1]] <- final_ratings
    names(data_list)[length(data_list)] <- "final_ratings"
  }
  if (exists("outcome_evaluation")){
    data_list[[length(data_list)+1]] <- outcome_evaluation
    names(data_list)[length(data_list)] <- "outcome_evaluation"
  }
  data_list[[length(data_list)+1]] <- demographics; data_list[[length(data_list)+1]] <- strategies; 
  names(data_list)[c(length(data_list)-1,length(data_list))] <- c("demographics","strategies")
  
  return(data_list)
}

# ---------------------
# Find outlier Mturkers
# ---------------------

# This function finds outlier MTurkers according to predefined exclusion criteria. 

find_outlier_mturkers <- function(full_data_list, non_responses_criterion, blur_focus_criterion, inst_tests_criterion, del_too_fast_criterion, chosen_bias_criterion){
  # warnings collected throughout the experiment
  warnings <- subset(full_data_list$all_data, 
                     category %in% c("respond_faster",
                                     "deliberation_too_fast",
                                     "deliberation_too_slow",
                                     "missed_instruction_checkup",
                                     "no_registration_of_computer_choice")) %>% 
    group_by(PID,phase,category) %>% dplyr::summarize(n=n())
  # interactive data (blur focus)
  interactive_data <- full_data_list$interactive_data %>% 
    group_by(PID,phase,event) %>% 
    dplyr::summarize(n=n()) %>%
    dplyr::rename(category = event)
  
  # merge sets
  behavior <- bind_rows(warnings, interactive_data) %>% arrange(PID, phase, category)
  
  # choices for chosen pairs
  chosen_bias <- subset(full_data_list$final_decisions, chosen_trial == 1) %>%
    group_by(PID) %>%
    dplyr::summarize(chosen_bias = mean(higher_outcome_chosen,na.rm=1))
  behavior <- merge(behavior,chosen_bias,by="PID")
  
  # find subjects with above criterion events
  behavior <- mutate(behavior,
                     outlier = ifelse((category=="respond_faster" & n > non_responses_criterion) |
                                      (category=="deliberation_too_fast" & n > del_too_fast_criterion) |
                                      (category=="blur" & n > blur_focus_criterion) | 
                                      (category=="missed_instruction_checkup" & n > inst_tests_criterion) | 
                                      (chosen_bias < chosen_bias_criterion),1,0)) %>%
    arrange(PID, phase)
  
  behavior_outliers <- subset(behavior, outlier==1)
  
  outliers <- as.character(unique(behavior_outliers$PID))
  
  outlier_list <- list(behavior,behavior_outliers,outliers)
  names(outlier_list) <- c("all_behavior","behavior_outliers","outliers")
  
  return(outlier_list)
}
  
 
# -----------------------
# Remove outlier Mturkers
# -----------------------

# This function recieves data with all participants and a list of outliers and outputs data without these outliers.

remove_outlier_mturk <- function(data_list, outlier_mturk){
  new_data_list <- list()
  if (any(!is.nan(outlier_mturk))){
    for (i in 1:length(data_list)){
      if (names(data_list)[i] %in% c("all_data", "interactive_data")){
        new_data_list[[i]] <- data_list[[i]] # don't remove outlier subjects from all data list
      } else {
        new_data_list[[i]] <- subset(data_list[[i]],!PID %in% outlier_mturk)
      }
      if (names(data_list)[i] == "all_data"){
        new_data_list[[i]] <- new_data_list[[i]] %>% 
          mutate(is_outlier = ifelse(PID %in% outlier_mturk, 1, 0))
      }
    }
    names(new_data_list) <- names(data_list)
  } else {
    new_data_list <- data_list
  }
  return(new_data_list)
}

# -------------
# Save csv data
# -------------

# This function creates summary csv files using individual files of participants. 

save_csv_data <- function(exp_name, ind_data_folder, summary_data_folder){
  
  exp_data_files <- list.files(sprintf("%s/Individual_data/",ind_data_folder),pattern="*.csv")
  exp_data = do.call(rbind, lapply(exp_data_files,
                                   function(x) read.csv(sprintf("%s/Individual_data/%s",ind_data_folder,x), stringsAsFactors = FALSE)))
  
  int_data_files <- list.files(sprintf("%s/Interactive_data/",ind_data_folder),pattern="*.csv")
  int_data = do.call(rbind, lapply(int_data_files,
                                   function(x) read.csv(sprintf("%s/Interactive_data/%s",ind_data_folder,x), stringsAsFactors = FALSE)))
  # save
  write.csv(exp_data, file = sprintf("%s/Experimental_data/data_%s.csv",summary_data_folder, exp_name))
  write.csv(int_data, file = sprintf("%s/Interactive_data/int_data_%s.csv",summary_data_folder, exp_name))
}

