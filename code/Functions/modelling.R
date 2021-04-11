# The following functions run Bayesian regression models. 

# -------------------------------------------------------------------
# p(select rewarded) ~ choice * ratings model - Final Decisions phase
# -------------------------------------------------------------------

# The function runs a Bayesian logistic regression predicting the tendency to choose a rewarded item (higher_outcome_chosen)
# as a function of choice type (chosen or unchosen pairs, chosen_trial_centered) and the difference in ratings between the
# rewarded and unrewarded items (norm_drate_by_outcome). For Experiments 2-4 the model also includes a third predictor that 
# describes the condition type (e.g., in Experiment 4, painter type) and interacts with all other predictors in the model.

run_choice_ratings_model <- function(data,third_predictor,params,Exp){
  # build the formula
  if (length(third_predictor)==0){
    third_predictor <- ""
  } else {
    third_predictor <- sprintf("*%s",third_predictor)
  }
  formula <- sprintf("higher_outcome_chosen ~ chosen_trial_centered%s*norm_drate_by_outcome +
                     (chosen_trial_centered%s*norm_drate_by_outcome | PID)",
                     third_predictor,third_predictor)
  # run the model
  M <- stan_glmer(data = data, 
                  eval(expr(formula)),
                  family = binomial(link="logit"), 
                  adapt_delta = params$adapt_delta, 
                  iter = params$iterations, 
                  chains = params$chains, 
                  warmup = params$warmup)
  # save it
  assign(sprintf("M_%s_choice_ratings",Exp),M)
  save(list = sprintf("M_%s_choice_ratings",Exp),
       file = sprintf("../data/Models/Choice_Ratings_models/Model_objects/M_%s_choice_ratings.RData",Exp))
} # end function

# --------------------------------------------
# create coef lists for choice * ratings model
# --------------------------------------------

# The following function rearranges the choice-ratings model to derive our two parameters of interest for chosen and unchosen 
# pairs seperately: (1) Slope: the influence of initial ratings on choice and (2) Intercept, the tendency to choose gain 
# items when there is no difference between ratings. 
# The following function rearranges the coefs to derive these parameters. We rearrange the position of the model coefficients 
# such that the first half are the coefficients that build up the intercept term and the last half are all the coefficients 
# that build up the slope term. Then, we add or substract these coefs according to the condition at hand. To this end, we 
# include a signs matrix that describes these operations. For example, if we want to compute the intercept and slope terms 
# for unchosen pairs and same painter in Experiment 4, we assign chosen_trial_centered=-1, and same_painter_centered=1. 
# We then get the following formula: (intercept - chosen + same_painter - chosen:same_painter) + 
# (drate - chosen:drate + same_painter:drate - chosen:drate:same_painter), for which the signs are 1,-1,1,-1,1,-1,1,-1.

create_choice_ratings_coef_list <- function(model_object, Exp, cond_names){
  
  # ==========================================================
  # assign signs and names of coefficients for each experiment 
  # ==========================================================
  
  # create the names of coefs to be used in the coef_signs matrix
  choice_names <- c("Chosen","Unchosen"); colnames_coefs <- c()
  for (i in 1:length(choice_names)){
    for (j in 1:length(cond_names)){
      colnames_coefs <- c(colnames_coefs, sprintf("%s_%s",choice_names[i],cond_names[j]))
    }
  }
  if (Exp %in% c("Exp1","Pilot")){
    coefs_names <- rownames(as.data.frame(fixef(model_object))); 
    coefs_signs <- data.frame(c(1,1,1,1), c(1,-1,1,-1))
    colnames(coefs_signs) <- colnames_coefs
  } else {
    coefs_names <- rownames(as.data.frame(fixef(model_object))); coefs_names[c(4,5)] <- coefs_names[c(5,4)];
    coefs_signs <- data.frame(c(1,1,1,1,1,1,1,1), c(1,1,-1,-1,1,1,-1,-1), 
                              c(1,-1,1,-1,1,-1,1,-1), c(1,-1,-1,1,1,-1,-1,1))
    colnames(coefs_signs) <- colnames_coefs
  }
  
  # ====================================
  # posterior draws of group-level coefs
  # ====================================
  
  sims <- as.data.frame(model_object)[,coefs_names]
  new_group_coefs <- as.data.frame(matrix(NaN,nrow=nrow(sims))); # same room
  intercept <- matrix(0,nrow=nrow(sims),ncol=ncol(coefs_signs)); # same room
  slope <- matrix(0,nrow=nrow(sims),ncol=ncol(coefs_signs)); # same room
  for (c in 1:ncol(coefs_signs)){
    for (r in 1:(nrow(coefs_signs)/2)){
      intercept[,c] <- intercept[,c] + coefs_signs[r,c]*sims[,coefs_names[r]]
      slope[,c] <- slope[,c] + coefs_signs[r+(nrow(coefs_signs)/2),c] * 
        sims[,coefs_names[r+(nrow(coefs_signs)/2)]]
    }
    new_group_coefs[,sprintf("Intercept_%s",colnames(coefs_signs)[c])] <- intercept[,c]
    new_group_coefs[,sprintf("Slope_%s",colnames(coefs_signs)[c])] <- slope[,c]
  }
  new_group_coefs <- new_group_coefs[2:ncol(new_group_coefs)]
  posterior_group_coefs <- cbind(sims,new_group_coefs)
  
  # =======================================
  # summary estimates for group-level coefs 
  # =======================================
  
  # compute medians and %95HDIs
  summary_group_coefs <- data.frame(
    cbind(colnames(posterior_group_coefs),
          posterior_interval(as.matrix(posterior_group_coefs), prob=0.95),
          sapply(posterior_group_coefs,FUN=median),
          sapply(posterior_group_coefs,FUN=mean))) 
  colnames(summary_group_coefs) <- c("coef","low95HDI","high95HDI","Median", "Mean")
  rownames(summary_group_coefs) <- 1:nrow(summary_group_coefs)
  # make sure the values are numeric
  if (class(summary_group_coefs$low95HDI)=="factor"){
    summary_group_coefs <- summary_group_coefs %>% 
      mutate(Median = as.numeric(levels(Median))[Median],
             Mean = as.numeric(levels(Mean))[Mean],
             low95HDI = as.numeric(levels(low95HDI))[low95HDI],
             high95HDI = as.numeric(levels(high95HDI))[high95HDI])
  }
  
  # ===================
  # subject-level coefs
  # ===================
  
  subs_coefs <- coef(model_object)[[1]]; 
  subs_coefs <- cbind(PID = rownames(subs_coefs), subs_coefs)
  rownames(subs_coefs) <- 1:nrow(subs_coefs)
  intercept <- as.data.frame(matrix(0,nrow=nrow(subs_coefs),ncol=ncol(coefs_signs))); # same room
  slope <- as.data.frame(matrix(0,nrow=nrow(subs_coefs),ncol=ncol(coefs_signs))); # same room
  for (c in 1:ncol(coefs_signs)){
    for (r in 1:(nrow(coefs_signs)/2)){
      intercept[,c] <- intercept[,c] + (coefs_signs[r,c]*subs_coefs[ ,coefs_names[r]])
      slope[,c] <- slope[,c] + coefs_signs[(r+(nrow(coefs_signs)/2)),c] * 
        subs_coefs[ ,coefs_names[r+(nrow(coefs_signs)/2)]]
    }
    subs_coefs[,sprintf("Intercept_%s",colnames(coefs_signs)[c])] <- intercept[,c]
    subs_coefs[,sprintf("Slope_%s",colnames(coefs_signs)[c])] <- slope[,c]
  }
  
  coefs_list <- list(coefs_names, coefs_signs, summary_group_coefs, posterior_group_coefs, subs_coefs)
  names(coefs_list) <- c("coefs_names", "coefs_signs", "summary_group_coefs", "posterior_group_coefs", "subs_coefs")
  
  # save
  assign(sprintf("coef_list_%s",Exp),coefs_list)
  save(list = sprintf("coef_list_%s",Exp),
       file = sprintf("../data/Models/Choice_Ratings_models/Coef_lists/coef_list_%s.RData",Exp))
  
  return(coefs_list)
}

# -------------------------------------------------------------------
# Normalized RT ~ pair type * choice type - Final Decisions phase
# -------------------------------------------------------------------

run_rt_choice_pair_model <- function(data,dependent_measure,third_predictor,params,Exp){
  
  # make adjusments to data
  data <- data %>%
    subset(!is.na(left_chosen)) %>%
    mutate(pair_type_centered = chosen_trial_centered, 
           choice_type_centered = ifelse(higher_outcome_chosen==1, 1, -1)) %>%
    group_by(PID) %>%
    mutate(zscored_rt = zscore(rt, na.rm=1))
  
  # build the formula
  if (length(third_predictor)==0){
    third_predictor <- ""
  } else {
    third_predictor <- sprintf("*%s",third_predictor)
  }
  formula <- sprintf("%s ~ pair_type_centered*choice_type_centered%s +
                     (pair_type_centered*choice_type_centered%s | PID)",
                     dependent_measure,third_predictor,third_predictor)
  # run the model
  M <- stan_glmer(data = data, 
                  eval(expr(formula)),
                  family = gaussian(), 
                  adapt_delta = params$adapt_delta, 
                  iter = params$iterations, 
                  chains = params$chains, 
                  warmup = params$warmup)
  # save it
  assign(sprintf("M_%s_rt_choice_pair",Exp),M)
  save(list = sprintf("M_%s_rt_choice_pair",Exp),
       file = sprintf("../data/Models/RT_choice_pair/M_%s_rt_choice_pair.RData",Exp))
} # end function


# -------------------
# bias ~ memory model
# -------------------

# This function runs a Bayesian linear regression model that predicts inverse bias as a function of memory accuracy of the
# deliberation pairs. For Experiments 2-4 we also include a third predictor denoting the condition type that interacts with
# all other predictors. The function also computes a summary matrix of the model as well as a predicting y observations as a 
# function of the model coefficients for plotting purposes.

run_bias_memory_model <- function(data,memory_predictor,third_predictor,params,Exp){
  
  # =========
  # run model
  # =========
  
  # create the data frame
  bias_memory <- data %>% 
    group_by(.dots=c("PID","chosen_trial",third_predictor)) %>%
    dplyr::summarize(bias = mean(higher_outcome_chosen, na.rm=1),
                     pair_acc = mean(pair_acc, na.rm=1),
                     object_acc = mean(object_acc, na.rm=1),
                     choice_acc = mean(choice_acc, na.rm=1)) %>%
    spread(chosen_trial, bias) %>%
    mutate(bias_diff = `1` - `0`)
  
  # create formula to run the model
  if (length(third_predictor)==0){
    third_predictor_formula <- ""
  } else {
    third_predictor_formula <- sprintf("*%s",third_predictor)
  }
  formula <- sprintf("bias_diff ~ %s%s",memory_predictor, third_predictor_formula)
  
  # run the model
  M <- stan_glm(data = bias_memory, 
                eval(expr(formula)),
                family = gaussian(), 
                adapt_delta = params$adapt_delta, 
                iter = params$iterations, 
                chains = params$chains, 
                warmup = params$warmup)
  
  # save it
  assign(sprintf("M_%s_memory_bias",Exp),M)
  save(list = sprintf("M_%s_memory_bias",Exp),
       file = sprintf("../data/Models/Memory_Bias/Between_subs/Model_objects/M_%s_memory_bias.RData",Exp))
  
  # =================================
  # draws from posterior distribution
  # =================================
  
  # create posterior draws
  draws <- as.data.frame(M)
  
  # if there is a third predictor we can create two types of mats 
  if (length(third_predictor)!=0){
    cond1 <- data.frame(Intercept = draws[,1] + draws[,3], Slope = draws[,2] + draws[,4])
    cond0 <- data.frame(Intercept = draws[,1] - draws[,3], Slope = draws[,2] - draws[,4])
    draws_list <- list(cond1,cond0)
    names(draws_list) <- c("cond1", "cond0")
  } else {
    draws_list <- list(data.frame(Intercept = draws[,1], Slope = draws[,2]))
    names(draws_list) <- "no_cond"
  }
  
  # =================================
  # summary estimates of coefficients
  # =================================
  
  summary_group_coefs <- data.frame(
    cbind(colnames(draws),
          posterior_interval(as.matrix(draws), prob=0.95),
          sapply(draws,FUN=median),
          sapply(draws,FUN=mean))) 
  colnames(summary_group_coefs) <- c("coef","low95HDI","high95HDI","Median", "Mean")
  rownames(summary_group_coefs) <- 1:nrow(summary_group_coefs)
  
  # make sure the values are numeric
  if (class(summary_group_coefs$low95HDI)=="factor"){
    summary_group_coefs <- summary_group_coefs %>% 
      mutate(Median = as.numeric(levels(Median))[Median],
             Mean = as.numeric(levels(Mean))[Mean],
             low95HDI = as.numeric(levels(low95HDI))[low95HDI],
             high95HDI = as.numeric(levels(high95HDI))[high95HDI])
  }
  
  # =============================
  # predict y values for plotting
  # =============================
  
  # predict bias for plotting
  n <- 100 # number x values
  # create new data
  if (length(third_predictor)!=0){ # we have two conditions
    conds <- c(1,-1); new_data_list <- list()
    for (i in 1:length(conds)){
      memory_data_cond <- bias_memory[bias_memory[,third_predictor]==conds[i],memory_predictor]
      x_steps <- seq(min(memory_data_cond,na.rm=1), max(memory_data_cond,na.rm=1), length.out = n)
      new_data_list[[i]] <- data.frame(obs = seq_along(x_steps), x1=conds[i], x2=x_steps)
      colnames(new_data_list[[i]])[c(2,3)] <- c(third_predictor, memory_predictor)
      names(new_data_list)[i] <- sprintf("cond%d",conds[i])
    }
  } else {
    memory_data <- bias_memory[,memory_predictor]
    x_steps <- seq(min(memory_data,na.rm=1), max(memory_data,na.rm=1), length.out = n)
    new_data_list <- list(data.frame(obs = seq_along(x_steps), x1=x_steps))
    colnames(new_data_list[[1]])[c(2)] <- c(memory_predictor)
    names(new_data_list) <- "no_cond"
  }
  # predict y values given the new data
  pred_list <- list()
  for (i in 1:length(new_data_list)){
    pred_list[[i]] <- posterior_linpred(M,newdata = new_data_list[[i]])
    names(pred_list)[i] <- names(new_data_list)[i]
  }
  # compute summary stats for each x observation
  predicted_summary_list <- list()
  for (c in 1:length(new_data_list)){
    df_pred <- pred_list[[c]] %>% 
      as.data.frame %>%
      setNames(seq_len(ncol(.))) %>% 
      tibble::rownames_to_column("posterior_sample") %>% 
      tidyr::gather_("obs", "fitted", setdiff(names(.), "posterior_sample")) %>%
      group_by(obs) %>% 
      dplyr::summarise(median = median(fitted),
                       lower = quantile(fitted, 0.025), 
                       upper = quantile(fitted, 0.975)) %>%
      mutate(obs = as.numeric(obs)) %>%
      left_join(new_data_list[[c]], by="obs") %>% arrange(obs)
    predicted_summary_list[[c]] <- df_pred; names(predicted_summary_list)[c] <- names(new_data_list)[c]
  }
  
  # ===========
  # save object
  # ===========
  
  model_list <- list(draws, draws_list, predicted_summary_list)
  names(model_list) <- c("posterior_draws", "posterior_draws_per_cond", "predicted_summary_list")
  
  # save coef_list
  assign(sprintf("coefs_%s_bias_%s",memory_predictor,Exp),model_list)
  save(list = sprintf("coefs_%s_bias_%s",memory_predictor,Exp),
       file = sprintf("../data/Models/Memory_Bias/Between_subs/Coef_lists/coefs_%s_bias_%s.RData",memory_predictor,Exp))
  
  return(model_list)
}


# -----------------------------------------
# preference change ~ reward * choice model 
# -----------------------------------------

# This function runs a Bayesian linear regression predicting preference change (normalized difference between pre- and 
# post-task ratings, diff_norm_rating) as a function of choice type (chosen or unchosen item, chosen_obj_centered) and 
# actual outcome (rewarded or unrewarded, reward_type). For Experiments 2-4 we also include a third predictor denoting
# the condition type that interacts with all other predictors. 

run_preference_change_model <- function(data,third_predictor,params,Exp){
  # build the formula
  if (length(third_predictor)==0){
    third_predictor <- ""
  } else {
    third_predictor <- sprintf("*%s",third_predictor)
  }
  formula <- sprintf("diff_norm_rating ~ reward_type*chosen_obj_centered%s + (reward_type*chosen_obj_centered%s | PID)",third_predictor,third_predictor)
  # run the model
  M <- stan_glmer(data = data, 
                  eval(expr(formula)),
                  family = gaussian(), 
                  adapt_delta = params$adapt_delta, 
                  iter = params$iterations, 
                  chains = params$chains, 
                  warmup = params$warmup)
  # save it
  assign(sprintf("M_%s_preference_change",Exp),M)
  save(list = sprintf("M_%s_preference_change",Exp),
       file = sprintf("../data/Models/Supplementary_analyses/Preference_change/M_%s_preference_change.RData",Exp))
  return(M)
}


# ------------------------------------------
# choice ~ ratings model: Deliberation phase
# ------------------------------------------

# This function runs a Bayesian logistic regression predicting the tendency to select the left item on the screen as a function
# of the normalized ratings difference between the left and right items. For Experiments 2-4 we also include a third predictor
# denoting the condition type that interacts with all other predictors. 

run_deliberation_choice_model <- function(data,third_predictor,params,Exp){
  # build the formula
  if (length(third_predictor)==0){
    third_predictor <- ""
  } else {
    third_predictor <- sprintf("*%s",third_predictor)
  }
  formula <- sprintf("left_chosen ~ norm_delta_rate%s + (norm_delta_rate%s | PID)",third_predictor,third_predictor)
  # run the model
  M <- stan_glmer(data = data, 
                  eval(expr(formula)),
                  family = binomial(link="logit"), 
                  adapt_delta = params$adapt_delta, 
                  iter = params$iterations, 
                  chains = params$chains, 
                  warmup = params$warmup)
  # save it
  assign(sprintf("M_deliberation_%s",Exp),M)
  save(list = sprintf("M_deliberation_%s",Exp),
       file = sprintf("../data/Models/Supplementary_analyses/Deliberation/M_deliberation_%s.RData",Exp))
  return(M)
}
