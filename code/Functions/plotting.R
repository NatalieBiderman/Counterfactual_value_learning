# Plotting functions

# ====== general plot theme ======

theme <- theme(plot.title = element_text(hjust = 0.5, size = 28), 
               text = element_text(size=20,family="Helvetica"),
               axis.title = element_text(size = 24), 
               axis.text = element_text(size = 21, color = "black"), 
               legend.position = "top",
               legend.title = element_blank(),
               legend.spacing.x = unit(0.2,'cm'),
               axis.line = element_line(size = 1), 
               axis.ticks = element_blank(),  
               aspect.ratio = 2/3)

point_plot_theme <- theme(strip.background=element_blank(), 
                          panel.spacing.x=unit(1,"line"), 
                          axis.line=element_blank(), aspect.ratio=1) + 
  panel_border(colour="black", size=2, linetype=1, remove=FALSE)

# ====== correlation text ======

create_corr_text <- function(data_mat,groupvars,varx,vary,pos_x,pos_y){
  # create corr matrix
  corrs_mat <- data_mat %>% group_by(.dots=c(groupvars)) %>% 
    dplyr::summarize(corr = as.numeric(cor_test(!!sym(vary), !!sym(varx))[[4]]),
                     p = as.numeric(cor_test(!!sym(vary), !!sym(varx))[[3]]),
                     p_text = ifelse(p < 0.001, "***",
                                           ifelse(p < 0.01, "**", 
                                                  ifelse(p < 0.05, "*", "")))) %>%
    dplyr::mutate(label = sprintf("r = %.2f, p = %.3f", corr, p),
                  label2 = sprintf("r = %.2f%s", corr, p_text),
                  x = pos_x, 
                  y= pos_y)
  
  return(corrs_mat)
}