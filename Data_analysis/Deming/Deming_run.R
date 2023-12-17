###
# Script that analyse Deming's school data
###

# Libraries and sources ---------------------------------------------------

source("Data_analysis/Deming/Deming_init.R")

library(kableExtra)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(data.table)

#  Deming analysis --------------------------------------------------------------

# Get data ready
school_data = deming_data(curr_cms,c("lottery", "lott_VA","new_lott_VA"))

# With VA

set.seed(156)
school_analysis_with_VA <- run_school_nciv(cms_data = school_data,
                                               ivs= c("lottery", "lott_VA","new_lott_VA"),
                                               # ivs= c("lottery", "lott_VA"),
                                               with_VA = TRUE)

deming_results_with_VA <- as.data.frame(t(as.matrix(school_analysis_with_VA$results,nrow=2,ncol =4)))

names(deming_results_with_VA) <-  c("lottery", "lott_VA")

deming_results_with_VA$method <- c("F","bonf","GAM","GAM (smooth cntrl)")


kbl(deming_results_with_VA, 
    row.names = FALSE,
    format = "simple")


# Naive psuedo-outcome NCO

NCO <- school_data[,"testz2002"]
iv.lot <- school_data[,"lottery"]
iv.lot.VA <- school_data[,"lott_VA"]
relevant_cols <- c("math_2002_imp", "read_2002_imp", "math_2002_imp_sq", "math_2002_imp_cub",
                             "read_2002_imp_sq", "read_2002_imp_cub", "math_2002_miss", "read_2002_miss")
covars <- school_data[,relevant_cols]

formula.lott <- paste0("testz2002 ~ lottery + ",paste(relevant_cols,collapse = " + "))
lm.psuedo.outcome.iv.lot <- lm(formula.lott, school_data)
summary(lm.psuedo.outcome.iv.lot)

formula.lot.VA <- paste0("testz2002 ~ lott_VA + ",paste(relevant_cols,collapse = " + "))
lm.psuedo.outcome.iv.lot.VA <- lm(formula.lot.VA, school_data)
summary(lm.psuedo.outcome.iv.lot.VA)


# Save NC names -----------------------------------------------------------

library(stringr)
library(readr)

nc_names_dt <- data.table(NC = names(NC))

nc_names_dt[,type := ifelse(str_detect(pattern = "math_",string = NC),
                            "Math",
                            ifelse(str_detect(pattern = "read_",string = NC),
                                   "Read",
                                   "Past Outcome"))]

nc_names_dt[,year := parse_number(NC)]

nc_names_dt[,tranformation := ifelse(str_detect(string = NC, pattern = "_imp_sq"),
                                     "Squared",
                                     ifelse(str_detect(string = NC, pattern = "_imp_cub"),
                                            "Cubic",
                                            ifelse(str_detect(string = NC, pattern = "_miss"),
                                                   "Missing indicator", 
                                                   "None")))]

write.csv(x = nc_names_dt,
          file = "Data_analysis/Deming/deming_nc_description.csv",
          row.names = F)



# Cor-cor plots --------------------------------------------------------------


# Prepare data set as in analysis runs

model <-  c("2") 
prev_yr <- c("all") 
relevant_cols <- c("math_2002_imp", "read_2002_imp", "math_2002_imp_sq", "math_2002_imp_cub",
                   "read_2002_imp_sq", "read_2002_imp_cub", "math_2002_miss", "read_2002_miss")
relevant_cols_for_analysis <- c(relevant_cols,"testz2002")
ivs <- c("lottery", "lott_VA","new_lott_VA")

data_reduced <- setDT(prepare_school_data(curr_cms, model, relevant_cols_for_analysis, 
                                                  prev_yr, ivs))

# Get IVs, NC, and controls 

choice_var <- c(sprintf("ch1_mod%smix_%s_test", model, prev_yr),
                sprintf("ch2_mod%smix_%s_test", model, prev_yr),
                sprintf("ch3_mod%smix_%s_test", model, prev_yr),
                sprintf("hm_mod%smix_%s_test", model, prev_yr))

# With VA
# variables_to_remove= c("testz2003", "VA","lottery_FE",ivs,choice_var)
variables_to_remove= c("testz2003", "VA","lottery_FE",ivs)

IVs <- data_reduced[,.SD,.SDcols = ivs]
controls <- data_reduced[,.SD,.SDcols = relevant_cols]
NC <- select(data_reduced,-one_of(c(variables_to_remove,relevant_cols)))


# Get residualized outcome on controls (using lottery_FE)

resid_outcome_nc <- get_resids_data(Z=unlist(as.data.frame(data_reduced[,testz2003])),
                                     NC = as.data.frame(NC),
                                     cntrls = as.data.frame(controls),
                                     nc_resid = "fe",
                                     z_resid = "fe",
                                     fixed_e_var = data_reduced[,"lottery_FE"])

resid_outcome <- resid_outcome_nc$Z

resid_nc <- resid_outcome_nc$NC

# corr_nc_outcome <- cor(resid_nc, resid_outcome)

corr_nc_outcome <- abs(cor(resid_nc, resid_outcome))


deming_nc_description <- read.csv("Data_analysis/Deming/deming_nc_description.csv")

for (i in seq(2)){
  
  resid_data <- get_resids_data(Z=unlist(as.data.frame(IVs[,..i])),
                                NC = as.data.frame(NC),
                                cntrls = as.data.frame(controls),
                                nc_resid = "no",
                                z_resid = "fe",
                                fixed_e_var = data_reduced[,"lottery_FE"])$Z
  
  # corr_nc_iv <- cor(resid_nc, resid_data)
  corr_nc_iv <- abs(cor(resid_nc, resid_data))
  
  cur_df <- data.frame(outcome_nc_cor = c(corr_nc_outcome),
                       iv_nc_cor = c(corr_nc_iv),
                       variable_name = rownames(corr_nc_iv),
                       variable_categ = deming_nc_description$type,
                       variable_year = deming_nc_description$year,
                       variable_transformation = deming_nc_description$tranformation)
  
  corr_plot <- ggplot(data = cur_df,
                     aes(x=outcome_nc_cor,
                         y=iv_nc_cor,
                         label = variable_categ,
                         color = variable_categ)) + 
                    geom_point(aes(shape = factor(variable_year)), size = 4, alpha = 0.7) +
                    geom_text(size =4, hjust = 0, nudge_x = 0.002, angle = 20, check_overlap = F, show.legend = F) +
                    scale_x_continuous(expand = expansion(add = c(0.01, 0.03))) +
                    scale_y_continuous(expand = expansion(add = c(0.01,0.03))) +
                    scale_shape_manual(values = 1:9) +
                    labs(
                      x = "Outcome ~ NC Absolute Correlation",
                      y = "IV ~ NC Absolute Correlation", 
                      shape = "",
                      title = paste0("Cor-cor plot for IV=",ivs[i]),
                      subtitle = "Resdiualized by controls using lottery_FE as fixed-effect") + 
                    guides(color ="none") +
                    theme_bw(base_size = 22)
  
  ggsave(paste0("Data_analysis/Deming/corplot_",ivs[i],".jpeg"),
         corr_plot,
         device = "jpeg",
         width = 11,
         height = 8)
}


