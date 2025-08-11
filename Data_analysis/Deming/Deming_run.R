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
library(stringr)
library(readr)
library(ivreg)


#  Deming analysis --------------------------------------------------------------

# Get data ready
school_data = deming_data(curr_cms,c("lottery", "lott_VA","new_lott_VA"))

which.is.enrolled <- which(names(school_data) == "enrolled")


# With VA

set.seed(156)
school_analysis_with_VA <- run_school_nciv(cms_data = school_data[,-which.is.enrolled],
                                               ivs= c("lottery", "lott_VA","new_lott_VA"),
                                               # ivs= c("lottery", "lott_VA"),
                                               with_VA = TRUE)

deming_results_with_VA <- as.data.frame(t(as.matrix(school_analysis_with_VA$results,nrow=2,ncol =4)))

names(deming_results_with_VA) <-  c("lottery", "lott_VA")

deming_results_with_VA$method <- c("F","bonf","GAM","GAM (smooth cntrl)")

print("Deming analysis results")
kbl(deming_results_with_VA, 
    row.names = FALSE,
    format = "rst")


# Naive psuedo-outcome NCO (lagged test scores)

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
NC <- dplyr::select(data_reduced,-one_of(c(variables_to_remove,relevant_cols)))


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


# Save NC names

nc_names_dt <- data.table(variable_name = names(NC))

nc_names_dt[,variable_categ := ifelse(str_detect(pattern = "math_",string = variable_name),
                            "Math",
                            ifelse(str_detect(pattern = "read_",string = variable_name),
                                   "Read",
                                   "Past Outcome"))]

nc_names_dt[,variable_year := as.character(parse_number(variable_name))]
nc_names_dt[34:37, variable_year := c("1st Choice", 
                                       "2nd Choice",
                                       "3rd Choice",
                                       "Neighborhood")]

nc_names_dt[,variable_transformation := ifelse(str_detect(string = variable_name, pattern = "_imp_sq"),
                                     "Squared",
                                     ifelse(str_detect(string = variable_name, pattern = "_imp_cub"),
                                            "Cubic",
                                            ifelse(str_detect(string = variable_name, pattern = "_miss"),
                                                   "Missing indicator", 
                                                   "None")))]

# Add outcome~NCO corr
nc_names_dt[,outcome_nc_corr := corr_nc_outcome]

# Add lottery~NCO corr
resid_data_lottery <- get_resids_data(Z=unlist(as.data.frame(IVs[,1])),
                              NC = as.data.frame(NC),
                              cntrls = as.data.frame(controls),
                              nc_resid = "no",
                              z_resid = "fe",
                              fixed_e_var = data_reduced[,"lottery_FE"])$Z
corr_nc_iv_lottery <- abs(cor(resid_nc, resid_data_lottery))
nc_names_dt[,lottery_nc_corr := corr_nc_iv_lottery]

# Add lott_VA~NCO corr 
resid_data_lott_VA <- get_resids_data(Z=unlist(as.data.frame(IVs[,2])),
                              NC = as.data.frame(NC),
                              cntrls = as.data.frame(controls),
                              nc_resid = "no",
                              z_resid = "fe",
                              fixed_e_var = data_reduced[,"lottery_FE"])$Z
corr_nc_iv_lott_VA <- abs(cor(resid_nc, resid_data_lott_VA))
nc_names_dt[,lott_va_nc_corr := corr_nc_iv_lott_VA]

write.csv(x = nc_names_dt,
          file = "Data_analysis/Deming/deming_cor_plot_data_relabeled.csv",
          row.names = F)


deming_cor_df <- read.csv("Data_analysis/Deming/deming_cor_plot_data_relabeled.csv")


# GGplot of corr-corr plot
theme_set(theme_bw(base_size = 21))


deming_cor_df <- deming_cor_df %>% 
  mutate(variable_label = ifelse(variable_categ == "VA", 
                                 variable_year,
                                 paste(variable_categ, variable_year)
  ),
  variable_year = ifelse(!variable_year %in% c("1998","1999","2000","2001","2002"), 
                         "Choice School VA",
                         paste(variable_year, "Test Scores"))
  )

lott_va_corr_plot <- deming_cor_df %>%
  mutate(variable_label = ifelse(variable_label == "Neighborhood ", "Neighborhood School", "")) %>%
  ggplot(
    aes(x=outcome_nc_corr,
        y=lott_va_nc_corr,
        label = variable_label)) + 
  geom_point(aes(shape = factor(variable_year)), size = 5, alpha = 0.9) +
  geom_text(size = 5, hjust = 0, nudge_x = 0.005, angle = 0, check_overlap = F, show.legend = F) +
  scale_x_continuous(expand = expansion(add = c(0.01, 0.03))) +
  scale_y_continuous(expand = expansion(add = c(0.01,0.03)),
                     limits = c(0, .6)) +
  scale_shape_manual(values = 1:9) + theme(panel.grid = element_blank()) +
  annotate("text", x = 0.042, y = 0.545, 
         label = "Neighborhood School",
         size = 5) +
  labs(
    x = "Absolute Correlation between NCO and Outcome",
    y = "Absolute Correlation between NCO and IV",
    shape = "",
  ) + 
  guides(color ="none")
lott_va_corr_plot

ggsave("Data_analysis/Deming/corplot_lott_va.pdf",
       lott_va_corr_plot,
       width = 11,
       height = 8)
