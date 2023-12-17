###
# Script that analyse ADH data
###

# Load libraries and R files ----------------------------------------------

source("Aux_functions/Estimation_Functions.R")
source("Data_analysis/ADH/ADH_Init.R")

library(ggplot2)
library(data.table)
library(kableExtra)
library(foreach)
library(doParallel)
library(ivreg)
library(sandwich)
library(stargazer)


# Register parallel engine ------------------------------------------------
# 
# cores = detectCores()/2
# cl = makeCluster(cores)
# registerDoParallel(cl)
# print(paste("is par registered? ",getDoParRegistered()))
# print(paste("numer of cores,",getDoParWorkers()))
# 

# Read data and define variables ------------------------------------------------------------

china_1990 <- as.data.frame(china_1990)
n_obs <- nrow(china_1990)

variables_to_remove= c("timepwt48", "instrument2000", "outcome2000","statefip")

IV <- china_1990$instrument2000

control2 <- china_1990[,col_2_controls[2:length(col_2_controls)]]
control3 <- china_1990[,col_3_controls[2:length(col_3_controls)]]
control4 <- china_1990[,col_4_controls[2:length(col_4_controls)]]
control5 <- china_1990[,col_5_controls[2:length(col_5_controls)]]
control6 <- china_1990[,col_6_controls[2:length(col_6_controls)]]


# NC is all variables outside outcome, instrument, exposures and controls
NCs <- select(china_1990,-one_of(c(variables_to_remove,all_controls,all_controls_old)))

NCs.lagged.outcomes <- data.frame(nc=NCs[,c("outcome1970")])

china_weights <- china_1990$timepwt48


cluster.robust <- china_1990$statefip


# Run regressions ---------------------------------------------------------

# Control 2

set.seed(2)
cntrl2_run <- single_china_run(Z = IV,
                              NC = NCs,
                              cntrls = as.data.frame(control2),
                              z_resid = "no",
                              nc_resid = "no",
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

cntrl2_run.NC.lagged.outcomes <- single_china_run(Z = IV,
                                                  NC = NCs.lagged.outcomes,
                                                  cntrls = as.data.frame(control2),
                                                  z_resid = "no",
                                                  nc_resid = "no",
                                                  c_weight = china_weights,
                                                  cluster.robust = cluster.robust)

# Control 3

set.seed(3)
cntrl3_run <- single_china_run(Z = IV,
                              NC = NCs,
                              cntrls = as.data.frame(control3),
                              z_resid = "no",
                              nc_resid = "no",
                              # c_weight = NULL)
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

cntrl3_run.NC.lagged.outcomes <- single_china_run(Z = IV,
                                                  NC = NCs.lagged.outcomes,
                                                  cntrls = as.data.frame(control3),
                                                  z_resid = "no",
                                                  nc_resid = "no",
                                                  # c_weight = NULL)
                                                  c_weight = china_weights,
                                                  cluster.robust = cluster.robust)

# Control 4

set.seed(4)
cntrl4_run <- single_china_run(Z = IV,
                              NC = NCs,
                              cntrls = as.data.frame(control4),
                              z_resid = "no",
                              nc_resid = "no",
                              # c_weight = NULL)
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

cntrl4_run.NC.lagged.outcomes <- single_china_run(Z = IV,
                              NC = NCs.lagged.outcomes,
                              cntrls = as.data.frame(control4),
                              z_resid = "no",
                              nc_resid = "no",
                              # c_weight = NULL)
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

# Control 5

set.seed(5)
cntrl5_run <- single_china_run(Z = IV,
                              NC = NCs,
                              cntrls = as.data.frame(control5),
                              z_resid = "no",
                              nc_resid = "no",
                              # c_weight = NULL)
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

cntrl5_run.NC.lagged.outcomes <- single_china_run(Z = IV,
                              NC = NCs.lagged.outcomes,
                              cntrls = as.data.frame(control5),
                              z_resid = "no",
                              nc_resid = "no",
                              # c_weight = NULL)
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

# Control 6

set.seed(6)
cntrl6_run <- single_china_run(Z = IV,
                              NC = NCs,
                              cntrls = as.data.frame(control6),
                              z_resid = "no",
                              nc_resid = "no",
                              # c_weight = NULL,
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

cntrl6_run.NC.lagged.outcomes <- single_china_run(Z = IV,
                              NC = NCs.lagged.outcomes,
                              cntrls = as.data.frame(control6),
                              z_resid = "no",
                              nc_resid = "no",
                              # c_weight = NULL)
                              c_weight = china_weights,
                              cluster.robust = cluster.robust)

# Combine results


combined_f_test <- c(cntrl2_run$f_pval,
                        cntrl3_run$f_pval,
                        cntrl4_run$f_pval,
                        cntrl5_run$f_pval,
                        cntrl6_run$f_pval)

combined_bonf <- c(cntrl2_run$bonf_pval,
                        cntrl3_run$bonf_pval,
                        cntrl4_run$bonf_pval,
                        cntrl5_run$bonf_pval,
                        cntrl6_run$bonf_pval)

combined_wald <- c(cntrl2_run$wald_pval,
                        cntrl3_run$wald_pval,
                        cntrl4_run$wald_pval,
                        cntrl5_run$wald_pval,
                        cntrl6_run$wald_pval)

combined_gam <- c(cntrl2_run$gam_pval,
                        cntrl3_run$gam_pval,
                        cntrl4_run$gam_pval,
                        cntrl5_run$gam_pval,
                        cntrl6_run$gam_pval)

combined_gam_smooth <- c(cntrl2_run$gam_pval_smooth_c,
                        cntrl3_run$gam_pval_smooth_c,
                        cntrl4_run$gam_pval_smooth_c,
                        cntrl5_run$gam_pval_smooth_c,
                        cntrl6_run$gam_pval_smooth_c)



combined_f_test.lagged.outcomes <- c(cntrl2_run.NC.lagged.outcomes$f_pval,
                                      cntrl3_run.NC.lagged.outcomes$f_pval,
                                      cntrl4_run.NC.lagged.outcomes$f_pval,
                                      cntrl5_run.NC.lagged.outcomes$f_pval,
                                      cntrl6_run.NC.lagged.outcomes$f_pval)

combined_bonf.lagged.outcomes <- c(cntrl2_run.NC.lagged.outcomes$bonf_pval,
                                    cntrl3_run.NC.lagged.outcomes$bonf_pval,
                                    cntrl4_run.NC.lagged.outcomes$bonf_pval,
                                    cntrl5_run.NC.lagged.outcomes$bonf_pval,
                                    cntrl6_run.NC.lagged.outcomes$bonf_pval)

combined_gam.lagged.outcomes <- c(cntrl2_run.NC.lagged.outcomes$gam_pval,
                                    cntrl3_run.NC.lagged.outcomes$gam_pval,
                                    cntrl4_run.NC.lagged.outcomes$gam_pval,
                                    cntrl5_run.NC.lagged.outcomes$gam_pval,
                                    cntrl6_run.NC.lagged.outcomes$gam_pval)

combined_gam.lagged.outcomes.sm <- c(cntrl2_run.NC.lagged.outcomes$gam_pval_smooth_c,
                                    cntrl3_run.NC.lagged.outcomes$gam_pval_smooth_c,
                                    cntrl4_run.NC.lagged.outcomes$gam_pval_smooth_c,
                                    cntrl5_run.NC.lagged.outcomes$gam_pval_smooth_c,
                                    cntrl6_run.NC.lagged.outcomes$gam_pval_smooth_c)


# Convert results to tables -----------------------------------------------

# Main analysis

# Change to strings


f_tbl <- ifelse(combined_f_test <0.01, "<0.01",as.character(round(combined_f_test,3)))
bonf_tbl <- ifelse(combined_bonf<0.01, "<0.01",as.character(round(combined_bonf,3)))
gam_tbl <- ifelse(combined_gam<0.01, "<0.01",as.character(round(combined_gam,3)))
gam_smooth_tbl <- ifelse(combined_gam_smooth<0.01, "<0.01",as.character(round(combined_gam_smooth,3)))

method_names <- c(rep(c("F-test","Bonf.","GAM", "GAM (Smooth C)"),times = 1))

results_ <- as.data.frame(cbind(method_names,
                             rbind(f_tbl,
                                   bonf_tbl,
                                   gam_tbl, 
                                   gam_smooth_tbl)))

colnames(results_) <- c("Method",sprintf("Control%s",seq(2,6)))

kable(results_,
      row.names = FALSE, 
      format= "rst",
      booktabs = TRUE)


# Lagged outcomes only analysis

# Change to strings


f_tbl.lo <- ifelse(combined_f_test.lagged.outcomes <0.01, "<0.01",
                   as.character(round(combined_f_test.lagged.outcomes,3)))
bonf_tbl.lo <- ifelse(combined_bonf.lagged.outcomes<0.01, "<0.01",
                      as.character(round(combined_bonf.lagged.outcomes,3)))

gam_tbl.lo <- ifelse(combined_gam.lagged.outcomes<0.01, "<0.01",
                      as.character(round(combined_gam.lagged.outcomes,3)))

method_names <- c(rep(c("F-test","Bonf."),times = 1))

results_lo <- as.data.frame(cbind(method_names,
                             rbind(f_tbl.lo,
                                   bonf_tbl.lo)))

colnames(results_lo) <- c("Method",sprintf("Control%s",seq(2,6)))

kable(results_lo,
      row.names = FALSE, 
      format= "latex",
      booktabs = TRUE)

# Corr-corr plot ----------------------------------------------------------

# Get residualized IV
iv_resid_china <- get_resids_data(Z = IV,
                                  NC = NCs,
                                   cntrls = control6, 
                                   nc_resid = "lm",
                                   z_resid = "lm",
                                  wght = china_weights)
# Get residualized outcome
outcome_resid_china <- get_resids_data(Z = china_1990$outcome2000,
                                    NC = NCs,
                                   cntrls = control6, 
                                   nc_resid = "lm",
                                   z_resid = "lm",
                                   wght = china_weights)


# Compute correlations
cor_iv_nc <- cor(iv_resid_china$Z, iv_resid_china$NC)
cor_outcome_nc <- cor(outcome_resid_china$Z, iv_resid_china$NC)

data_for_corplot = data.frame(cor_iv_nc = c(cor_iv_nc),
                cor_outcome_nc = c(cor_outcome_nc),
                nc = colnames(cor_iv_nc))

data_for_corplot_abs <- data_for_corplot
data_for_corplot_abs$cor_iv_nc <- abs(data_for_corplot$cor_iv_nc)
data_for_corplot_abs$cor_outcome_nc <- abs(data_for_corplot$cor_outcome_nc)

corr_plot <- ggplot(data = data_for_corplot_abs,
                    aes(x=cor_iv_nc, y= cor_outcome_nc,
                        label = nc)) + 
                    geom_point(alpha = 0.7) +
                    geom_text(hjust = 0, nudge_x = 0.002, angle = 20, check_overlap = F) +
                    # scale_x_continuous(limits = c(range(cor_outcome_nc)[1]-0.1,range(cor_outcome_nc)[2]+0.1)) +
                    # scale_y_continuous(limits = c(range(cor_iv_nc)[1]-0.05,range(cor_iv_nc)[2]+0.05)) +
                    xlab("Outcome ~ NC Correlation") + 
                    ylab("IV ~ NC Correlation") + 
                     ggtitle("Cor-cor of Outcome (year 2000) and IV",
                             "Resdiualized by controls (spec. 6) using linear regression. Correlation in absolute values.") +
                    theme_minimal()

# Table 3 replications -----------------------------------------------------------

table3_rep <- replicate_table_3(workfile_china,
                                col_2_controls_old,
                                col_3_controls_old,
                                col_4_controls_old,
                                col_5_controls_old,
                                col_6_controls_old,
                                G,
                                N)

stargazer(list(table3_rep$col2_reg, table3_rep$col3_reg, table3_rep$col4_reg,
                       table3_rep$col5_reg, table3_rep$col6_reg),
            se = list(table3_rep$col2_se, table3_rep$col3_se, table3_rep$col4_se,
                      table3_rep$col5_se, table3_rep$col6_se),
            title = "Autor et al. Table 3 replication",
            type = "latex",
            digits = 3,
            dep.var.labels=sprintf("Col %s",seq(2,6)),
           omit = c("t2",col_3_controls_old[3:10],"(Intercept)","Constant"),
          no.space = TRUE
          )




# Save NC names -----------------------------------------------------------

library(stringr)

nc_names_dt <- data.table(NC = names(NCs))

nc_names_dt[,type := ifelse(str_detect(pattern = "l_",string = NC),
                            "Year 1990 variable","Past Outcome")]

str_to_categ <- function(strg){
  if(str_detect(strg, "_empl_mfg")){return(c("manufacturing employment (%)"))}
  if(str_detect(strg, "_empl_nmfg")){return(c("non-manufacturing employment (%)"))}
  if(str_detect(strg, "_empl")){return(c("employment (%)"))}
  if(str_detect(strg, "_unempl")){return(c("Un-employment (%)"))}
  if(str_detect(strg, "_nilf")){return(c("Not-in-Labor-Force (NILF) (%)"))}
  if(str_detect(strg, "_trans_")){return(c("Transfer receipts per capita"))}
  if(str_detect(strg, "_avg_lnwkwage")){return(c("average log weekly wage"))}
  if(str_detect(strg, "_avg_hhin")){return(c("average annual household income per working-age adult"))}
  if(str_detect(strg, "ssadiswkrs")){return(c("Social Security Disability Insurance (SSDI) receipt (%)"))}
  else{return("Else")}
}

nc_names_dt[,category := sapply(nc_names_dt$NC,function(x){str_to_categ(x)})]



