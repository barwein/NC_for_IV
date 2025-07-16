### 
# Script that initatilze Autor et al. (ADH) analysis
# Read data and provide aux function for analysis
###


# Libraries ---------------------------------------------------------------

library(haven)
library(dplyr)
library(caret)
library(lmtest)
library(ivreg)

# Read data and define variables ------------------------------------------


workfile_china_raw <- read_dta("Data_analysis/ADH/workfile_china.dta")

workfile_china_raw_pre <- read_dta("Data_analysis/ADH/workfile_china_preperiod.dta")


# czone= commuting zone, the basic economic unit used in the article
# we give explicit names for regressions.
# instrument = d_tradeotch_pw_lag - delta in trade to other states from China
# exposure = d_tradeusch_pw - delta in trade to US from China
# outcome = d_sh_empl_mfg - delta in employment in manufacturing 

workfile_china <- workfile_china_raw  %>%
  left_join (
    workfile_china_raw %>%
      group_by(czone) %>%
      summarise(instrument1990= sum(d_tradeotch_pw_lag*(yr==1990 + 0)),
                exposure1990= sum(d_tradeusch_pw*(yr==1990 + 0)),
                outcome1990= sum(d_sh_empl_mfg*(yr==1990 + 0)),
                instrument2000= sum(d_tradeotch_pw_lag*(yr==2000 + 0)),
                exposure2000= sum(d_tradeusch_pw*(yr==2000 + 0)),
                outcome2000= sum(d_sh_empl_mfg*(yr==2000 + 0)),
                
               l_shind_manuf_cbp_2000 = sum(l_shind_manuf_cbp*(yr==2000 + 0)),
               
               reg_midatl_2000 = sum(reg_midatl*(yr==2000 + 0)),
               reg_encen_2000 = sum(reg_encen*(yr==2000 + 0)),
               reg_wncen_2000 = sum(reg_wncen*(yr==2000 + 0)),
               reg_satl_2000 = sum(reg_satl*(yr==2000 + 0)),
               reg_escen_2000 = sum(reg_escen*(yr==2000 + 0)),
               reg_wscen_2000 = sum(reg_wscen*(yr==2000 + 0)),
               reg_mount_2000 = sum(reg_mount*(yr==2000 + 0)),
               reg_pacif_2000 = sum(reg_pacif*(yr==2000 + 0)),
               
               l_sh_popedu_c_2000 = sum(l_sh_popedu_c*(yr==2000 + 0)),
               l_sh_popfborn_2000 = sum(l_sh_popfborn*(yr==2000 + 0)),
               l_sh_empl_f_2000 = sum(l_sh_empl_f*(yr==2000 + 0)),
               l_sh_routine33_2000 = sum(l_sh_routine33*(yr==2000 + 0)),
               l_task_outsource_2000 = sum(l_task_outsource*(yr==2000 + 0)),
               
      )
    , by= "czone")



#t2 should be removed because we check NCIV only on 1990 data
# czone, yr, statefip, city are not numrical
# other variables removed because they don't reflect the status in 
# 1990 only (delta, change of any sort)
china_1990 <- workfile_china %>%
  filter(yr == 1990) %>%
  dplyr::select(-c("l_tradeusch_pw", "l_tradeotch_pw",
            "czone", "yr", "t2", "city"),
            # "exposure1990", "instrument1990", "exposure2000",  
            # "czone", "yr", "t2", "statefip", "city"), 
            # "czone", "yr",  "city"),
         -starts_with("d"),
         -starts_with("relchg"),
         -starts_with("lnchg")) #Table 3 (pg. 2137)


# Add 1980,1970 past outcomes

china_1990$outcome1980 <- c(unlist(workfile_china_raw_pre %>% filter(yr==1980) %>% 
                                     dplyr::select("d_sh_empl_mfg")))
china_1990$outcome1970 <- c(unlist(workfile_china_raw_pre %>% filter(yr==1970) %>% 
                                     dplyr::select("d_sh_empl_mfg")))

# assertthat::are_equal(ncol(workfile_china) , 197)
assertthat::are_equal(nrow(workfile_china) , 1444)

# assertthat::are_equal(ncol(china_1990) , 67)
assertthat::are_equal(nrow(china_1990) , 722)


# Replicate Table 3 -------------------------------------------------------


# used for getting heteroscedasticity s.e. for table 3 replication
G <- length(unique(workfile_china$statefip))
N <- length(workfile_china$statefip)

get_formula_for_table_3 <- function(col_controls){
  return(as.formula(paste("d_sh_empl_mfg ~",
                          paste(col_controls, collapse=" + "),
                          "| d_tradeusch_pw | d_tradeotch_pw_lag")))
}

# get heteroscedasticity consistent estimates for linear model
# input: linear regression model, number of groups, observations 
# output: heteroscedasticity consistent estimates
get_stata_coef <- function(G, # number of groups
                           N, # observations
                           reg_model, #lm object
                           hc_type= "HC2" #heteroscedasticity type
){
  dfa <- (G/(G - 1)) * (N - 1)/reg_model$df.residual
  
  firm_c_vcov <- dfa * vcovHC(reg_model, type = hc_type, cluster = "group", adjust = T)
  return(coeftest(reg_model, vcov = firm_c_vcov))
}

# rerunning table 3 w. the appropriate controls
# Based on pg. 2136 and czone_analysis_ipw_final.do
replicate_table_3 <- function(workfile_china, # dataset used to replicate table 3
                              col_2_controls, # groups of controls corresponding to
                              col_3_controls, # the columns in table 3
                              col_4_controls,
                              col_5_controls,
                              col_6_controls,
                              G, N #used for heteroscedasticity stds
) {
  # Partial replication of table 3
  # eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) t2 [aw=timepwt48], cluster(statefip) first
  tbl_3_col_2 <- ivreg(formula= get_formula_for_table_3(col_2_controls)
                       , weights= timepwt48
                       , data= workfile_china)
  robust_se_col2 = sqrt(diag(vcovCL(x=tbl_3_col_2,
                                    vcov. = sandwich::vcovCL,
                                    cluster=workfile_china$statefip,
                                    type="HC1")))
  
  tbl_3_col_3 <- ivreg(formula= get_formula_for_table_3(col_3_controls)
                       , weights= timepwt48
                       , data= workfile_china)
  robust_se_col3 = sqrt(diag(vcovCL(x=tbl_3_col_3,
                                    vcov. = sandwich::vcovCL,
                                    cluster=workfile_china$statefip,
                                    type="HC1")))
  
  tbl_3_col_4 <- ivreg(formula= get_formula_for_table_3(col_4_controls)
                       , weights= timepwt48
                       , data= workfile_china)
  robust_se_col4 = sqrt(diag(vcovCL(x=tbl_3_col_4,
                         vcov. = sandwich::vcovCL,
                         cluster=workfile_china$statefip,
                         type="HC1")))
  
  
  tbl_3_col_5 <- ivreg(formula= get_formula_for_table_3(col_5_controls)
                       , weights= timepwt48
                       , data= workfile_china)
  robust_se_col5 = sqrt(diag(vcovCL(x=tbl_3_col_5,
                                    vcov. = sandwich::vcovCL,
                                    cluster=workfile_china$statefip,
                                    type="HC1")))
  
  tbl_3_col_6 <- ivreg(formula= get_formula_for_table_3(col_6_controls)
                       , weights= timepwt48
                       , data= workfile_china)
  robust_se_col6 = sqrt(diag(vcovCL(x=tbl_3_col_6,
                                    vcov. = sandwich::vcovCL,
                                    cluster=workfile_china$statefip,
                                    type="HC1")))
  
  return(list(col2_reg = tbl_3_col_2,
              col3_reg = tbl_3_col_3,
              col4_reg = tbl_3_col_4,
              col5_reg = tbl_3_col_5,
              col6_reg = tbl_3_col_6,
              col2_se = robust_se_col2,
              col3_se = robust_se_col3,
              col4_se = robust_se_col4,
              col5_se = robust_se_col5,
              col6_se = robust_se_col6))
}

# Define controls variables -----------------------------------------------


# Different groups of controls for replication and NCIV test of the specifications
# of table 3

# eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw=d_tradeotch_pw_lag) l_shind_manuf_cbp t2 [aw=timepwt48], cluster(statefip) first
col_2_controls <- c("t2", "l_shind_manuf_cbp_2000")
census_controls <- c(#c("statefip",#	"city",
  "reg_midatl_2000",	"reg_encen_2000",	"reg_wncen_2000",
  "reg_satl_2000",	"reg_escen_2000",	"reg_wscen_2000",	"reg_mount_2000",
  "reg_pacif_2000")
col_3_controls <- c(col_2_controls, census_controls)
col_4_controls <- c(col_3_controls, "l_sh_popedu_c_2000", "l_sh_popfborn_2000", "l_sh_empl_f_2000")
col_5_controls <-  c(col_3_controls, "l_sh_routine33_2000", "l_task_outsource_2000")
col_6_controls <-  c(col_4_controls, "l_sh_routine33_2000", "l_task_outsource_2000")
all_controls <- col_6_controls[2:length(col_6_controls)]


# OLD 1990 controls

col_2_controls_old <- c("t2", "l_shind_manuf_cbp")
census_controls_old <- c(#c("statefip",#	"city",
  "reg_midatl",	"reg_encen",	"reg_wncen",
  "reg_satl",	"reg_escen",	"reg_wscen",	"reg_mount",
  "reg_pacif")
col_3_controls_old <- c(col_2_controls_old, census_controls_old)
col_4_controls_old <- c(col_3_controls_old, "l_sh_popedu_c", "l_sh_popfborn", "l_sh_empl_f")
col_5_controls_old <-  c(col_3_controls_old, "l_sh_routine33", "l_task_outsource")
col_6_controls_old <-  c(col_4_controls_old, "l_sh_routine33", "l_task_outsource")
all_controls_old <- col_6_controls_old[2:length(col_6_controls_old)]

# Based on pg. 2135 second paragraph and czone_analysis_preperiod_final.do

#eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw_future=d_tradeotch_pw_lag_future) [aw=timepwt48] if yr==1970, cluster(statefip)
#eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw_future=d_tradeotch_pw_lag_future) [aw=timepwt48] if yr==1980, cluster(statefip)
#eststo: ivregress 2sls d_sh_empl_mfg (d_tradeusch_pw_future=d_tradeotch_pw_lag_future) t1980 [aw=timepwt48] if yr>=1970 & yr<1990, cluster(statefip)

# Data set with only 1990 data with only the negative control used in the
# article - the outcome in 2000
china_1990_only_org_nc <- china_1990 %>% 
  dplyr::select(c(col_6_controls[2:length(col_6_controls)], "timepwt48", "instrument2000", "outcome1990"))

assertthat::are_equal(ncol(china_1990_only_org_nc) , 17)
assertthat::are_equal(nrow(china_1990_only_org_nc) , 722)

# Additional  groups of controls
location_controls <- c("factor(statefip)",	"city")
#mfg= manufacring
man_controls <- c("l_shind_manuf_cbp",
                  "l_sh_empl",	"l_sh_empl_mfg",	"l_sh_empl_mfg_m",	"l_sh_empl_mfg_f",
                  "l_sh_empl_mfg_edu_nc",	"l_sh_empl_mfg_edu_c",	"l_sh_empl_mfg_age1634",
                  "l_sh_empl_mfg_age3549", 	"l_sh_empl_mfg_age5064",	"l_sh_empl_nmfg",
                  "l_sh_empl_nmfg_m",	"l_sh_empl_nmfg_f",	"l_sh_empl_nmfg_edu_nc",
                  "l_sh_empl_nmfg_edu_c",	"l_sh_empl_nmfg_age1634",	"l_sh_empl_nmfg_age3549",
                  "l_sh_empl_nmfg_age5064")
#unemployment
uemp_controls <- c("l_sh_unempl",	"l_sh_unempl_m",	"l_sh_unempl_f",	"l_sh_unempl_edu_nc",	
                   "l_sh_unempl_edu_c",	"l_sh_unempl_age1634",	"l_sh_unempl_age3549",	"l_sh_unempl_age5064")
#Not-in-Labor-Force
nilf_controls <- c("l_sh_nilf",	"l_sh_nilf_m",	"l_sh_nilf_f",	"l_sh_nilf_edu_nc",
                   "l_sh_nilf_edu_c",	"l_sh_nilf_age1634",	"l_sh_nilf_age3549",	"l_sh_nilf_age5064")

trans_controls <- c("l_trans_totindiv_pc",	"l_trans_totmed_pc",	"l_trans_fedinc_pc",
                    "l_trans_othinc_pc",	"l_trans_unemp_pc",	"l_trans_taaimp_pc",
                    "l_trans_totedu_pc",	"l_trans_ssaret_pc",	"l_trans_ssadis_pc")

hh_controls <- c("l_avg_hhincsum_pc_pw",	"l_avg_hhincwage_pc_pw") #house hold

wage_controls <- c("l_sh_ssadiswkrs",	"l_avg_lnwkwage_mfg",	"l_avg_lnwkwage_nmfg")

demographics_controls <- c("l_popcount",	"l_no_workers_totcbp",	"l_shind_manuf_cbp",
                           "l_sh_popedu_c",	"l_sh_popfborn",	"l_sh_empl_f")

offshoring_controls <- c("l_sh_routine33",	"l_task_outsource")




# Aux function ------------------------------------------------------------


single_china_run <- function(Z, 
                             NC, 
                             cntrls,
                             z_resid, 
                             nc_resid, 
                             c_weight = NULL,
                             cluster.robust = NULL){
  
  # If residualization should be performed 
  resid_df <- get_resids_data(Z = Z,
                              NC = NC,
                              cntrls = cntrls,
                              nc_resid = nc_resid,
                              z_resid = z_resid,
                              ntree = ntree,
                              wght = c_weight)
  # If there are controls in the model
  cntrl_observed <- ifelse(is.null(resid_df$cntrls), FALSE, TRUE)
  # Estimate RF

  # Run F-test
  f_result <- estimate_lm_f_test(Z = resid_df$Z, 
                                 NC = resid_df$NC,
                                 cntrls = resid_df$cntrls,
                                 cntrl_observed = cntrl_observed,
                                 f_weights = c_weight)
  # Run Bonferroni 
  bonf_result <- estimate_lm_bonf(Z = resid_df$Z, 
                                  NC = resid_df$NC,
                                  cntrls = resid_df$cntrls,
                                  cntrl_observed = cntrl_observed,
                                  weight_vec = c_weight,
                                  cluster.robust = cluster.robust)
  
  wald_results <- wald.robust.function(Z = resid_df$Z, 
                                       NC = resid_df$NC,
                                       cntrls = resid_df$cntrls,
                                       cntrl_observed = cntrl_observed,
                                       w_weights = c_weight,
                                       cluster.robust = cluster.robust)

  gam_results <- gam.chisq.function(Z = resid_df$Z, 
                                    NC = resid_df$NC,
                                    cntrls = resid_df$cntrls,
                                    cntrl_observed = cntrl_observed,
                                    g_weights = c_weight)
  
  # Return results
  return(list(f_pval = f_result,
              bonf_pval = min(bonf_result)*ncol(resid_df$NC),
              wald_pval = wald_results,
              gam_pval = gam_results$pval.wald.no.p,
              gam_pval_smooth_c = gam_results$pval.wald.smooth.cntrl))
  
}


























