### 
# Initialize Deming analysis and some aux functions
###


# Load libraries and sources -----------------------------------------------

library(haven)
library(dplyr)
library(fixest)
library(caret)
library(mltools)
library(stringr)

source("Aux_functions/Estimation_Functions.R")


# Formulas for replication analysis ---------------------------------------

get_formula_for_2sls <- function(outcome, # the name of the outcome variable
                                  col_factors, # the name of the outcome variable
                                  fixed, # the name of the fixed effects groups
                                  endogen, # the name of the endogen treatment variable
                                  iv # the name of the instrumental variable
                                  ){
  return(as.formula(paste(outcome, "~", paste(col_factors, collapse=" + "),
                          "|", fixed,
                          "| ", endogen, "~", paste(c(iv, col_factors), collapse=" + "))))
}

get_formula_for_iv_by_controls <- function(iv, # the name of the instrumental variable
                                           fixed,  # the name of the fixed effects groups
                                           controls # the name of the outcome variable
                                           ){
  return(as.formula(paste(iv, "~ ", paste(controls, collapse=" + "),
                          "|", fixed))
  ) 
}


# Read data ---------------------------------------------------------------

cms <- read_dta("Data_analysis/Deming/cms_added_columns.dta") 
curr_cms <- dplyr::filter(cms, !is.na(cms$lottery_FE))
curr_cms$lottery_FE <- factor(curr_cms$lottery_FE)


# Prepare data ------------------------------------------------------------


deming_data <- function(data,
                        ivs){
  # General definitions
  model <-  c("2") 
  prev_yr <- c("all") 
  
  # the controls as specified in the article
  
  relevant_cols <- c("math_2002_imp", "read_2002_imp", "math_2002_imp_sq", "math_2002_imp_cub",
                     "read_2002_imp_sq", "read_2002_imp_cub", "math_2002_miss", "read_2002_miss",
                     "enrolled")
  
  
  # relevant_cols_for_analysis <- c(relevant_cols,"testz1998","testz1999","testz2000","testz2001","testz2002")
  relevant_cols_for_analysis <- c(relevant_cols,"testz2002")
  
  
  choice <- c(sprintf("ch1_mod%smix_%s_test", model, prev_yr),
              sprintf("ch2_mod%smix_%s_test", model, prev_yr),
              sprintf("ch3_mod%smix_%s_test", model, prev_yr),
              sprintf("hm_mod%smix_%s_test", model, prev_yr))
  
  choice_min <- c(sprintf("ch1_mod%smix_%s_test", model, prev_yr),
                  sprintf("hm_mod%smix_%s_test", model, prev_yr))
  
  data_reduced <- as.data.frame(prepare_school_data(data, model, relevant_cols_for_analysis, 
                                                    prev_yr, ivs))
  
  return(data_reduced)
}


prepare_school_data <- function(data, model, relevant_cols, prev_yr, ivs) {
  # gen VA=as_mod`m'`e'_`s'_`t'
  data$VA <- unlist(data[sprintf("as_mod%smix_%s_test", model, prev_yr)])
  
  data$lott_VA <- rep(-1, nrow(data))
  # gen lott_VA=`c'_mod`m'`e'_`s'_`t' if lottery==0
  data[data$lottery == 0,][,"lott_VA"] <- 
    data[data$lottery == 0,][,sprintf("hm_mod%smix_%s_test", model, prev_yr)]
  # replace lott_VA=ch1_mod`m'`e'_`s'_`t' if lottery==1
  data[data$lottery == 1,][,"lott_VA"] <- 
    data[data$lottery == 1,][,sprintf("ch1_mod%smix_%s_test", model, prev_yr)]
  
  all_controls <- c("math_1998_imp", "math_1998_miss", "math_1998_imp_sq", "math_1998_imp_cub",
                    "read_1998_imp", "read_1998_miss", "read_1998_imp_sq", "read_1998_imp_cub",
                    "math_1999_imp", "math_1999_miss", "math_1999_imp_sq", "math_1999_imp_cub",
                    "read_1999_imp", "read_1999_miss", "read_1999_imp_sq", "read_1999_imp_cub",
                    "math_2000_imp", "math_2000_miss", "math_2000_imp_sq", "math_2000_imp_cub",
                    "read_2000_imp", "read_2000_miss", "read_2000_imp_sq", "read_2000_imp_cub",
                    "math_2001_imp", "math_2001_miss", "math_2001_imp_sq", "math_2001_imp_cub",
                    "read_2001_imp", "read_2001_miss" , "read_2001_imp_sq", "read_2001_imp_cub")
  
  iv_and_outcome = c("testz2003", "lott_VA", "VA","lottery_FE","lottery")
  
  #The VA of the school the student chose. For exmaple:ch2_mod1mix_02_test
  choice <- c(sprintf("ch1_mod%smix_%s_test", model, prev_yr),
              sprintf("ch2_mod%smix_%s_test", model, prev_yr),
              sprintf("ch3_mod%smix_%s_test", model, prev_yr),
              sprintf("hm_mod%smix_%s_test", model, prev_yr))
  
  # Create new_lott_VA: 0 when the student lost the lottery and VAM of the 
  # the 1st choice school had she won the lottery
  data$new_lott_VA <- rep(-1, nrow(data))
  data[data$lottery == 0,][,"new_lott_VA"] <- 0
  
  data[data$lottery == 1,][,"new_lott_VA"] <-
    data[data$lottery == 1,][,sprintf("ch1_mod%smix_%s_test", model, prev_yr)]
  
  # data$lottery <- as.factor(data$lottery)
  #leave only students with all the varibales 
  data_reduced <- data[,unique(c(relevant_cols, all_controls, iv_and_outcome, choice, ivs))]
  data_reduced <- data_reduced[complete.cases(data_reduced),]
  #problematic group, all chose the same school
  data_reduced <- data_reduced[which(data_reduced$lottery_FE != 14),]
  
  return(data_reduced)
}


# Aux functions for Deming analysis -----------------------------------------------------

run_school_nciv <- function(cms_data, #cms school data
                            ivs, #the potential instrumental variables that
                            # ntree=1000, # the number of trees in the RF prediction
                            iv_demean=FALSE,
                            control_demean=FALSE,
                            NC_demean=FALSE,
                            z_resid="no",
                            nc_resid="no",
                            post_resid_test="lm_fe",
                            with_VA){
  
  # init variables
  relevant_cols <- c("math_2002_imp", "read_2002_imp", "math_2002_imp_sq", "math_2002_imp_cub",
                     "read_2002_imp_sq", "read_2002_imp_cub", "math_2002_miss", "read_2002_miss")
  model <-  c("2") 
  prev_yr <- c("all")
  choice_var <- c(sprintf("ch1_mod%smix_%s_test", model, prev_yr),
                      sprintf("ch2_mod%smix_%s_test", model, prev_yr),
                      sprintf("ch3_mod%smix_%s_test", model, prev_yr),
                      sprintf("hm_mod%smix_%s_test", model, prev_yr))
  
  results <- as.data.frame(matrix(data= NA, nrow = 2, ncol = 4))
  # names(results) <- c("RF","F","bonf")
  names(results) <- c("F","bonf", "GAM","GAM sm.")

  var_to_remove = unlist(ifelse(with_VA,
                         list(c("testz2003", "VA","lottery_FE",ivs)),
                         list(c("testz2003", "VA","lottery_FE",ivs,choice_var))))
  for (i in seq(2)){  
 
    # Run F test
    results[i, 1] <- school_F_test(data= cms_data,
                                   iv_names = ivs[i],
                                   controls_names = relevant_cols,
                                   variables_to_remove= var_to_remove,
                                   z_resid = z_resid,
                                   nc_resid = nc_resid, 
                                   post_resid_test = post_resid_test)
    results[i, 1] <- ifelse(results[i, 1] < 0.01, "<0.01",round(results[i, 1],3))
    
      
    results[i, 2] <- school_bonf(data= cms_data,
                                 iv_names = ivs[i],
                                 controls_names = relevant_cols,
                                 variables_to_remove= var_to_remove,
                                 z_resid = z_resid,
                                 nc_resid = nc_resid)
    results[i, 2] <- ifelse(results[i, 2] < 0.01, "<0.01",round(results[i, 2],3))
    
    gam.pvals <- school_GAM(data= cms_data,
                               iv_names = ivs[i],
                               controls_names = relevant_cols,
                               variables_to_remove= var_to_remove,
                               z_resid = z_resid,
                               nc_resid = nc_resid)
    
    results[i, 3] <- ifelse(gam.pvals$pval.no.sm < 0.01,
                            "<0.01",
                            round(gam.pvals$pval.no.sm,3))
    
    results[i, 4] <- ifelse(gam.pvals$pval.sm < 0.01,
                            "<0.01",
                            round(gam.pvals$pval.sm,3))
    
  }
  return(list(results = results))
}



school_F_test <- function(data, iv_names,
                                 controls_names,
                                 variables_to_remove,
                                 z_resid,
                                 nc_resid,
                                 post_resid_test){
  
  # Compute F test to school data using fixed effects OLS
  
  # Get data
  
  IV <- data[,iv_names]
  controls <- data[,controls_names]
  NC <- dplyr::select(data,-one_of(c(variables_to_remove,controls_names)))
  
  # Residualize if user chooses
  
  if (z_resid == "fe"){
    IV <- get_resids_data(Z=IV,
                          NC = NC,
                          cntrls = controls,
                          nc_resid = nc_resid,
                          z_resid = z_resid,
                          fixed_e_var = data[,"lottery_FE"])$Z
    colnames(IV) <- "IV"
  }
  
  if (nc_resid == "fe"){
    NC <- get_resids_data(Z=IV,
                          NC = NC,
                          cntrls = controls,
                          nc_resid = nc_resid,
                          z_resid = z_resid,
                          fixed_e_var = data[,"lottery_FE"])$NC
  }
  
  if (nc_resid == "fe" | z_resid == "fe"){
    cntrl_observed = FALSE
    controls = NULL
  }
  
  # Run F test with controls and `lottery_FE` fixed effects
  
  if(!is.null(controls)){
  
      frmla1 <- get_formula_for_iv_by_controls(iv = "IV",fixed = "lottery_FE",
                                               controls = c(controls_names[-length(controls_names)],names(NC)))
      
      lm_fit_full <- feols(frmla1,data.frame(IV,NC,controls,lottery_FE = data[,"lottery_FE"]))
      
      frmla2 <- get_formula_for_iv_by_controls(iv = "IV",fixed = "lottery_FE",
                                               controls = c(controls_names[-length(controls_names)]))
      
      lm_fit_no_nc <- feols(frmla2,data.frame(IV,controls,lottery_FE = data[,"lottery_FE"]))
      
      sse_full <- sum((lm_fit_full$residuals)^2)
      
      df_residuals <- nrow(data) - (ncol(NC) + ncol(controls) + lm_fit_full$fixef_sizes - 1) 
      
      sigma_sq_full <- lm_fit_full$sigma2 # denom of f-stat

      sse_no_nc <- sum((lm_fit_no_nc$residuals)^2)
      
      df_diff <- ncol(as.matrix(NC)) 
      
      F_stat <- ((sse_no_nc - sse_full) / df_diff) / sigma_sq_full # Fstat of null beta_{NC} = 0
      
      f_stat_p_value <- pf(F_stat,
                           df_diff,
                           df_residuals,
                           lower.tail = FALSE)
      
      return(f_stat_p_value)
  } 
  # Same without controls
  if(is.null(controls)){
    if (post_resid_test == "lm_fe"){
      
        frmla1 <- get_formula_for_iv_by_controls(iv = iv_names,fixed = "lottery_FE",
                                                 controls = c(names(NC)))
        
        frmla2 <- get_formula_for_iv_by_controls(iv = iv_names,fixed = "lottery_FE",
                                                 controls = "1")
        
        p_data <- cbind(IV, NC, lottery_FE = data[,"lottery_FE"])
        names(p_data)[1] <- iv_names
        
        lm_fit_full <- feols(frmla1, p_data)
        
        lm_fit_no_nc <- feols(frmla2, p_data)
        

        sse_full <- sum((lm_fit_full$residuals)^2)
        
        df_residuals <- nrow(data) - (ncol(NC) + lm_fit_full$fixef_sizes - 1) 
        
        sigma_sq_full <- lm_fit_full$sigma2 # denom of f-stat

        sse_no_nc <- sum((lm_fit_no_nc$residuals)^2)
        
        df_diff <- ncol(as.matrix(NC)) 
        
        F_stat <- ((sse_no_nc - sse_full) / df_diff) / sigma_sq_full # Fstat of null beta_{NC} = 0
        
        f_stat_p_value <- pf(F_stat,
                             df_diff,
                             df_residuals,
                             lower.tail = FALSE)
  
        return(f_stat_p_value)
    }
    
    if (post_resid_test == "lm"){
        Z = data.frame(Z=IV)    
        names(Z) = "Z"
        return(estimate_lm_f_test(Z = Z, NC = NC, cntrl_observed = FALSE))
    }
  }  
}

school_bonf <- function(data,
                        iv_names,
                       controls_names,
                       variables_to_remove,
                       z_resid,
                       nc_resid){
  
  # Compute Bonf test to school data using fixed effects OLS
  
  # Get data
  
  IV <- data[,iv_names]
  controls <- data[,controls_names]
  NC <- dplyr::select(data,-one_of(c(variables_to_remove,controls_names)))
  
  # Residualize if user chooses
  
  if (z_resid == "fe"){
  IV <- get_resids_data(Z=IV,
                  NC = NC,
                  cntrls = controls,
                  nc_resid = nc_resid,
                  z_resid = z_resid,
                  fixed_e_var = data[,"lottery_FE"])$Z
  }
  
  if (nc_resid == "fe"){
  NC <- get_resids_data(Z=IV,
                  NC = NC,
                  cntrls = controls,
                  nc_resid = nc_resid,
                  z_resid = z_resid,
                  fixed_e_var = data[,"lottery_FE"])$NC
  }
  
  if (nc_resid == "fe" | z_resid == "fe"){
  cntrl_observed = FALSE
  controls = NULL
  }
  
  # Run Bonf with controls and `lottery_FE` fixed effects
  
  if(!is.null(controls)){
  
  frmla_bonf <- get_formula_for_iv_by_controls(iv = "x",fixed = "lottery_FE",
                                       controls = c("IV",controls_names[-length(controls_names)]))
  
  bonf_p_values <- apply(NC, 2, 
                         function(x){
                           # lm_fit <- lm("x ~ Z", data = data.frame(x=x, Z=Z), weights = weight_vec);
                           fe_lm_fit <- feols(fml = frmla_bonf,
                                              data.frame(IV=IV,x=x,controls,lottery_FE = data[,"lottery_FE"]))
                           return(summary(fe_lm_fit)$coeftable[1,4])})
  
  return(min(bonf_p_values,na.rm = T)*ncol(NC))
  } 
  # Same without controls
  if(is.null(controls)){
    
    frmla_bonf <- get_formula_for_iv_by_controls(iv = "x",fixed = "lottery_FE",
                                                 controls = c("IV"))
    
    bonf_p_values <- apply(NC, 2, 
                           function(x){
                             # lm_fit <- lm("x ~ Z", data = data.frame(x=x, Z=Z), weights = weight_vec);
                             fe_lm_fit <- feols(fml = frmla_bonf,
                                                data.frame(IV=IV,x=x,lottery_FE = data[,"lottery_FE"]))
                             return(summary(fe_lm_fit)$coeftable[1,4])})
    
    return(min(bonf_p_values)*ncol(NC))
  }  
}


school_GAM <- function(data,
                        iv_names,
                       controls_names,
                       variables_to_remove,
                       z_resid,
                       nc_resid){
  
  # Compute Bonf test to school data using fixed effects OLS
  
  # Get data
  
  Z <- data[,iv_names]
  controls <- data[,c(controls_names)]
  FE <- data[,c("lottery_FE")]
  NC <- dplyr::select(data,-one_of(c(variables_to_remove,controls_names)))
  
  binary.iv <- length(unique(Z)) < 3

  p <- ncol(NC)
  q.1 <- ifelse(is.null(controls),0,ncol(controls))
  q.2 <- length(unique(FE))
  
  gam.df <- as.data.frame(cbind(Z=Z,C=controls,FE=FE,NC=NC))
  
  which.NC.to.smooth <- apply(gam.df[,seq(q.1 + 3,ncol(gam.df))],
                              2,
                              function(x)length(unique(x))>2)
  names.NC.smooth <- names(which.NC.to.smooth[which.NC.to.smooth])
  names.NC.binary <- names(which.NC.to.smooth[!which.NC.to.smooth])
  
  which.cntrl.to.smooth <- apply(gam.df[,seq(2,q.1+1)],
                              2,
                              function(x)length(unique(x))>2)
  names.cntrl.smooth <- names(which.cntrl.to.smooth[which.cntrl.to.smooth])
  names.cntrl.binary <- names(which.cntrl.to.smooth[!which.cntrl.to.smooth])
  
  # Define formulas 
  
  gam.formula.full <- as.formula(paste0("Z ~ ",
                                        paste0(names(gam.df)[seq(2,q.1+1)],collapse = " + "),
                                        " + ",
                                        paste0("FE"),
                                        "+",
                                        paste0(names.NC.binary,collapse = " + "),
                                        "+",
                                        paste0("s(",names.NC.smooth,")",
                                               collapse = " + ")))
  
  gam.formula.smooth <- as.formula(paste0("Z ~ ",
                                        paste0("s(",names.cntrl.smooth,")",collapse = " + "),
                                        " + ",
                                        paste0(names.cntrl.binary,collapse = " + "),
                                        " + ",
                                        paste0("FE"),
                                        "+",
                                        paste0(names.NC.binary,collapse = " + "),
                                        "+",
                                        paste0("s(",names.NC.smooth,")",
                                               collapse = " + ")))

  # Run GAM -- no smooth controls

  if(binary.iv){
    full.fit <- gam(formula = gam.formula.full,
                       family = binomial(),
                       data = gam.df,
                       method = "ML")
  } else{
    full.fit <- gam(formula = gam.formula.full,
                       family = gaussian(),
                       data = gam.df,
                       method = "ML")
  }
  
  coef.full <- coef(full.fit)
  
  non.na.full <- sum(!is.na(coef.full))
  
  robust.vcov <- vcov(full.fit)

  beta.terms <- seq(q.1+q.2+1,non.na.full)
  beta.hat <- coef.full[!is.na(coef.full)]
  beta.hat <- beta.hat[beta.terms]
  inv.robust.vcov <- MASS::ginv(robust.vcov)
  wald.stat <- t(beta.hat) %*% inv.robust.vcov[beta.terms, beta.terms] %*% beta.hat
  pval.wald.no.sm <- pchisq(q=wald.stat, df = length(beta.hat), lower.tail = FALSE)
  
  
  # Run GAM -- with smooth controls
  
  if(binary.iv){
    full.fit.sm <- gam(formula = gam.formula.smooth,
                         family = binomial(),
                         data = gam.df,
                         method = "ML")
  } else{
    full.fit.sm <- gam(formula = gam.formula.smooth,
                       family = gaussian(),
                       data = gam.df,
                       method = "ML")
  }
              
  
  coef.full.sm <- coef(full.fit.sm)
  
  non.na.full.sm <- sum(!is.na(coef.full.sm))
  
  which.nc.coef <- which(stringr::str_detect(string = names(coef.full.sm),pattern = "NC"))

  robust.vcov.sm <- vcov(full.fit.sm)
  
  beta.hat.sm <- coef.full.sm[!is.na(coef.full.sm)]
  beta.hat.sm <- beta.hat.sm[which.nc.coef]
  inv.robust.vcov.sm <- MASS::ginv(robust.vcov.sm)
  wald.stat.sm <- t(beta.hat.sm) %*% inv.robust.vcov.sm[which.nc.coef, which.nc.coef] %*% beta.hat.sm
  pval.wald.sm <- pchisq(q=wald.stat.sm, df = length(beta.hat.sm), lower.tail = FALSE)
  
  
  return(list(pval.no.sm = pval.wald.no.sm,
              pval.sm = pval.wald.sm))
  
}



