###
# Script that contain different estimation functions for the simulations and data examples
###


# Libraries ---------------------------------------------------------------


library(dplyr)
library(lfe)
library(foreach)
library(mgcv)
library(caret)
library(systemfit)
library(sandwich)
library(aod)

source("Aux_functions/Data_Generator.R")


# Functions ---------------------------------------------------------------

estimate_lm_bonf <- function(Z, 
                             NC,
                             cntrls = NULL,
                             cntrl_observed,
                             weight_vec = NULL,
                             cluster.robust = NULL){
  #' @description compute bonforoni adjusted p-values for NC_i~Z marginal lm models
  #' @param Z vector of IV
  #' @param NC matrix of negative controls
  #' @param cntrls are the control covariates matrix
  #' @return vectors (length = ncol(NC)) of bonforoni adjusted p-values
  #' 
 
  # Without controls
  if (!cntrl_observed){
    bonf_p_values <- apply(NC, 2, 
                           function(x){
                              lm_fit <- lm(formula = x~.,
                                           data = data.frame(x=x, Z=Z), weights = weight_vec);
                              return(summary(lm_fit)$coefficients[2,4])})
  }
  # With controls
  else{
    bonf_p_values <- apply(NC, 2, 
                           function(x){
                            
                             lm_fit <- lm(formula = x~., data = data.frame(x=x, Z=Z, cntrls),
                                          weights = weight_vec)
                             if(is.null(cluster.robust)){
                              return(summary(lm_fit)$coefficients[2,4]) 
                             } else{
                               lm.coef <- coef(lm_fit)
                               robust.vcov <- vcovCL(lm_fit, cluster = cluster.robust)
                               robust.t <- lm.coef[2] / sqrt(robust.vcov[2,2])
                               robust.pval <- 2*pt(abs(robust.t), lm_fit$df.residual, lower.tail = FALSE)
                               return(robust.pval)
                             }
                              })
  }
  return(bonf_p_values)
}

estimate_lm_f_test <- function(Z, NC, cntrls = NULL, cntrl_observed, f_weights=NULL){
  #' @description Compute F-stat p-values of lm model Z~NC
  #' @return F-test P-value
  
  
  if(!cntrl_observed){
    f_formula <- formula(Z  ~ .)
    environment(f_formula) <- environment()
    lm_fit <- lm(formula = f_formula, data = data.frame(Z=Z,NC), weights = f_weights)
    
    f_stat_p_value <- pf(summary(lm_fit)$fstatistic[1],
                       summary(lm_fit)$fstatistic[2],
                       summary(lm_fit)$fstatistic[3],
                       lower.tail = FALSE)
  }
  else{ # with controls
    
    # Full model
    f_formula <- formula(Z  ~ .)
    environment(f_formula) <- environment()
    lm_fit_full <- lm(formula = f_formula, data = data.frame(Z,NC,cntrls), weights = f_weights)
    sse_full <- sum((lm_fit_full$residuals)^2)
    sigma_sq_full <- sse_full / lm_fit_full$df.residual # denom of f-stat
    
    # Partial model
    f_formula <- formula(Z  ~ .)
    environment(f_formula) <- environment()
    lm_fit_no_nc <- lm(formula = f_formula, data = data.frame(Z,cntrls),weights = f_weights)
    sse_no_nc <- sum((lm_fit_no_nc$residuals)^2)
    
    df_diff <- ncol(as.matrix(NC)) 
    
    # Compute statistic
    
    F_stat <- ((sse_no_nc - sse_full) / df_diff) / sigma_sq_full # Fstat of null beta_{NC} = 0
    
    f_stat_p_value <- pf(F_stat,
                         df_diff,
                         lm_fit_full$df.residual,
                         lower.tail = FALSE)
    
  }
  return(f_stat_p_value)
}



lm_nc_on_cntrls <- function(NC, 
                            cntrls,
                            wght=NULL){
  #' @description residualize NC on controls using linear regression
  #' 
  nc_names <- names(NC)
  resid <- foreach(i=1:ncol(NC), .combine = "cbind") %dopar%
    {
      c_formula = formula(nc ~ .)
      environment(c_formula) <- environment()
      lm_nc <- lm(formula = c_formula, data = data.frame(nc = NC[,i], cntrls), weights = wght)
      nc_resid <- NC[,i] - lm_nc$fitted.values  
      
      results <- data.frame(nc_resid)
      names(results)[1] <- nc_names[i]
      results
    }
  return(resid)
}





felm_nc_on_controls <- function(NC,
                                cntrls,
                                fixed_e_var){
  #' @description residualize NC on controls using fixed-effect linear regression
  #' @param fixed_e_var fixed-effect variable
  #' @param cntrls controls without fixed-effect variable
  
  nc_names <- names(NC)
  fixed_e_var <- as.data.frame(fixed_e_var)
  names(fixed_e_var) <- "fe"
  df <- cbind(cntrls, fixed_e_var)
  names(df) <- c(names(cntrls),names(fixed_e_var))
  
  fe_formula <- as.formula(sprintf("nc ~ %s| fe", paste(names(cntrls), collapse = " + ")))
  
  resid <- foreach(i=1:ncol(NC), .combine = "cbind") %dopar%
    {
      lm_fe_nc <- felm(formula=fe_formula, data = cbind(nc = NC[,i], df))
      nc_resid <- NC[,i] - lm_fe_nc$fitted.values  
      
      results <- data.frame(nc_resid)
      # names(results)[1] <- paste0("NC_resid_",i)
      names(results)[1] <- nc_names[i]
      results
    }
  return(resid)
}

felm_Z_on_controls <- function(Z,
                               cntrls,
                               fixed_e_var){
  #' @description residualize NC on controls using fixed-effect linear regression
  #' @param fixed_e_var fixed-effect variable
  #' @param cntrls controls without fixed-effect variable
  
  fixed_e_var <- as.data.frame(fixed_e_var)
  names(fixed_e_var) <- "fe"
  df <- cbind(cntrls, fixed_e_var)
  names(df) <- c(names(cntrls),names(fixed_e_var))
  
  fe_formula <- as.formula(sprintf("Z ~ %s| fe", paste(names(cntrls), collapse = " + ")))
  
  lm_fe_z <- felm(formula=fe_formula, data = cbind(Z = Z, df))

  z_resid <- Z - lm_fe_z$fitted.values  
  
  return(z_resid)
}


residualize_all <- function(Z, NC, cntrls, nc_resid ,z_resid, fixed_e_var ,ntree, wght = NULL){
  #' @description residualize NC and Z on controls using specified types
  #' @param nc_type either "lm" for linear; "rf" for random-forest; "fe" for fixed effect; "no" for no resid
  #' @param z_type either "lm" for linear; "rf" for random-forest; "fe" for fixed effect; "no" for no resid
  #'
  
 
  if (z_resid == "lm"){
    z_formula <- formula(Z ~ .)
    environment(z_formula) <- environment()
    z_lm <- lm(formula = z_formula, data = data.frame(Z, cntrls), weights = wght)
    z_res <- Z - z_lm$fitted.values
  }
  
  if (z_resid == "fe"){
    z_res <- felm_Z_on_controls(Z = Z, cntrls = cntrls, fixed_e_var = fixed_e_var)
  }
  if (z_resid == "no"){
    z_res <- Z
  }
  
  if (nc_resid == "lm"){
    nc_res <- lm_nc_on_cntrls(NC = NC, cntrls = cntrls, wght = wght)
  }
  if (nc_resid == "fe") { 
    nc_res <- felm_nc_on_controls(NC = NC,cntrls = cntrls ,fixed_e_var = fixed_e_var)
  }
  if (nc_resid == "no") { 
    nc_res <- NC
  }
  
  return(list(z_res = z_res, nc_res = nc_res))
}

get_resids_data <- function(Z, NC, cntrls, nc_resid ,z_resid, fixed_e_var = NULL ,ntree=1000, wght = NULL){
  #' @description get matrix of user choice residuals
  #' @param nc_resid is NC residual type
  #' @param z_resid is Z residual type
  
  resids_df <- residualize_all(Z = Z,
                               NC = NC,
                               cntrls = cntrls,
                               nc_resid = nc_resid,
                               z_resid = z_resid,
                               fixed_e_var = fixed_e_var ,
                               ntree = ntree,
                               wght = wght)
  if (z_resid != "no"){
    cntrls = NULL
  }
  return(list(Z=resids_df$z_res, cntrls = cntrls, NC = resids_df$nc_res))
}


Smooth.cntrls.formula <- function(cntrls,
                                  gam.df,
                                  p,
                                  q){
  # Obtain formula for GAM regression with smooth controls (non-binary controls)
  
  # Check which controls are non-binary
  which.cntrl.to.smooth <- apply(cntrls, 2, function(x){length(unique(x))>2})
  cntrl.smooth <- seq(2,(q+1))[which.cntrl.to.smooth]
  cntrl.linear <- seq(2,(q+1))[!which.cntrl.to.smooth]
  
  if(length(cntrl.smooth) > 0 & length(cntrl.linear) > 0){
    gam.formula.null.smooth.cntrl <- paste0("Z ~ ",       
                                         paste0("s(",names(gam.df)[cntrl.smooth],")",
                                                collapse = " + "),
                                         " + ",
                                         paste0(names(gam.df[cntrl.linear]), collapse = " + "))
  
  gam.formula.full.no.p.smooth.cntrl <- paste0("Z ~ ",
                                      paste0("s(",names(gam.df)[cntrl.smooth],")",
                                             collapse = " + "),
                                      "+",
                                      paste0(names(gam.df[cntrl.linear]), collapse = "+"),
                                      " + ",
                                      # paste0("s(",names(gam.df)[(q+2):(p+q+1)],",bs='bs', fx=TRUE)",
                                      paste0("s(",names(gam.df)[(q+2):(p+q+1)],")",
                                             collapse = " + "))
  }
  if(length(cntrl.smooth) == 0 & length(cntrl.linear) > 0){
    gam.formula.null.smooth.cntrl <- paste0("Z ~ ",
                             paste0(names(gam.df[cntrl.linear]), collapse = " + "))
    
    # 
    gam.formula.full.no.p.smooth.cntrl <- paste0("Z ~ ",
                        paste0(names(gam.df[cntrl.linear]), collapse = " + "),
                          " + ",
                          paste0("s(",names(gam.df)[(q+2):(p+q+1)],")",
                                 collapse = " + "))
}
  if(length(cntrl.smooth) > 0 & length(cntrl.linear) == 0){
    gam.formula.null.smooth.cntrl <- paste0("Z ~ ",       
                                             paste0("s(",names(gam.df)[cntrl.smooth],")",
                                                    collapse = " + ")
                                             )
    
    # 
    gam.formula.full.no.p.smooth.cntrl <- paste0("Z ~ ",
                                  paste0("s(",names(gam.df)[cntrl.smooth],")",
                                       collapse = " + "),
                                " + ",
                                # paste0("s(",names(gam.df)[(q+2):(p+q+1)],",bs='bs', fx=TRUE)",
                                paste0("s(",names(gam.df)[(q+2):(p+q+1)],")",
                                       collapse = " + "))
}
return(list(gam.null.smooth = gam.formula.null.smooth.cntrl,
              gam.full.smooth = gam.formula.full.no.p.smooth.cntrl))
}

gam.chisq.function <- function(Z,
                               NC,
                               cntrls = NULL,
                               cntrl_observed, 
                               g_weights=NULL,
                               cluster.robust = NULL){
  # Perform NC test with GAM with smooth and non-smooth controls
  # g_weights are weights for weighted GAM
  
  p <- ncol(NC)
  q <- ifelse(is.null(cntrls),0,ncol(cntrls))
  
  if(cntrl_observed){
    gam.df <- as.data.frame(cbind(Z=Z,C=cntrls,NC=NC))
    
    which.cntrl.to.smooth <- apply(cntrls, 2, function(x){length(unique(x))>2})
    cntrl.smooth <- seq(2,(q+1))[which.cntrl.to.smooth]
    cntrl.linear <- seq(2,(q+1))[!which.cntrl.to.smooth]
      
    gam.formula.full <- as.formula(paste0("Z ~ ",
                                          paste0(names(gam.df)[2:(q+1)],collapse = " + "),
                                          " + ",
                                          paste0("s(",names(gam.df)[(q+2):(p+q+1)],")",collapse = " + ")))
    gam.formula.null <- as.formula(paste0("Z ~ ",
                                          paste0(names(gam.df)[2:(q+1)],collapse = " + ")))
    
    gam.smooth <- Smooth.cntrls.formula(cntrls = cntrls, gam.df = gam.df,
                                        p=p, q=q)
  
    gam.formula.null.smooth.cntrl <- as.formula(gam.smooth$gam.null.smooth)
    gam.formula.full.no.p.smooth.cntrl <- as.formula(gam.smooth$gam.full.smooth)
    
  }
  else{
    gam.df <- as.data.frame(cbind(Z=Z,NC=NC))
    gam.formula.full <- as.formula(paste0("Z ~ ",
                                          paste0("s(",names(gam.df)[(q+2):(p+q+1)],")",collapse = " + ")))
    gam.formula.null <- as.formula(paste0("Z ~ 1"))
  }
  
  
  
  # Non smooth controls
  
  gam.full.no.p <- gam(formula = gam.formula.full,
                       data = gam.df,
                       method = "ML",
                       weights = g_weights)
  gam.full.coef <- coef(gam.full.no.p)
  
  non.na.full <- sum(!is.na(gam.full.coef))
  
  if(is.null(cluster.robust)){
    robust.vcov <- vcov(gam.full.no.p)
  } else{
    robust.vcov <- vcovCL(gam.full.no.p, cluster = cluster.robust)
  }
  
  # Obtain coef., vcov matrix and p-value using Wald test
  
  beta.terms <- seq(q+2,non.na.full)
  beta.hat <- gam.full.coef[!is.na(gam.full.coef)]
  beta.hat <- beta.hat[beta.terms]
  inv.robust.vcov <- MASS::ginv(robust.vcov)
  wald.stat <- t(beta.hat) %*% inv.robust.vcov[beta.terms, beta.terms] %*% beta.hat
  pval.wald.no.p <- pchisq(q=wald.stat, df = length(beta.hat), lower.tail = FALSE)
  
  
  # Smooth (numeric) controls
  
  gam.full.smooth.all.no.p <- gam(formula = gam.formula.full.no.p.smooth.cntrl,
                                  data = gam.df,
                                  method = "ML",
                                  weights = g_weights) # full model
  
 
  
  gam.full.smooth.coef <- coef(gam.full.smooth.all.no.p)

  # non.na.smooth.all <- sum(!is.na(gam.full.smooth.coef))
  non.na.smooth.all <- length(gam.full.smooth.coef)
  
  if(is.null(cluster.robust)){
    robust.vcov.sm <- vcov(gam.full.smooth.all.no.p)
  } else{
    robust.vcov.sm <- vcovCL(gam.full.smooth.all.no.p, cluster = cluster.robust)
  }
  
  n.cntrls.coef <- 1 + length(cntrl.smooth)*9 + length(cntrl.linear)
  beta.terms.sm <- seq(n.cntrls.coef+1,non.na.smooth.all)
  beta.hat.sm <- gam.full.smooth.coef[!is.na(gam.full.smooth.coef)]
  beta.hat.sm <- beta.hat.sm[beta.terms.sm]

  inv.robust.vcov.sm <- MASS::ginv(robust.vcov.sm)
  wald.stat.sm <- t(beta.hat.sm) %*% inv.robust.vcov.sm[beta.terms.sm, beta.terms.sm] %*% beta.hat.sm
  pval.wald.smooth.cntrls.no.p <- pchisq(q=wald.stat.sm, df = length(beta.hat.sm),
                                         lower.tail = FALSE)
  
  return(list(pval.wald.no.p=pval.wald.no.p,
          pval.wald.smooth.cntrl = pval.wald.smooth.cntrls.no.p))

}



wald.robust.function <- function(Z,
                                 NC,
                                 cntrls = NULL,
                                 cntrl_observed,
                                 w_weights=NULL,
                                 cluster.robust = NULL){
  
  # Run NC test via Wald test and robust s.e.
  
  p <- ncol(NC)
  q <- ifelse(is.null(cntrls),0,ncol(cntrls))
  
  if(cntrl_observed){
    wald.df <- as.data.frame(cbind(Z=Z,C=cntrls,NC=NC))
    lm.formula <- as.formula(paste0("Z ~ ",
                                    paste0(names(wald.df)[2:(q+1)],collapse = " + "),
                                    " + ",
                                    paste0(names(wald.df)[(q+2):(q+p+1)],collapse = " + ")))
    }
  else{
    wald.df <- as.data.frame(cbind(Z=Z,NC=NC))
    lm.formula <- as.formula(paste0("Z ~ ",
                                    paste0(names(wald.df)[2:(p+1)],collapse = " + ")))
    
  }
  
  # Estimation
  lm.for.wald <- lm(formula = lm.formula,
                    data = wald.df,
                    weights = w_weights)
  # Get robust COV
  if(is.null(cluster.robust)){
    vcov.robust <- vcovHC(lm.for.wald, "HC3")
  } else{
    vcov.robust <- vcovCL(lm.for.wald, cluster = cluster.robust)
  }
  # Coef.
  wald.coef <- coef(lm.for.wald)
  # Non NA coef
  N.non.na.coef <- sum(!is.na(wald.coef))
  # Wald test
  any.error <- TRUE
  wald.results <- tryCatch({
                    message("Estimating Wald")  
                    wald.res = wald.test(Sigma = vcov.robust,
                      b = wald.coef[!is.na(wald.coef)],
                      # Terms = seq(1+q+1,1+q+p))
                      Terms = seq(1+q+1,N.non.na.coef))
                      any.error = FALSE
                      wald.res
                    },
                    error = function(cond){
                      message("Error - Wald didn't converge, doing manual Wald test")
                      beta.hat <- wald.coef[seq(1+q+1,N.non.na.coef)]
                      non.na.coef <- which(!is.na(beta.hat))
                      beta.hat <- beta.hat[non.na.coef]
                      vcov.hat <- vcov.robust[seq(1+q+1,N.non.na.coef),seq(1+q+1,N.non.na.coef)]
                      vcov.hat <- vcov.hat[non.na.coef,non.na.coef]
                      wald.stat <- t(beta.hat) %*% MASS::ginv(vcov.hat) %*% beta.hat
                      wald.pval <- pchisq(q=wald.stat,df = length(beta.hat),lower.tail = FALSE)
                      return(wald.pval)
                    })
 
  wald.output <- ifelse(!any.error,
                      c(wald.results$result$chi2[3]),
                      wald.results)

    return(wald.output)
  
}




