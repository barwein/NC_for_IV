### 
# Aux functions that manage the simulations procedure
###



# Libraries ---------------------------------------------------------------

source("Aux_functions/Data_Generator.R")
source("Aux_functions/Estimation_Functions.R")

library(parallel)
library(data.table)
library(ivreg)

# NCO simulations ---------------------------------------------------------------


single_general_iteration <- function(Z,
                                     NC, 
                                     cntrls = NULL,
                                     cntrl_observed){
  
  #' @description run one iteration for given data
  #' @return list of p-values for rf, bonf, and f-stat methods
  
  
  lm_bonf_pvalues <- estimate_lm_bonf(Z = Z, NC = NC, cntrls = cntrls, cntrl_observed = cntrl_observed)
  lm_bonf_min_pvalue <- min(lm_bonf_pvalues)*ncol(NC)
  
  lm_f_p_value <- estimate_lm_f_test(Z = Z, NC = NC, cntrls = cntrls, cntrl_observed = cntrl_observed)
  
  wald_p_value <- wald.robust.function(Z = Z, NC = NC, cntrls = cntrls, cntrl_observed = cntrl_observed)
  
  gam_p_value <- gam.chisq.function(Z = Z, NC = NC, cntrls = cntrls, cntrl_observed = cntrl_observed)
  
 
  return(list(bonf_pval = lm_bonf_min_pvalue,
              f_pval = lm_f_p_value,
              wald_pval = wald_p_value,
              gam_wald.no.p = gam_p_value$p.wald.no.p,
              gam_wald.no.p.smooth.cntrl = gam_p_value$pval.wald.no.p.all.smooth))
}


multiple_iterations_same_setup <- function(n_col_nc,
                                           n_iter,
                                           n_obs,
                                           sigma_sq_z,
                                           sigma_sq_nc,
                                           z_u_coef,
                                           nc_u_coef,
                                           with_cntrls,
                                           sigma_sq_cntrls = NULL,
                                           z_cntrls_coef = NULL,
                                           nc_cntrls_coef = NULL,
                                           cntrl_observed,
                                           nonlinear_weight,
                                           noise_type){
    
  #' @description run multiple iteration using given parameter setup
  #' @details parameters descripition as in other R files
  #' @param n_iter is the number of iteration to perform
  #' @return data frame of results (row-binded)
  
  one.spec.results <- 
                mclapply(seq(n_iter), function(iter){
                    # sample data
                    sample_data <- one_data_sample(n_col_nc = n_col_nc, 
                                                   n_obs = n_obs, 
                                                   sigma_sq_z = sigma_sq_z,
                                                   z_u_coef = z_u_coef,
                                                   sigma_sq_nc = sigma_sq_nc,
                                                   nc_u_coef = nc_u_coef,
                                                   with_cntrls = with_cntrls, 
                                                   sigma_sq_cntrls = sigma_sq_cntrls,
                                                   z_cntrls_coef = z_cntrls_coef,
                                                   nc_cntrls_coef = nc_cntrls_coef, 
                                                   nonlinear_weight = nonlinear_weight,
                                                   noise_type = noise_type)
                
                    # run one iterations
                    iter_results <- single_general_iteration(Z = sample_data$Z,
                                                             NC = sample_data$NC,
                                                             cntrls = sample_data$cntrls,
                                                             cntrl_observed = cntrl_observed)
                 
                    # save results
                    data.table(iter = iter,
                               n_obs = n_obs,
                               n_col_nc = n_col_nc,
                               z_u_coef = z_u_coef,
                               nonlinear_weight = nonlinear_weight,
                               bonf_pval = iter_results$bonf_pval,
                               f_pval = iter_results$f_pval,
                               wald_pval = iter_results$wald_pval,
                               gam_wald.no.p = iter_results$gam_wald.no.p,
                               gam_wald.no.p.smooth.cntrl = iter_results$gam_wald.no.p.smooth.cntrl,
                               cntrl_observed = cntrl_observed)
                    
                    }, 
                    mc.cores = 1
                    # mc.cores = 4
                    )
  
  return(rbindlist(one.spec.results))
}


multiple_iterations_different_setups <- function(n_iter,
                                                 n_obs_vec,
                                                 sigma_sq_z,
                                                 sigma_sq_nc,
                                                 z_u_coef_vector, 
                                                 nc_u_coef_vector,
                                                 with_cntrls,
                                                 sigma_sq_cntrls = NULL,
                                                 z_cntrls_coef = NULL,
                                                 nc_cntrls_coef = NULL,
                                                 cntrl_observed,
                                                 nonlinear_weight,
                                                 noise_type = "light"){
  
  #' @description run multiple iteration for different parameters setup
  #' @param z_u_coef_vector is vector of paramteters -- each parameter for different simulation run
  #' @param nc_u_coef_vector list of vector of parameters
  #' @return data frame of p-value results for each parameter setup and simulation iteration (large data frame)
  
  results_df <- 
                lapply(z_u_coef_vector, function(z_u_coef){
                  
                  rbindlist(lapply(n_obs_vec, function(n.obs){
                    
                    print(paste0("z-u coef = ", z_u_coef,
                                "; n = ", n.obs))
                    
                      multiple_iterations_same_setup(n_col_nc = length(nc_u_coef_vector),
                                                     n_iter = n_iter,
                                                     n_obs = n.obs,
                                                     sigma_sq_z = sigma_sq_z,
                                                     sigma_sq_nc = sigma_sq_nc,
                                                     z_u_coef = z_u_coef,
                                                     nc_u_coef = nc_u_coef_vector, 
                                                     with_cntrls = with_cntrls, 
                                                     sigma_sq_cntrls = sigma_sq_cntrls,
                                                     z_cntrls_coef = z_cntrls_coef,
                                                     nc_cntrls_coef = nc_cntrls_coef,
                                                     cntrl_observed = cntrl_observed,
                                                     nonlinear_weight = nonlinear_weight,
                                                     noise_type = noise_type) 
                  }
                  ))
                  })
  return(rbindlist(results_df))
}




# Rich covariates simulations ---------------------------------------------


Testing_RC_simulation <- function(n_iter, 
                                  n_obs_vec,
                                  n_nc,
                                  nc_coef_vec){
  
  RC_results <- lapply(n_obs_vec, function(n_obs){
    rbindlist(lapply(nc_coef_vec, function(nc_coef){
      
      print(paste0("n = ", n_obs,"; nc.coef = ", nc_coef))
      
      rbindlist(lapply(seq(n_iter),function(m){
        
        c.data <- Sample_RC_data(n_obs = n_obs,
                                 nc.coef = nc_coef,
                                 n_nc = n_nc)
        
        outcome_reg <- ivreg("Y ~ T_ + X | Z + X", data = as.data.frame(c.data))
        ivrg.summary <- summary(outcome_reg, vcov = vcovHC(outcome_reg,type="HC3"))
        T.coef <- ivrg.summary$coefficients[2,1]
        T.se <- ivrg.summary$coefficients[2,2]
        
        nc.F.test <- estimate_lm_f_test(Z = c.data$Z,
                                        NC = c.data$NC,
                                        cntrls = data.frame(c.data$X),
                                        cntrl_observed = TRUE)
        
        nc.bonf <- estimate_lm_bonf(Z = c.data$Z,
                                    NC = c.data$NC,
                                    cntrls = data.frame(c.data$X),
                                    cntrl_observed = TRUE)
        bonf.pval <- min(nc.bonf)*n_nc  
        
        nc.wald <- wald.robust.function(Z = c.data$Z,
                                        NC = c.data$NC,
                                        cntrls = data.frame(c.data$X),
                                        cntrl_observed = TRUE)
        
        gam_p_value <- gam.chisq.function(Z = c.data$Z,
                                          NC = c.data$NC,
                                          cntrls = data.frame(c.data$X),
                                          cntrl_observed = TRUE)
        
        
        data.table(n = n_obs,
                   ncol.nc = n_nc,
                   nc_coef = nc_coef,
                   hat.LATE = T.coef,
                   hat.LATE.se = T.se,
                   nc.F.pval = nc.F.test,
                   nc.bonf.pval = bonf.pval,
                   nc.wald.pval = nc.wald,
                   gam_wald = gam_p_value$p.wald.no.p,
                   gam_wald.smooth.cntrl = gam_p_value$pval.wald.no.p.all.smooth,
                   iter = m)
      }))                  
    }))
  })
  
  return(rbindlist(RC_results))
}










