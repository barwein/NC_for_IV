###
# Run Rich-Covariates (RC) simulations
###

source("Aux_functions/Iteration_aux_functions.R")


# NOTE: Numerical results might differ a bit since simulations were conducted in a power-cluster with parallel computation
set.seed(510510)
RC.testing <- Testing_RC_simulation(n_iter = 10e2,
                                     n_obs_vec = 10e4,
                                     n_nc = 10,
                                     nc_coef_vec = seq(0,5,0.5))

# Compute proprtion of null rejections
RC.testing[,.(prop.F = mean(nc.F.pval <= 0.05),
       prop.bonf = mean(nc.bonf.pval <= 0.05),
       prop.wald = mean(nc.wald.pval <= 0.05),
       prop.GAM = mean(gam_wald <= 0.05),
       prop.GAM.smooth.cntrls = mean(gam_wald.smooth.cntrl <= 0.05)),
    by = c("n","nc_coef")]


# Save results
write.csv(RC.testing, "Simulations/Testing_RC_1000iter.csv", row.names = FALSE)
