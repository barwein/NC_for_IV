###
# Run NCO simulations
###

source("Aux_functions/Iteration_aux_functions.R")

z_u_coef_grid <- seq(0,1,0.1)

# NOTE: Numerical results might differ a bit since simulations were conducted in a power-cluster with parallel computation

# Linear DGP simulations
set.seed(52062)
linear.dgp <- multiple_iterations_different_setups(n_iter = 10e2,
                                                    n_obs_vec = 10e4,
                                                    sigma_sq_z = 25,
                                                    sigma_sq_nc = 3,
                                                    z_u_coef_vector = z_u_coef_grid,
                                                    nc_u_coef_vector = rep(0.25,10),
                                                    with_cntrls = TRUE,
                                                    sigma_sq_cntrls = 3,
                                                    z_cntrls_coef = rep(1,5),
                                                    nc_cntrls_coef = rep(0,5),
                                                    cntrl_observed = TRUE,
                                                    nonlinear_weight = 0)



# Nonlinear DGP (truncated parabolic) simulations
set.seed(52062)
nonlinear.dgp <- multiple_iterations_different_setups(n_iter = 10e2,
                                                    n_obs_vec = 10e4,
                                                    sigma_sq_z = 3,
                                                    sigma_sq_nc = 3,
                                                    z_u_coef_vector = z_u_coef_grid,
                                                    nc_u_coef_vector = rep(0.25,10),
                                                    with_cntrls = TRUE,
                                                    sigma_sq_cntrls = 3,
                                                    z_cntrls_coef = rep(1,5),
                                                    nc_cntrls_coef = rep(0,5),
                                                    cntrl_observed = TRUE,
                                                    nonlinear_weight = 1)

# Combine results
sim_results <- rbindlist(list(linear.dgp,nonlinear.dgp))
write.csv(sim_results, "Simulations/Sim_results_linear_non_linear_DGP.csv", row.names = FALSE)


