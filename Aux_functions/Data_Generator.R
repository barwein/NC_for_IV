###
# Script that sample data from user choice of DGP
###


# Libraries ---------------------------------------------------------------

library(foreach)
library(mvtnorm)
library(sandwich)


# Functions ---------------------------------------------------------------

sample_z_u_cntrls <- function(n_obs, 
                              sigma_sq_z, 
                              z_u_coef,
                              with_cntrls,
                              sigma_sq_cntrls = NULL, 
                              cntrls_coef = NULL,
                              nonlinear_weight,
                              noise_type){
  #' @details Function that sample Z and U variables
  #' 
  #' @param n_obs number of observations
  #' @param sigma_sq_z Z variance
  #' @param z_u_coef vector of coefficents for Z ~ gamma*U ...
  #' @param with_cntrls boolean variable, TRUE for model with controls, FALSE o.w.
  #' @param sigma_sq_cntrls controls variance
  #' @param cntrls_coef vector of coefficents for Z ~ cntrls * cntrls_coef
  #' @param noise_type is the IV noise variable: gaussian (light-tail) or gaussian mixture (heavy-tailed)
  #' 
  #' @return list of Z (IV), U (APV), and controls
  #' 
  if(with_cntrls){
      # Generate U and controls data
      
      U <- runif(n_obs, -3 , 3)

      cntrls_c <- matrix(rnorm(n_obs*3, 1, sqrt(sigma_sq_cntrls)),
                          nrow = n_obs, ncol = 3)
      
      cntrls_b <- matrix(cbind(rbinom(n_obs, 1, prob = 0.3),
                         rbinom(n_obs, 1, prob = 0.5)),
                          nrow = n_obs, ncol = 2)
    
      cntrls <- cbind(cntrls_c, cntrls_b)
      
      # Controls effect
      
      cntrl_linear <- cntrls %*% cntrls_coef
     
      # U effect on Z
      
      nonlinear_effect_u <- z_u_coef * 8 * ifelse(U^2<1.5,U^2,1.5)
      linear_effect_u <- z_u_coef * U
      
      # Z signal
      
      effect_z <- cntrl_linear + 
                  ((1-nonlinear_weight)*linear_effect_u) + 
                   (nonlinear_weight*nonlinear_effect_u)
      
      # General Z noise (either light or heavy tailed; default is light == normal)
      
      if (noise_type == "light"){
        noise_z <- simulate_gauss_mix(p = 1, n_obs = n_obs,
                                      sigma_1 = sqrt(var(effect_z)),
                                      sigma_2 = 2.5*sqrt(sigma_sq_z))
      }
      
      if (noise_type == "heavy"){
        noise_z <- simulate_gauss_mix(p = 0.5, n_obs = n_obs,
                                      sigma_1 = sqrt(sigma_sq_z),
                                      sigma_2 = 2.5*sqrt(sigma_sq_z))
      }
      # Construct Z
      Z <- effect_z + noise_z
  }
  return(list(Z = c(Z),U = c(U), cntrls = cntrls))
}


sample_NC <- function(n_col,
                      sigma_sq_nc,
                      nc_u_coef,
                      U,
                      with_cntrls,
                      cntrls = NULL,
                      cntrls_coef = NULL){
  
  #' @details Function that sample NC variables
  #'
  #' @param n_col number of NC 
  #' @param sigma_sq_nc white noise variance of NC
  #' @param nc_u_coef coefficients vector of NC_i = nc_u_coef_i*f(U) + noise + (cntrls)
  #' @param U unmeasured confounding
  #' @param with_cntrls boolean variable, TRUE for model with controls, FALSE o.w.
  #' @param cntrls matrix of controls
  #' @param contrls_coef vector of control coef. s.t. NC_i  = nc_u_coef_i*f(u) + cntrls'cntrls_coef + noise 
  
  n_obs <- dim(matrix(U))[1]
  
  if(with_cntrls){
  
    u_effect <- matrix(U, n_obs, n_col) %*% diag(nc_u_coef)
    
    cntrls_effect <- matrix((cntrls %*% cntrls_coef), n_obs, n_col) 
    
    ttl_effect <- u_effect + cntrls_effect
    
    noise_nc <- simulate_gauss_mix(p = 1, n_obs = n_obs*n_col,
                                  sigma_1 = sqrt(sigma_sq_nc),
                                  sigma_2 = 2.5*sqrt(sigma_sq_nc))

    NC <- data.frame(ttl_effect + matrix(noise_nc,nrow = n_obs, ncol = n_col))
    
  }
  else{ # no controls
    
    effect_matrix <- matrix(U*nc_u_coef, n_obs, n_col)
    NC <- data.frame(effect_matrix + matrix(rnorm(n_obs*n_col, 0, sqrt(sigma_sq_nc)), n_obs, n_col))
    
  }
  return(NC)
}


one_data_sample <- function(n_col_nc, 
                            n_obs, 
                            sigma_sq_z, 
                            z_u_coef,
                            sigma_sq_nc, 
                            nc_u_coef,
                            with_cntrls, 
                            sigma_sq_cntrls = NULL,
                            z_cntrls_coef = NULL,
                            nc_cntrls_coef = NULL, 
                            nonlinear_weight, 
                            noise_type){
  
  #' @description sample Z, U, cntrls and NC data by user parameters choice
  #' @return list of Z, U, cntrls, NC 
  
  # Sample Z (IV), U (APV), and cntrls (Controls aka covariates)
  z_u_cntrls <- sample_z_u_cntrls(n_obs = n_obs, 
                                  sigma_sq_z = sigma_sq_z, 
                                  z_u_coef = z_u_coef,
                                  with_cntrls = with_cntrls,
                                  sigma_sq_cntrls = sigma_sq_cntrls, 
                                  cntrls_coef = z_cntrls_coef,
                                  nonlinear_weight = nonlinear_weight,
                                  noise_type = noise_type)
  
  Z <- z_u_cntrls$Z
  U <- z_u_cntrls$U
  cntrls <- z_u_cntrls$cntrls
  
  # Sample NCs
  NC <- sample_NC(n_col = n_col_nc,
                  sigma_sq_nc = sigma_sq_nc,
                  nc_u_coef = nc_u_coef,
                  U = U,
                  with_cntrls = with_cntrls,
                  cntrls = cntrls, 
                  cntrls_coef =  nc_cntrls_coef)
  
  return(list(Z = Z , U = U , cntrls = cntrls, NC = NC))
}


simulate_gauss_mix <- function(p, n_obs, sigma_1, sigma_2){
  #' @description Generate light (gaussian) or heavy (gaussian mixture) tailed noise
  
  sim_types <- rbinom(n = n_obs, size = 1, prob = p)
  
  gauss_mix <- (sim_types*rnorm(n_obs,0, sigma_1)) + ((1-sim_types)*rnorm(n_obs, 0, sigma_2))
  
  return(gauss_mix)
}

Sample_RC_data <- function(n_obs, nc.coef, n_nc){
  #' @description Sample data for the RC-testing simulation

  X <- runif(n_obs,0,1)
  pz <- 0.119 + 1.785*X - 1.534*X^2 + 0.597*X^3
  Z <- rbinom(n = n_obs,size = 1, prob = pz)
  U.V <- rmvnorm(n = n_obs, mean = c(0,0),sigma = matrix(c(1,0.527,0.527,1),2,2))
  U <- U.V[,1]
  V <- U.V[,2]
  ps.z <- 0.29*Z + 0.22*(1-Z) 
  T_ <- as.numeric(pnorm(V) <= ps.z)
  APV <- runif(n_obs,-3,3)
  Y <- log(129.7 + 1247.7*X - 2149*X^2 + 1515.7*X^3) + 1.2*T_ + U + APV
 
  NC <- as.data.frame(sapply(seq(n_nc), function(m){
       1.534*X + nc.coef*(0.119 - 1.534*X^2 + 0.597*X^3) + 0.3*APV + rnorm(n_obs,0,1)}))
  
  return(list(X=c(X),Z=Z,T_=T_,Y=Y,APV=APV,NC=NC))
}

