###
# ``Negative Control Falsification Tests for Instrumental Variable Designs" 
# Run the data analysis
###


# Required libraries ------------------------------------------------------

require(data.table)
require(sandwich)
require(ivreg)
require(mvtnorm)
require(foreach)
require(parallel)
require(dplyr)
require(lfe)
require(mgcv)
require(caret)
require(systemfit)
require(sandwich)
require(aod)
require(tidyverse)
require(ggpubr)
require(latex2exp)
require(gridExtra)
require(haven)
require(fixest)
require(mltools)
require(stringr)
require(kableExtra)
require(lmtest)
require(MASS)
require(stargazer)
require(ggplot2)
require(readr)
require(assertthat)
require(Matrix)
require(janitor)
require(xtable)


# Applications (data analyses) -----------------------------------------------------------

# Run ADH analysis

source("Data_analysis/ADH/ADH_Run.R")

# Run Deming analysis

source("Data_analysis/Deming/Deming_run.R")

# Run Ashraf & Galor analysis

source("Data_analysis/AshrafGalor/AshrafGalor.R")

# Run Nunn & Qian analysis

source("Data_analysis/NunnQian/NunnQian.R")


# Simulations -------------------------------------------------------------

### IMPORTANT NOTE ###
# The simulation study was performed in a power-cluster
# The provided code may take a while to run -- please consider before running the entire file!

### May take a while! ###

# NCO and NCE simulations

# source("Simulations/NCO_simulations.R")

# Rich-covariates (RC) simulations

# source("Simulations/RC_simulations.R")

# Generate figures for the results

# source("Simulations/simulation_plots.R")



























