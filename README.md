# Negative Control Falsification Tests for Instrumental Variable Designs
Replication files for ``Negative Control Falsification Tests for Instrumental Variable Designs" simulations and applications (data analyses). See preprint at https://arxiv.org/abs/2312.15624.

This README file provides information on the data and code used to produce all applications and simulations results given in the paper.

## Software Requirements
The code is written in R and was run on an RStudio server with the following specs:

R version 4.2.2 

Platform: xx86_64-w64-mingw32

R packages: data.table, sandwich, ivreg, mvtnorm, foreach, parallel, dplyr, lfe, mgcv, caret, systemfit, sandwich, 
            aod, tidyverse, ggpubr, latex2exp, gridExtra, haven, fixest, mltools, stringr, kableExtra, lmtest,
            MASS, stargazer, ggplot2, readr

Aprroximate runtime: 1 hour (for data analysis); 24 hours (for simulations)


## Code Overview
The code consists of two main parts:

1. Data analyses.
2. Simulations study.

The data analyses, or applications, can run in a feasible runtime on a personal compute (~1 hour). The simulation study is more intensive and can take more than 24 hours to run it.

To obtain all the results run the *main.R* file. The code will replicate all the output files (csv, figures, and tables). 

The folder *Simulations* contain the files *NCO_simulations.R* and *RC_simulations.R* which run the NCO/NCE (linear and nonlinear DGP) and rich covariates simulations, respectively. The script *simulations_plots.R* generate the final figures.

The folder *Data_analysis* contain four additional folders, one for each data application. The relevant datasets are given in each folder (see details on availability below). Specifically,

1. *ADH* sub-folder contain the script *ADH_Init.R* which initilaize the data set for analysis and define aux functions. *ADH_Run.R* run the analysis and provide the results as tables and files.

2. *Deming* sub-folder, similarly to *ADH*, contain a *Deming_Init.R* and *Deming_run.R* files.

3. *AshrafGalor* sub-folder contains a single script *AshrafGalor.R* that perform the analysis and output the results as a table.

4. *NunnQian* sub-folder also contain a single script *NunnQian.R* that run the analysis and provide the results as table print output.


## Data Availability  
All data files used in the analyses are freely available from the authors, and can be obtained via the following links.

1. Autor, Dorn, Hanson (2013). https://www.openicpsr.org/openicpsr/project/112670/version/V1/view
2. Deming (2014). https://www.openicpsr.org/openicpsr/project/112805/version/V1/view
3. Ashraf & Galor (2013). https://www.openicpsr.org/openicpsr/project/112588/version/V1/view
4. Nunn & Qian (2014). https://www.openicpsr.org/openicpsr/project/112825/version/V1/view







