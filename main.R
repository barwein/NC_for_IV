###
# ``Negative Control Falsification Tests for Instrumental Variable Designs" 
# Run the data analysis
###


# Environment setup -------------------------------------------------
# Run this file to install all the required packages
# List of all required packages is in 'requirements.txt' file
# Two modes of installation: "strict" (exact versions) and "relaxed" (latest CRAN)
# To run:
#   1) source("main.R")
#   2) When prompted, choose:
#        [1] Strict (use exact versions from requirements.txt)
#        [2] Relaxed (ignore versions; install latest CRAN)

source("setup_env.R")

# Applications (data analyses) -----------------------------------------------------------

# ADH analysis
# Run time: ~ 15 minutes
source("Data_analysis/ADH/ADH_Run.R")

# Deming analysis
# Run time: ~ 15 minutes
source("Data_analysis/Deming/Deming_run.R")

# Ashraf & Galor analysis
# Run time < 1 minute
source("Data_analysis/AshrafGalor/AshrafGalor.R")

# Nunn & Qian analysis
# Run time: ~ 15 minutes
source("Data_analysis/NunnQian/NunnQian.R")

# Summarize literature
# Run time < 1 minute
source("Literature_survey/NC_literature_survey_summary.R")















