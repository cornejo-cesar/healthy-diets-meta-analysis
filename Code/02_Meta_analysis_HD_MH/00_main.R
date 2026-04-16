################################################################################
# 0_main.R
#
# Purpose: Master script to run all analyses for the Meta-Analysis 
#          
#
# Author: Cesar Cornejo
# Last Modified: 2025-02-14
################################################################################

# =======================
# 1. Define Folder Paths
# =======================

# Clear the workspace to avoid conflicts from previous sessions
rm(list=ls(all=TRUE))
# Turn off scientific notation for better readability
options(scipen = 999)
# Set the seed for reproducibility
set.seed(123)


# Define paths for data, scripts, and outputs

root          <-  "/Replication package"
data_path      <- file.path(root, "Datasets/Excel files") 
scripts_path   <- file.path(root, "Code/02_Meta_analysis_HD_MH") 
functions_path <- file.path(root, "Code/02_Meta_analysis_HD_MH/functions")
output_path    <- file.path(root, "Output") 
data_map       <- file.path(root, "Datasets/Maps") 

# =======================
# 2. Set Up Environment
# =======================

# Set working directory to the project root
setwd(scripts_path)

# Activate environment
renv::activate()
deps <- renv::dependencies()
print(deps)

# =======================
# 3. Load Required Packages
# =======================

# List of required packages
#required_packages <- c(
#  "rlang", "glue", "cli", "purrr", "curl", "Rcpp", "digest",
#  "readxl", "ggplot2", "dplyr", "metafor", "vctrs", "xtable",
#  "magrittr", "forestplot", "usethis", "extrafont", "clubSandwich"
#)
#renv::install(required_packages)

# Function to install and load packages
#renv::install("MuMIn@1.47.1")
#renv::install("MathiasHarrer/dmetar")
#renv::install("rdboyes/forester")

renv::status()
renv::snapshot()
renv::status()

# =======================
# 4. Run Data Preparation and Analysis Scripts
# =======================

# Descriptive Analysis
source(file.path(scripts_path, "01_descriptive_analysis.R"))
source(file.path(scripts_path, "02_map.R"))

# Meta-Analysis
source(file.path(scripts_path, "03_meta_analysis_z_fisher.R"))
source(file.path(scripts_path, "04_meta_analysis_hedges_g.R"))
source(file.path(scripts_path, "05_meta_analysis_d_cohen.R"))

source(file.path(scripts_path, "06_outliers_and_influence.R"))
source(file.path(scripts_path, "07_heterogeneity.R"))

# Risk of bias
source(file.path(scripts_path, "08_meta_and_rob_hedges.R"))
source(file.path(scripts_path, "10_traffic_light_summary_rob.R"))

# Subgroup Analyses
source(file.path(scripts_path, "11_meta_high_coufunding_g.R"))
source(file.path(scripts_path, "11_meta_low_coufunding_g.R"))
source(file.path(scripts_path, "12_meta_high_participation_g.R"))
source(file.path(scripts_path, "12_meta_low_participation_g.R"))
source(file.path(scripts_path, "13_study_design.R"))
source(file.path(scripts_path, "14_income_level.R"))
source(file.path(scripts_path, "15_effect_sizes_indices.R"))
source(file.path(scripts_path, "16_dietary_measurement.R"))



cat("All analyses have been successfully completed. Results are saved in the 'Outputs' folder.\n")



