###############################
# 1.  LOAD LIBRARIES AND DATA #
##############################

# Load libraries
library(readxl)
library(metafor)
library(clubSandwich)
library(dmetar)
library(dplyr)

##########################
# 2.  PREPARE THE DATA   #
#########################

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr   <- subset(data, !is.na(depr_z) & unique_statistic == 1)
anx    <- subset(data, !is.na(anx_z) & unique_statistic == 1)
stress <- subset(data, !is.na(stress_z) & unique_statistic == 1)


# Create a data frame for meta-analysis - Depression
depr_meta <- data.frame(
  slab                  = depr$author_year,                                     # Study label (author and year)
  n                     = depr$n,                                               # Sample size
  sample_n              = depr$n_final,                                         # sample cohort
  effect_size_id        = depr$effect_size_id,                                  # Effect size identifier
  pop_cohort_dataset_id = depr$pop_cohort_dataset_id,                           # Population cohort/dataset identifier
  sample_label          = depr$sample_label,
  r                     = depr$depr_r                                           # Correlation coefficient
)                                                                              
depr_meta$z <- atanh(depr_meta$r)                                               # Fisher's z transformation
depr_meta$se <- 1 / sqrt(depr_meta$n - 3)                                       # Standard error of Fisher's z


# Create a data frame for meta-analysis - Depression
anx_meta <- data.frame(
  slab                  = anx$author_year,                                      # Study label (author and year)
  n                     = anx$n,                                                # Sample size
  sample_n              = anx$n_final,                                          # sample cohort
  effect_size_id        = anx$effect_size_id,                                   # Effect size identifier
  pop_cohort_dataset_id = anx$pop_cohort_dataset_id,                            # Population cohort/dataset identifier
  sample_label          = anx$sample_label,
  r                     = anx$anx_r                                             # Correlation coefficient
)                                                                              
anx_meta$z <- atanh(anx_meta$r)                                                 # Fisher's z transformation
anx_meta$se <- 1 / sqrt(anx_meta$n - 3)                                         # Standard error of Fisher's z


# Create a data frame for meta-analysis - Depression
stress_meta <- data.frame(
  slab                  = stress$author_year,                                   # Study label (author and year)
  n                     = stress$n,                                             # Sample size
  sample_n              = stress$n_final,                                       # sample cohort
  effect_size_id        = stress$effect_size_id,                                # Effect size identifier
  pop_cohort_dataset_id = stress$pop_cohort_dataset_id,                         # Population cohort/dataset identifier
  sample_label          = stress$sample_label,
  r                     = stress$stress_r                                       # Correlation coefficient  
)                                                                               
stress_meta$z <- atanh(stress_meta$r)                                           # Fisher's z transformation
stress_meta$se <- 1 / sqrt(stress_meta$n - 3)                                   # Standard error of Fisher's z


depr_re    <- escalc("ZCOR", ri=z, ni=n, slab=slab, data=depr_meta)
anx_re     <- escalc("ZCOR", ri=z, ni=n, slab=slab, data=anx_meta)
stress_re  <- escalc("ZCOR", ri=z, ni=n, slab=slab, data=stress_meta)


####################################
# 3. VARIANCE - COVARIANCE MATRIX  #
###################################

# Define rho value
rho <- 0.5

# Calculate the variance-covariance matrix
V_depr   <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = effect_size_id, data = depr_re, rho = rho)
V_anx    <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = effect_size_id, data = anx_re, rho = rho)
V_stress <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = effect_size_id, data = stress_re, rho = rho)

###########################################################
# 4. META-ANALYSIS WITH CHE & ROBUST VARIANCE ESTIMATION  #
##########################################################

res_depr   <- rma.mv(yi, V_depr, 
                     random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                     data = depr_re, 
                     method = "REML",
                     test = 't',
                     dfs = 'contain')

res_anx    <- rma.mv(yi, V_anx, 
                     random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                     data = anx_re, 
                     method = "REML",
                     test = 't',
                     dfs = 'contain')

res_stress <- rma.mv(yi, V_stress, 
                     random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                     data = stress_re, 
                     method = "REML",
                     test = 't',
                     dfs = 'contain')


depr_resultrobust_z   <- robust(res_depr, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
anx_resultrobust_z    <- robust(res_anx, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
stress_resultrobust_z <- robust(res_stress, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)

depr_resultrobust_z
anx_resultrobust_z 
stress_resultrobust_z
###############################################
# 5. DISTRIBUTIONS OF VARIANCE ACROSS LEVELS  #
##############################################

i2 <- var.comp(res_depr)
summary(i2)
plot(i2)

i2 <- var.comp(res_anx)
summary(i2)
plot(i2)

i2 <- var.comp(res_stress)
summary(i2)
plot(i2)


print("Script '03_meta_analysis_z_fisher.R' ran successfully")