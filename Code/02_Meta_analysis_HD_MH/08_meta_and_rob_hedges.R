##########################
# 1.  LOAD LIBRARIES     #
#########################


# Load libraries
library(readxl)
library(metafor)
library(dplyr)
library(magrittr)
library(xtable)
library(dplyr)
library(forester)
library(extrafont)
loadfonts(device = "win")
windowsFonts("Fira Sans" = windowsFont("Fira Sans"))

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

# Functions to create Risk of bias
source(file.path(functions_path, "functions.R"))
source(file.path(functions_path, "rob_blobbogram.R"))


################################
# 2. ROB AND META DATA        #
###############################


rob_depr <- risk_of_bias(depr, "Depression")
rob_anx <- risk_of_bias(anx, "Anxiety")
rob_stress <- risk_of_bias(stress, "Stress")


################################
# 3. MULTILEVEL META-ANALYSIS  #
###############################


# Create a data frame for meta-analysis - Depression
depr_meta <- data.frame(
  slab                  = depr$author_year,                                     # Study label (author and year)
  n                     = depr$n,                                               # Sample size
  sample_n              = depr$n_final,                                         # sample cohort
  report_id             = depr$effect_size_id,                                  # Effect size identifier
  pop_cohort_dataset_id = depr$pop_cohort_dataset_id,                           # Population cohort/dataset identifier
  sample_label          = depr$sample_label,
  g                    = depr$depr_g                                           
)                                                                                                                          
depr_meta$se <- sqrt((4 / depr_meta$n) + (depr_meta$g^2 / (2 * (depr_meta$n - 2))))                                  
depr_meta$vi <- depr_meta$se^2

# Create a data frame for meta-analysis - Depression
anx_meta <- data.frame(
  slab                  = anx$author_year,                                      # Study label (author and year)
  n                     = anx$n,                                                # Sample size
  sample_n              = anx$n_final,                                          # sample cohort
  report_id             = anx$effect_size_id,                                   # Effect size identifier
  pop_cohort_dataset_id = anx$pop_cohort_dataset_id,                            # Population cohort/dataset identifier
  sample_label          = anx$sample_label,
  g                     = anx$anx_g                                             
)                                                                               
anx_meta$se <- sqrt((4 / anx_meta$n) + (anx_meta$g^2 / (2 * (anx_meta$n - 2))))                                       
anx_meta$vi  <- anx_meta$se^2

# Create a data frame for meta-analysis - Depression
stress_meta <- data.frame(
  slab                  = stress$author_year,                                   # Study label (author and year)
  n                     = stress$n,                                             # Sample size
  sample_n              = stress$n_final,                                       # sample cohort
  report_id             = stress$effect_size_id,                                # Effect size identifier
  pop_cohort_dataset_id = stress$pop_cohort_dataset_id,                         # Population cohort/dataset identifier
  sample_label          = stress$sample_label,
  g                     = stress$stress_g                                       
)                                                                               
stress_meta$se <- sqrt((4 / stress_meta$n) + (stress_meta$g^2 / (2 * (stress_meta$n - 2))))          
stress_meta$vi <- stress_meta$se^2

depr_re   <- escalc(measure="SMD", yi=depr_meta$g, vi=depr_meta$vi, slab=depr_meta$slab, data=depr_meta)
anx_re    <- escalc(measure="SMD", yi=anx_meta$g, vi=anx_meta$vi, slab=anx_meta$slab, data=anx_meta)
stress_re <- escalc(measure="SMD", yi=stress_meta$g, vi=stress_meta$vi, slab=stress_meta$slab, data=stress_meta)


####################################
# 3. VARIANCE - COVARIANCE MATRIX  #
###################################

# Define rho value
rho <- 0.5

# Calculate the variance-covariance matrix
V_depr   <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = report_id, data = depr_re, rho = rho)
V_anx    <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = report_id, data = anx_re, rho = rho)
V_stress <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = report_id, data = stress_re, rho = rho)

###########################################################
# 4. META-ANALYSIS WITH CHE & ROBUST VARIANCE ESTIMATION  #
##########################################################


res_depr   <- rma.mv(yi, vi, 
                     random = ~ 1 | pop_cohort_dataset_id/report_id, 
                     data = depr_re, 
                     method = "REML",
                     test = 't',
                     dfs = 'contain')

res_depr_robust   <- robust(res_depr, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
summary(res_depr)
summary(res_depr_robust)


res_anx    <- rma.mv(yi, vi, 
                     random = ~ 1 | pop_cohort_dataset_id/report_id, 
                     data = anx_re, 
                     method = "REML",
                     test = 't',
                     dfs = 'contain')

res_anx_robust    <- robust(res_anx, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
summary(res_anx)
summary(res_anx_robust)

res_stress <- rma.mv(yi, vi, 
                     random = ~ 1 | pop_cohort_dataset_id/report_id, 
                     data = stress_re, 
                     method = "REML",
                     test = 't',
                     dfs = 'contain')

res_stress_robust <- robust(res_stress, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
summary(res_stress)
summary(res_stress_robust)


names(rob_depr)[names(rob_depr) == "overall"]     <- "Overall"
names(rob_anx)[names(rob_anx) == "overall"]       <- "Overall"
names(rob_stress)[names(rob_stress) == "overall"] <- "Overall"
names(rob_anx) # verify  RoB Group


#rstudioapi::getThemeInfo()


res_depr_robust
res_anx_robust
res_stress_robust

####################
# 4. FOREST PLOT
###################

source(file.path(functions_path, "rob_blobbogram_3.R"))

get_colour("QUIPS", "cochrane")

forest_depr <- rob_blobbogram(
  rma                = res_depr_robust,
  rob                = rob_depr,
  rob_tool           = "QUIPS",
  subset_col         = "Overall",
  x_scale_log        = TRUE,                        
  x_breaks           = c(0.01, 0.1, 1, 10, 100),      
  x_labels           = c("0.01", "0.1", "1", "10", "100"),
  RoB_Group_estimate = TRUE,
  add_tests          = TRUE,
  arrow_labels       = c("Lower", "Higher"),
  estimate_col_name  = "SMD (95% CI)",
  dpi                = 600,
  file_path          = file.path(output_path, "Meta and rob/depr_g.png")
)



forest_anx <- rob_blobbogram(
  rma                = res_anx_robust,
  rob                = rob_anx,
  rob_tool           = "QUIPS",         
  subset_col         = "Overall",    
  x_scale_log        = TRUE,                        
  x_breaks           = c(0.01, 0.1, 1, 10, 100),      
  x_labels           = c("0.01", "0.1", "1", "10", "100"),
  RoB_Group_estimate = TRUE,
  add_tests          = TRUE,
  arrow_labels       = c("Lower", "Higher"),
  estimate_col_name  = "SMD (95% CI)",
  dpi                = 600,
  file_path          = file.path(output_path, "Meta and rob/anx_g.png")
) 


forest_stress <- rob_blobbogram(
  rma                = res_stress_robust,
  rob                = rob_stress,
  rob_tool           = "QUIPS",        
  subset_col         = "Overall",    
  x_scale_log        = TRUE,                        
  x_breaks           = c(0.01, 0.1, 1, 10, 100),      
  x_labels           = c("0.01", "0.1", "1", "10", "100"),
  RoB_Group_estimate = TRUE,
  add_tests          = TRUE,
  arrow_labels       = c("Lower", "Higher"),
  estimate_col_name  = "SMD (95% CI)",
  dpi                = 600,
  file_path          = file.path(output_path, "Meta and rob/stress_g.png")
) 
