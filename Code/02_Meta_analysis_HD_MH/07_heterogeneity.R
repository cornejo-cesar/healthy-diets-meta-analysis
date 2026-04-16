# Load libraries
library(readxl)
library(metafor)
library(clubSandwich)
library(dmetar)
library(dplyr)
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

# Table with values only for depression, depriety, and depr
depr    <- subset(data, !is.na(depr_z) & unique_statistic == 1)

# Create a data frame for meta-analysis
depr_meta <- data.frame(
  slab                  = depr$author_year,                                      # Study label (author and year)
  n                     = depr$n,                                                # Sample size
  sample_n              = depr$n_final,                                          # sample cohort
  effect_size_id        = depr$effect_size_id,                                   # Effect size identifier
  pop_cohort_dataset_id = depr$pop_cohort_dataset_id,                            # Population cohort/dataset identifier
  sample_label          = depr$sample_label,
  g                     = depr$depr_g,
  overall               = depr$overall 
)                                                                               
depr_meta$se <- sqrt((4 / depr_meta$n) + (depr_meta$g^2 / (2 * (depr_meta$n - 2))))                                       
depr_meta$vi  <- depr_meta$se^2

depr_re    <- escalc(measure="SMD", yi=depr_meta$g, vi=depr_meta$vi, slab=depr_meta$slab, data=depr_meta)

##########################
# 2.  PREPARE THE DATA   #
#########################

# Table with values only for depression, anxiety, and stress
anx    <- subset(data, !is.na(anx_z) & unique_statistic == 1)

# Create a data frame for meta-analysis
anx_meta <- data.frame(
  slab                  = anx$author_year,                                      # Study label (author and year)
  n                     = anx$n,                                                # Sample size
  sample_n              = anx$n_final,                                          # sample cohort
  effect_size_id        = anx$effect_size_id,                                   # Effect size identifier
  pop_cohort_dataset_id = anx$pop_cohort_dataset_id,                            # Population cohort/dataset identifier
  sample_label          = anx$sample_label,
  g                     = anx$anx_g,
  overall               = anx$overall 
)                                                                               
anx_meta$se <- sqrt((4 / anx_meta$n) + (anx_meta$g^2 / (2 * (anx_meta$n - 2))))                                       
anx_meta$vi  <- anx_meta$se^2

anx_re    <- escalc(measure="SMD", yi=anx_meta$g, vi=anx_meta$vi, slab=anx_meta$slab, data=anx_meta)

##########################
# 2.  PREPARE THE DATA   #
#########################

# Table with values only for depression, stressiety, and stress
stress    <- subset(data, !is.na(stress_z) & unique_statistic == 1)

# Create a data frame for meta-analysis
stress_meta <- data.frame(
  slab                  = stress$author_year,                                      # Study label (author and year)
  n                     = stress$n,                                                # Sample size
  sample_n              = stress$n_final,                                          # sample cohort
  effect_size_id        = stress$effect_size_id,                                   # Effect size identifier
  pop_cohort_dataset_id = stress$pop_cohort_dataset_id,                            # Population cohort/dataset identifier
  sample_label          = stress$sample_label,
  g                     = stress$stress_g,
  overall               = stress$overall 
)                                                                               
stress_meta$se <- sqrt((4 / stress_meta$n) + (stress_meta$g^2 / (2 * (stress_meta$n - 2))))                                       
stress_meta$vi  <- stress_meta$se^2

stress_re    <- escalc(measure="SMD", yi=stress_meta$g, vi=stress_meta$vi, slab=stress_meta$slab, data=stress_meta)

####################################
# 3. VARIANCE - COVARIANCE MATRIX  #
###################################

# Define rho value
rho <- 0.5

# Calculate the variance-covariance matrix
V_depr    <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = effect_size_id, data = depr_re, rho = rho)
V_anx    <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = effect_size_id, data = anx_re, rho = rho)
V_stress    <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = effect_size_id, data = stress_re, rho = rho)


###########################################################
# 4. META-ANALYSIS WITH CHE & ROBUST VARIANCE ESTIMATION  #
##########################################################


res_depr   <- rma.mv(yi, V_depr, 
                     random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                     data = depr_re, 
                     method = "REML",
                     test = 't',
                     dfs = 'contain')
 
res_depr_low   <- rma.mv(yi, V_depr, 
                         random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                         data = depr_re, 
                         subset=(overall=="Low"),
                         method = "REML",
                         test = 't',
                         dfs = 'contain')

res_depr_high   <- rma.mv(yi, V_depr, 
                          random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                          subset=(overall=="High"),
                          data = depr_re, 
                          method = "REML",
                          test = 't',
                          dfs = 'contain')

depr_resultrobust_g      <- robust(res_depr, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
depr_resultrobust_g_low  <- robust(res_depr_low, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
depr_resultrobust_g_high <- robust(res_depr_high, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)

#####


res_anx   <- rma.mv(yi, V_anx, 
                    random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                    data = anx_re, 
                    method = "REML",
                    test = 't',
                    dfs = 'contain')

res_anx_low   <- rma.mv(yi, V_anx, 
                        random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                        data = anx_re, 
                        subset=(overall=="Low"),
                        method = "REML",
                        test = 't',
                        dfs = 'contain')

res_anx_high   <- rma.mv(yi, V_anx, 
                         random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                         subset=(overall=="High"),
                         data = anx_re, 
                         method = "REML",
                         test = 't',
                         dfs = 'contain')

anx_resultrobust_g      <- robust(res_anx, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
anx_resultrobust_g_low  <- robust(res_anx_low, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
anx_resultrobust_g_high <- robust(res_anx_high, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)

#### stress


res_stress   <- rma.mv(yi, V_stress, 
                       random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                       data = stress_re, 
                       method = "REML",
                       test = 't',
                       dfs = 'contain')

res_stress_low   <- rma.mv(yi, V_stress, 
                           random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                           data = stress_re, 
                           subset=(overall=="Low"),
                           method = "REML",
                           test = 't',
                           dfs = 'contain')

res_stress_high   <- rma.mv(yi, V_stress, 
                            random = ~ 1 | pop_cohort_dataset_id/effect_size_id, 
                            subset=(overall=="High"),
                            data = stress_re, 
                            method = "REML",
                            test = 't',
                            dfs = 'contain')

stress_resultrobust_g      <- robust(res_stress, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
stress_resultrobust_g_low  <- robust(res_stress_low, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
stress_resultrobust_g_high <- robust(res_stress_high, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)


k <- length(res_stress$yi)
png(file.path(output_path, "forest_tall.png") , width = 2200, height = 3600, res = 300)

forest(res_stress,
       cex=0.85,
       ylim=c(-37, k+4),
       spacing = 2,
       mlab=expression(bold("Random-Effects Model")))


pred0_depr <- predict(res_depr)
P_d_gt_0 <- round(pnorm(0, mean=pred0_depr$pred, sd=pred0_depr$pi.se, lower.tail=FALSE), digits=2)
P_d_gt_0

pred1_depr <- predict(res_depr_low)
P_d_gt_1 <- round(pnorm(0, mean=pred1_depr$pred, sd=pred1_depr$pi.se, lower.tail=FALSE), digits=2)
P_d_gt_1

pred2_depr <- predict(res_depr_high)
P_d_gt_2 <- round(pnorm(0, mean=pred2_depr$pred, sd=pred2_depr$pi.se, lower.tail=FALSE), digits=2)
P_d_gt_2

pred0_anx <- predict(res_anx)
P_a_gt_0 <- round(pnorm(0, mean=pred0_anx$pred, sd=pred0_anx$pi.se, lower.tail=FALSE), digits=2)
P_a_gt_0 

pred1_anx <- predict(res_anx_low)
P_a_gt_1 <- round(pnorm(0, mean=pred1_anx$pred, sd=pred1_anx$pi.se, lower.tail=FALSE), digits=2)
P_a_gt_1

pred2_anx <- predict(res_anx_high)
P_a_gt_2 <- round(pnorm(0, mean=pred2_anx$pred, sd=pred2_anx$pi.se, lower.tail=FALSE), digits=2)
P_a_gt_2

pred0_stress <- predict(res_stress)
P_s_gt_0 <- round(pnorm(0, mean=pred0_stress$pred, sd=pred0_stress$pi.se, lower.tail=FALSE), digits=2)
P_s_gt_0 

pred1_stress <- predict(res_stress_low)
P_s_gt_1 <- round(pnorm(0, mean=pred1_stress$pred, sd=pred1_stress$pi.se, lower.tail=FALSE), digits=2)
P_s_gt_1

pred2_stress <- predict(res_stress_high)
P_s_gt_2 <- round(pnorm(0, mean=pred2_stress$pred, sd=pred2_stress$pi.se, lower.tail=FALSE), digits=2)
P_s_gt_2


addpoly(pred0_depr, rows=-3, mlab=expression(bold(" ")), predstyle="dist")
addpoly(pred2_depr, rows=-7, mlab=expression(bold(" " )), predstyle="dist")
addpoly(pred1_depr, rows=-11, mlab=expression(bold(" ")),  predstyle="dist")

addpoly(pred0_anx, rows=-15, mlab=expression(bold(" ")), predstyle="dist")
addpoly(pred2_anx, rows=-19, mlab=expression(bold(" ")), predstyle="dist")
addpoly(pred1_anx, rows=-23, mlab=expression(bold(" ")),  predstyle="dist")

addpoly(pred0_stress, rows=-27, mlab=expression(bold(" ")), predstyle="dist")
addpoly(pred2_stress, rows=-31, mlab=expression(bold(" ")), predstyle="dist")
addpoly(pred1_stress, rows=-35, mlab=expression(bold(" ")),  predstyle="dist")

dev.off()