###############################
# 1.  LOAD LIBRARIES AND DATA #
##############################

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
#
# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr   <- subset(data, !is.na(depr_z) & unique_statistic == 1)
anx    <- subset(data, !is.na(anx_z) & unique_statistic == 1 )
stress <- subset(data, !is.na(stress_z) & unique_statistic == 1)


# Create a data frame for meta-analysis - Depression
depr_meta <- data.frame(
  slab                  = depr$author_year,                                     # Study label (author and year)
  n                     = depr$n,                                               # Sample size
  sample_n              = depr$n_final,                                         # sample cohort
  effect_size_id        = depr$effect_size_id,                                  # Effect size identifier
  pop_cohort_dataset_id = depr$pop_cohort_dataset_id,                           # Population cohort/dataset identifier
  sample_label          = depr$sample_label,
  g                     = depr$depr_g                                        
)                                                                                                                    
depr_meta$se <- sqrt((4 / depr_meta$n) + (depr_meta$g^2 / (2 * (depr_meta$n - 2))))                                  
depr_meta$vi <- depr_meta$se^2

# Create a data frame for meta-analysis - Depression
anx_meta <- data.frame(
  slab                  = anx$author_year,                                      # Study label (author and year)
  n                     = anx$n,                                                # Sample size
  sample_n              = anx$n_final,                                          # sample cohort
  effect_size_id        = anx$effect_size_id,                                   # Effect size identifier
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
  effect_size_id        = stress$effect_size_id,                                # Effect size identifier
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


depr_resultrobust_g   <- robust(res_depr, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
anx_resultrobust_g    <- robust(res_anx, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
stress_resultrobust_g <- robust(res_stress, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)

predict(depr_resultrobust_g)
predict(anx_resultrobust_g )
predict(stress_resultrobust_g)

table_depr <-coef(summary(depr_resultrobust_g))
table_anx <-coef(summary(anx_resultrobust_g))
table_stress <-coef(summary(stress_resultrobust_g))

summary(depr_resultrobust_g)
summary(anx_resultrobust_g)
summary(stress_resultrobust_g)


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

#####################################
# 6. DETECTION OF OUTLIERS  #
####################################


# studentized residuals
depr_re$stud_resid <- rstandard(res_depr)$z
anx_re$stud_resid <- rstandard(res_anx)$z
stress_re$stud_resid <- rstandard(res_stress)$z

# Bonferroni threshold for outlier detection
k_depr <- nrow(depr_re)
k_anx <- nrow(anx_re)
k_stress <- nrow(stress_re)

bonferroni_threshold_depr <- qnorm(1 - 0.05 / (2 * k_depr))
bonferroni_threshold_anx <- qnorm(1 - 0.05 / (2 * k_anx))
bonferroni_threshold_stress <- qnorm(1 - 0.05 / (2 * k_stress))

# Outlier identification
depr_re$outlier <- abs(depr_re$stud_resid) > bonferroni_threshold_depr
anx_re$outlier <- abs(anx_re$stud_resid) > bonferroni_threshold_anx
stress_re$outlier <- abs(stress_re$stud_resid) > bonferroni_threshold_stress

# Contar y reportar
cat("Depression Outliers:", sum(depr_re$outlier, na.rm = TRUE), "\n")
cat("Anxiety Outliers:", sum(anx_re$outlier, na.rm = TRUE), "\n")
cat("Stress Outliers:", sum(stress_re$outlier, na.rm = TRUE), "\n")


# Estudios identificados como outliers
outliers_depr <- depr_re[depr_re$outlier == TRUE, c("slab", "stud_resid")]
outliers_anx <- anx_re[anx_re$outlier == TRUE, c("slab", "stud_resid")]
outliers_stress <- stress_re[stress_re$outlier == TRUE, c("slab", "stud_resid")]

# Mostrar en consola
cat("Depression Outliers:\n")
print(outliers_depr)

cat("Anxiety Outliers:\n")
print(outliers_anx)

cat("Stress Outliers:\n")
print(outliers_stress)


#####################################
# 7. Checking for Influence #
####################################

# Compute Cook's Distance for each model
depr_re$cooksd <- cooks.distance(res_depr)
anx_re$cooksd <- cooks.distance(res_anx)
stress_re$cooksd <- cooks.distance(res_stress)

# Define the threshold based on the median + 6 times the interquartile range (IQR)
cooks_threshold_depr <- median(depr_re$cooksd, na.rm = TRUE) + 6 * IQR(depr_re$cooksd, na.rm = TRUE)
cooks_threshold_anx <- median(anx_re$cooksd, na.rm = TRUE) + 6 * IQR(anx_re$cooksd, na.rm = TRUE)
cooks_threshold_stress <- median(stress_re$cooksd, na.rm = TRUE) + 6 * IQR(stress_re$cooksd, na.rm = TRUE)

# Identify influential studies based on Cook's Distance threshold
depr_re$influential <- depr_re$cooksd > cooks_threshold_depr
anx_re$influential <- anx_re$cooksd > cooks_threshold_anx
stress_re$influential <- stress_re$cooksd > cooks_threshold_stress

# Print the number of influential studies for each outcome
cat("Depression Influential Studies:", sum(depr_re$influential, na.rm = TRUE), "\n")
cat("Anxiety Influential Studies:", sum(anx_re$influential, na.rm = TRUE), "\n")
cat("Stress Influential Studies:", sum(stress_re$influential, na.rm = TRUE), "\n")

# Extract and display the influential studies based on Cook's Distance
influential_depr <- depr_re[depr_re$influential == TRUE, c("slab", "cooksd")]
influential_anx <- anx_re[anx_re$influential == TRUE, c("slab", "cooksd")]
influential_stress <- stress_re[stress_re$influential == TRUE, c("slab", "cooksd")]

# Print the details of influential studies for each outcome
cat("Depression Influential Studies:\n")
print(influential_depr)

cat("Anxiety Influential Studies:\n")
print(influential_anx)

cat("Stress Influential Studies:\n")
print(influential_stress)


# Function to compute Cook’s Distance and generate plots
analyze_cooks_distance <- function(model, data, outcome_name) {
  
  # Compute Cook's Distance
  cooks <- cooks.distance(model)
  
  # Compute threshold (Median + 6*IQR)
  cooks_median <- median(cooks, na.rm = TRUE)
  cooks_iqr <- IQR(cooks, na.rm = TRUE)
  cooks_threshold <- cooks_median + 6 * cooks_iqr
  
  # Identify influential studies
  influential_studies <- which(cooks > cooks_threshold)
  
  cat("\n", outcome_name, "Influential Studies:", length(influential_studies), "\n")
  
  if (length(influential_studies) > 0) {
    print(data.frame(
      Study = data$slab[influential_studies],
      Cook_Distance = cooks[influential_studies]
    ))
  }
  
  # Generate Cook's Distance plot
  plot(cooks, type = "o", pch = 19,
       xlab = paste("Observed Outcome -", outcome_name),
       ylab = "Cook's Distance",
       main = paste("Cook's Distance Plot -", outcome_name))
  
  # Add threshold line
  abline(h = cooks_threshold, col = "red", lty = 2)
  
  # Add text labels for influential studies
  if (length(influential_studies) > 0) {
    text(influential_studies, cooks[influential_studies],
         labels = data$slab[influential_studies],
         pos = 3, cex = 0.7)
  }
}

# Apply function for each outcome
analyze_cooks_distance(res_depr, depr_re, "Depression")
analyze_cooks_distance(res_anx, anx_re, "Anxiety")
analyze_cooks_distance(res_stress, stress_re, "Stress")








