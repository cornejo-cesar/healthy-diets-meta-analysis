###################
# STUDY DESIGN   #
##################

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

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data %>%
  filter(!is.na(depr_z), income_level == "Low income") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), income_level == "Low income") %>%
  distinct(report_id, .keep_all = TRUE)



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

depr_re   <- escalc(measure="SMD", yi=depr_meta$g, vi=depr_meta$vi, slab=depr_meta$slab, data=depr_meta)
anx_re    <- escalc(measure="SMD", yi=anx_meta$g, vi=anx_meta$vi, slab=anx_meta$slab, data=anx_meta)



####################################
# 3. VARIANCE - COVARIANCE MATRIX  #
###################################

# Define rho value
rho <- 0.5

# Calculate the variance-covariance matrix
V_depr   <- vcalc(vi, cluster = pop_cohort_dataset_id, obs = effect_size_id, data = depr_re, rho = rho)


####################
# 6. FOREST PLOT  #
###################

# depression
# **1. Create Aggregated Meta-Analysis Results:**
depression_agg <- aggregate(depr_re, cluster = pop_cohort_dataset_id, V = V_depr, addk = TRUE)
depression_agg <- depression_agg %>%
  left_join(
    depr %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
depression_res <- rma(yi, vi, method = "REML", data = depression_agg, slab = sample_label, digits =3)
depression_res <- robust(depression_res, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
depression_res$n_final <- depression_agg$n_final

total_n <- sum(depression_res$n_final, na.rm = TRUE)
total_estimates <- sum(depression_agg$ki, na.rm = TRUE)

summary(depression_res)
total_n
total_estimates

# anxiety


anxiety_agg <- aggregate(anx_re, cluster = pop_cohort_dataset_id, V = vi, addk = TRUE)
anxiety_agg <- anxiety_agg %>%
  left_join(
    anx %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
anxiety_res <- rma(yi, vi, method = "REML", data = anxiety_agg, slab = sample_label, digits =3)
anxiety_res$n_final <-anxiety_agg$n_final

total_n <- sum(anxiety_res$n_final, na.rm = TRUE)
total_estimates <- sum(anxiety_agg$ki, na.rm = TRUE)

summary(anxiety_res)
total_n
total_estimates

###############################################################################


##########################
# 2.  PREPARE THE DATA   #
#########################
data$income_level
# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data %>%
  filter(!is.na(depr_z), income_level == "Lower middle income") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), income_level == "Lower middle income") %>%
  distinct(report_id, .keep_all = TRUE)

stress <- data %>%
  filter(!is.na(stress_z), income_level == "Lower middle income") %>%
  distinct(report_id, .keep_all = TRUE)


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


####################
# 6. FOREST PLOT  #
###################

# depression
# **1. Create Aggregated Meta-Analysis Results:**
depression_agg <- aggregate(depr_re, cluster = pop_cohort_dataset_id, V = V_depr, addk = TRUE)
depression_agg <- depression_agg %>%
  left_join(
    depr %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
depression_res <- rma(yi, vi, method = "REML", data = depression_agg, slab = sample_label, digits =3)
depression_res <- robust(depression_res, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
depression_res$n_final <- depression_agg$n_final

total_n <- sum(depression_res$n_final, na.rm = TRUE)
total_estimates <- sum(depression_agg$ki, na.rm = TRUE)

summary(depression_res)
total_n
total_estimates

# anxiety


anxiety_agg <- aggregate(anx_re, cluster = pop_cohort_dataset_id, V = V_anx, addk = TRUE)
anxiety_agg <- anxiety_agg %>%
  left_join(
    anx %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
anxiety_res <- rma(yi, vi, method = "REML", data = anxiety_agg, slab = sample_label, digits =3)
anxiety_res <- robust(anxiety_res, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
anxiety_res$n_final <-anxiety_agg$n_final

total_n <- sum(anxiety_res$n_final, na.rm = TRUE)
total_estimates <- sum(anxiety_agg$ki, na.rm = TRUE)

summary(anxiety_res)
total_n
total_estimates

# stress


stress_agg <- aggregate(stress_re, cluster = pop_cohort_dataset_id, V = V_stress, addk = TRUE)
stress_agg <- stress_agg %>%
  left_join(
    stress %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
stress_res <- rma(yi, vi, method = "REML", data = stress_agg, slab = sample_label, digits =3)
stress_res <- robust(stress_res, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
stress_res$n_final <-stress_agg$n_final

total_n <- sum(stress_res$n_final, na.rm = TRUE)
total_estimates <- sum(stress_agg$ki, na.rm = TRUE)

summary(stress_res)

total_n
total_estimates


#############################################################################

##########################
# 2.  PREPARE THE DATA   #
#########################
data$income_level
# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data %>%
  filter(!is.na(depr_z), income_level == "Upper middle income") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), income_level == "Upper middle income") %>%
  distinct(report_id, .keep_all = TRUE)

stress <- data %>%
  filter(!is.na(stress_z), income_level == "Upper middle income") %>%
  distinct(report_id, .keep_all = TRUE)




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


####################
# 6. FOREST PLOT  #
###################

# depression
# **1. Create Aggregated Meta-Analysis Results:**
depression_agg <- aggregate(depr_re, cluster = pop_cohort_dataset_id, V = V_depr, addk = TRUE)
depression_agg <- depression_agg %>%
  left_join(
    depr %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
depression_res <- rma(yi, vi, method = "REML", data = depression_agg, slab = sample_label, digits =3)
depression_res <- robust(depression_res, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
depression_res$n_final <- depression_agg$n_final

total_n <- sum(depression_res$n_final, na.rm = TRUE)
total_estimates <- sum(depression_agg$ki, na.rm = TRUE)

summary(depression_res)
total_n
total_estimates

# anxiety


anxiety_agg <- aggregate(anx_re, cluster = pop_cohort_dataset_id, V = V_anx, addk = TRUE)
anxiety_agg <- anxiety_agg %>%
  left_join(
    anx %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
anxiety_res <- rma(yi, vi, method = "REML", data = anxiety_agg, slab = sample_label, digits =3)
anxiety_res <- robust(anxiety_res, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
anxiety_res$n_final <-anxiety_agg$n_final

total_n <- sum(anxiety_res$n_final, na.rm = TRUE)
total_estimates <- sum(anxiety_agg$ki, na.rm = TRUE)

summary(anxiety_res)
total_n
total_estimates

# stress


stress_agg <- aggregate(stress_re, cluster = pop_cohort_dataset_id, V = V_stress, addk = TRUE)
stress_agg <- stress_agg %>%
  left_join(
    stress %>%
      group_by(pop_cohort_dataset_id) %>%
      summarize(
        n_final = case_when(
          pop_cohort_dataset_id[1] == 155 ~ sum(n_final, na.rm = TRUE),
          TRUE ~ max(n_final, na.rm = TRUE)
        )
      ),
    by = "pop_cohort_dataset_id"
  )


# **2. Fit Random-Effects Models on Aggregated Data:**
stress_res <- rma(yi, vi, method = "REML", data = stress_agg, slab = sample_label, digits =3)
stress_res <- robust(stress_res, cluster = pop_cohort_dataset_id, clubSandwich = TRUE, digits = 3)
stress_res$n_final <-stress_agg$n_final

total_n <- sum(stress_res$n_final, na.rm = TRUE)
total_estimates <- sum(stress_agg$ki, na.rm = TRUE)

summary(stress_res)

total_n
total_estimates



#########################
#   FOREST PLOT DEPR
########################



income_results <- data.frame(
  income_group = c("Low income", "Lower middle income", "Upper middle income"),
  estimate = c(-0.069, -0.393, -0.281),
  se = c(0.091, 0.141, 0.030),
  ci_lb = c(-0.475, -0.712, -0.342),
  ci_ub = c(0.337, -0.073, -0.219),
  total_n = c(8319, 15393, 455999),
  total_estimates = c(3, 10, 57)
)


table_text <- rbind(
  c("Income level", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = income_results$income_group,
    "N. Estimates" = as.character(income_results$total_estimates),  
    "n" = as.character(income_results$total_n),
    Effect = format(round(income_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(income_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(income_results$ci_ub, 2), nsmall = 2), "]")
  )
)


png(file.path(output_path, "Subgroups/depr_income_level.png"), width = 1200, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, income_results$estimate),  
  lower = c(NA, income_results$ci_lb),    
  upper = c(NA, income_results$ci_ub),    
  zero = 0,                               
  xlog = FALSE,                           
  boxsize = 0.2,                          
  lineheight = "auto",                   
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",          
  lwd.ci = 2,                             
  title = paste0("Effect Sizes by income level - Depression\nTotal Studies Included: ", sum(income_results$total_estimates)),
  graph.pos = 2
)

dev.off()



#########################
#   FOREST PLOT ANX
########################



anxiety_income_results <- data.frame(
  income_group = c("Low income", "Lower middle income", "Upper middle income"),
  estimate = c(-0.979, -0.474, -0.197),
  se = c(0.127, 0.282, 0.034),
  ci_lb = c(-1.228, -1.374, -0.267),
  ci_ub = c(-0.729, 0.425, -0.128),
  total_n = c(277, 1914, 144026),
  total_estimates = c(1, 4, 38)
)


table_text <- rbind(
  c("Income level", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = anxiety_income_results$income_group,
    "N. Estimates" = as.character(anxiety_income_results$total_estimates),  # N. Estimates column
    "n" = as.character(anxiety_income_results$total_n),
    Effect = format(round(anxiety_income_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(anxiety_income_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(anxiety_income_results$ci_ub, 2), nsmall = 2), "]")
  )
)


png(file.path(output_path, "Subgroups/anx_income_level.png"), width = 1200, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, income_results$estimate),  # Space for title and values
  lower = c(NA, income_results$ci_lb),    # Space for title and values
  upper = c(NA, income_results$ci_ub),    # Space for title and values
  zero = 0,                               # Null effect line
  xlog = FALSE,                           # Do not transform x-axis
  boxsize = 0.2,                          # Box size
  lineheight = "auto",                    # Automatic line height
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",          # X-axis label
  lwd.ci = 2,                             # Line width for confidence intervals
  title = paste0("Effect Sizes by income level - Anxiety\nTotal Studies Included: ", sum(anxiety_income_results$total_estimates)),
  graph.pos = 2
)

dev.off()


#########################
#   FOREST PLOT STRESS
########################


stress_income_results <- data.frame(
  income_group = c("Lower middle income", "Upper middle income"),
  estimate = c(-0.048, -0.260),
  se = c(0.021, 0.048),
  ci_lb = c(-0.313, -0.362),
  ci_ub = c(0.216, -0.159),
  total_n = c(3648, 40924),
  total_estimates = c(2, 24)
)


table_text <- rbind(
  c("Income level", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = stress_income_results$income_group,
    "N. Estimates" = as.character(stress_income_results$total_estimates),  # N. Estimates column
    "n" = as.character(stress_income_results$total_n),
    Effect = format(round(stress_income_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(stress_income_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(stress_income_results$ci_ub, 2), nsmall = 2), "]")
  )
)


png(file.path(output_path, "Subgroups/stress_income_level.png"), width = 1200, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, stress_income_results$estimate),  # Space for title and values
  lower = c(NA, stress_income_results$ci_lb),    # Space for title and values
  upper = c(NA, stress_income_results$ci_ub),    # Space for title and values
  zero = 0,                                     # Null effect line
  xlog = FALSE,                                 # Do not transform x-axis
  boxsize = 0.2,                                # Box size
  lineheight = "auto",                          # Automatic line height
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",                # X-axis label
  lwd.ci = 2,                                   # Line width for confidence intervals
  title = paste0("Effect Sizes by income level - Stress\nTotal Studies Included: ", sum(stress_income_results$total_estimates)),
  graph.pos = 2
)

dev.off()
