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
  filter(!is.na(depr_z), exposure_group == "G1- Adherence and adequacy") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), exposure_group == "G1- Adherence and adequacy") %>%
  distinct(report_id, .keep_all = TRUE)

stress <- data %>%
  filter(!is.na(stress_z), exposure_group == "G1- Adherence and adequacy") %>%
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
anxiety_res$n_final <-anxiety_agg$n_final

total_n <- sum(anxiety_res$n_final, na.rm = TRUE)
total_estimates <- sum(anxiety_agg$ki, na.rm = TRUE)

summary(anxiety_res)
total_n
total_estimates

# stress


stress_agg <- aggregate(stress_re, cluster = pop_cohort_dataset_id, V = vi, addk = TRUE)
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
stress_res$n_final <-stress_agg$n_final

total_n <- sum(stress_res$n_final, na.rm = TRUE)
total_estimates <- sum(stress_agg$ki, na.rm = TRUE)

summary(stress_res)

##############################################################################################

# Create a combined table with the results
results_table <- data.frame(
  Condition = rep(c("Depression", "Anxiety", "Stress"), 
                  c(nrow(depression_res$data), nrow(anxiety_res$data), nrow(stress_res$data))),
  Study = c(depression_res$data$slab, anxiety_res$data$slab, stress_res$data$slab), 
  g = c(depression_res$data$g, anxiety_res$data$g, stress_res$data$g),  
  lower_CI = c(depression_res$data$g - 1.96 * sqrt(depression_res$data$vi),
               anxiety_res$data$g - 1.96 * sqrt(anxiety_res$data$vi),
               stress_res$data$g - 1.96 * sqrt(stress_res$data$vi)),   
  upper_CI = c(depression_res$data$g + 1.96 * sqrt(depression_res$data$vi),
               anxiety_res$data$g + 1.96 * sqrt(anxiety_res$data$vi),
               stress_res$data$g + 1.96 * sqrt(stress_res$data$vi))   
)


# Show the table
print(results_table)



##############################################################################################

##########################
# 2.  PREPARE THE DATA   #
#########################

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data %>%
  filter(!is.na(depr_z), exposure_group == "G2- Dietary pattern (NRCD reducing)") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), exposure_group == "G2- Dietary pattern (NRCD reducing)") %>%
  distinct(report_id, .keep_all = TRUE)

stress <- data %>%
  filter(!is.na(stress_z), exposure_group == "G2- Dietary pattern (NRCD reducing)") %>%
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


############################################################################################
##########################
# 2.  PREPARE THE DATA   #
#########################

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data %>%
  filter(!is.na(depr_z), exposure_group == "G3- Diet diversity indices") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), exposure_group == "G3- Diet diversity indices") %>%
  distinct(report_id, .keep_all = TRUE)

stress <- data %>%
  filter(!is.na(stress_z), exposure_group == "G3- Diet diversity indices") %>%
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


stress_agg <- aggregate(stress_re, cluster = pop_cohort_dataset_id, V = vi, addk = TRUE)
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
stress_res$n_final <-stress_agg$n_final

total_n <- sum(stress_res$n_final, na.rm = TRUE)
total_estimates <- sum(stress_agg$ki, na.rm = TRUE)

summary(stress_res)

total_n
total_estimates



##############################################################################################

##########################
# 2.  PREPARE THE DATA   #
#########################

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data %>%
  filter(!is.na(depr_z), exposure_group == "G4- Diet quality indices") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), exposure_group == "G4- Diet quality indices") %>%
  distinct(report_id, .keep_all = TRUE)

stress <- data %>%
  filter(!is.na(stress_z), exposure_group == "G4- Diet quality indices") %>%
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


##############################################################################################


##########################
# 2.  PREPARE THE DATA   #
#########################

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data %>%
  filter(!is.na(depr_z), exposure_group == "G5- Factor analysis and others") %>%
  distinct(report_id, .keep_all = TRUE)

anx <- data %>%
  filter(!is.na(anx_z), exposure_group == "G5- Factor analysis and others") %>%
  distinct(report_id, .keep_all = TRUE)

stress <- data %>%
  filter(!is.na(stress_z), exposure_group == "G5- Factor analysis and others") %>%
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


####################
#  FOREST   depression
########################

library(forestplot)

# Create the dataframe with group results
study_design_results <- data.frame(
  study_design = c("G1: Adherence and adequacy", "G2: Dietary pattern (NRCD reducing)", 
                   "G3: Diet diversity indices", "G4: Diet quality indices", "G5: Factor analysis and others"),
  estimate = c(-0.116, -0.358, -0.175, -0.297, -0.322),
  se = c(0.029, 0.068, 0.049, 0.092, 0.034),
  ci_lb = c(-0.173, -0.505, -0.284, -0.495, -0.393),
  ci_ub = c(-0.059, -0.212, -0.067, -0.099, -0.250),
  total_n = c(3846, 16906, 191098, 144643, 141117),
  total_estimates = c(2, 18, 12, 15, 22)
)


table_text <- rbind(
  c("Dietary measurement", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = study_design_results$study_design,
    "N. Estimates" = as.character(study_design_results$total_estimates),  # N. Estimates column
    "n" = as.character(study_design_results$total_n),
    Effect = format(round(study_design_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(study_design_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(study_design_results$ci_ub, 2), nsmall = 2), "]")
  )
)

# Create the forest plot
png(file.path(output_path, "Subgroups/depr_dietary_measurement.png"), width = 1400, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, study_design_results$estimate),  # Space for title and values
  lower = c(NA, study_design_results$ci_lb),    
  upper = c(NA, study_design_results$ci_ub),   
  zero = 0,                                     # Null effect line
  xlog = FALSE,                                 # Do not transform x-axis
  boxsize = 0.2,                                # Box size
  lineheight = "auto",                          # Automatic line height
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",                # X-axis label
  lwd.ci = 2,                                   # Line width for confidence intervals
  title = paste0("Effect Sizes by dietary measurements - Depression\nTotal Studies Included: ", sum(study_design_results$total_estimates)),
  graph.pos = 2
)

dev.off()


####################
#  FOREST   ANX
########################


study_design_results <- data.frame(
  study_design = c("G1: Adherence and adequacy", "G2: Dietary pattern (NRCD reducing)", 
                   "G3: Diet diversity indices", "G4: Diet quality indices", "G5: Factor analysis and others"),
  estimate = c(-0.196, -0.248, -0.331, -0.315, -0.195),
  se = c(0.029, 0.068, 0.161, 0.120, 0.036),
  ci_lb = c(-0.253, -0.406, -0.778, -0.587, -0.277),
  ci_ub = c(-0.138, -0.090, 0.116, -0.044, -0.112),
  total_n = c(3846, 14923, 24077, 100722, 41933),
  total_estimates = c(2, 14, 5, 10, 12)
)


table_text <- rbind(
  c("Dietary measurement", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = study_design_results$study_design,
    "N. Estimates" = as.character(study_design_results$total_estimates),  # N. Estimates column
    "n" = as.character(study_design_results$total_n),
    Effect = format(round(study_design_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(study_design_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(study_design_results$ci_ub, 2), nsmall = 2), "]")
  )
)


png(file.path(output_path, "Subgroups/anx_dietary_measurement.png"), width = 1400, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, study_design_results$estimate),  
  lower = c(NA, study_design_results$ci_lb),    
  upper = c(NA, study_design_results$ci_ub),   
  zero = 0,                                     
  xlog = FALSE,                                 
  boxsize = 0.2,                               
  lineheight = "auto",                          
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",                
  lwd.ci = 2,                                  
  title = paste0("Effect Sizes by dietary measurements - Anxiety\nTotal Studies Included: ", sum(study_design_results$total_estimates)),
  graph.pos = 2
)

dev.off()

####################
# STRESS
#####################


study_design_results <- data.frame(
  study_design = c("G1: Adherence and adequacy", "G2: Dietary pattern (NRCD reducing)", 
                   "G3: Diet diversity indices", "G4: Diet quality indices", "G5: Factor analysis and others"),
  estimate = c(-0.282, -0.321, -0.151, -0.282, -0.170),
  se = c(0.036, 0.099, 0.106, 0.091, 0.050),
  ci_lb = c(-0.352, -0.555, -0.358, -0.572, -0.291),
  ci_ub = c(-0.212, -0.087, 0.056, 0.008, -0.049),
  total_n = c(3846, 14662, 360, 18417, 14979),
  total_estimates = c(1, 12, 1, 4, 8)
)


table_text <- rbind(
  c("Dietary measurement", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = study_design_results$study_design,
    "N. Estimates" = as.character(study_design_results$total_estimates),  # N. Estimates column
    "n" = as.character(study_design_results$total_n),
    Effect = format(round(study_design_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(study_design_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(study_design_results$ci_ub, 2), nsmall = 2), "]")
  )
)


png(file.path(output_path, "Subgroups/stress_dietary_measurement.png"), width = 1400, height = 800, res = 150)


forestplot(
  labeltext = table_text,
  mean = c(NA, study_design_results$estimate),  
  lower = c(NA, study_design_results$ci_lb),   
  upper = c(NA, study_design_results$ci_ub),  
  zero = 0,                                   
  xlog = FALSE,                                 
  boxsize = 0.2,                               
  lineheight = "auto",                          
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",               
  lwd.ci = 2,                                  
  title = paste0("Effect Sizes by dietary measurements - Stress\nTotal Studies Included: ", sum(study_design_results$total_estimates)),
  graph.pos = 2
)

dev.off()
