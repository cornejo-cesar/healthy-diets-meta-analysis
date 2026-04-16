##########################
# 1.  LOAD LIBRARIES     #
#########################

# Load libraries
library(readxl)
library(dplyr)
library(robvis)
library(ggplot2)

##########################
# 2.  PREPARE THE DATA   #
#########################

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Table with values only for depression, anxiety, and stress
depr <- data[!is.na(data$depr_z), ]
anx <- data[!is.na(data$anx_z), ]
stress <- data[!is.na(data$stress_z), ]

# Functions to create Risk of bias
source(file.path(functions_path, "functions.R"))

################################
# 2. ROB DATA                 #
###############################

rob_depr_with_overall      <- risk_of_bias(depr, "Depression", include_overall = TRUE)
rob_depr_without_overall   <- risk_of_bias(depr, "Depression", include_overall = FALSE)
rob_anx_with_overall       <- risk_of_bias(anx, "Anxiety", include_overall = TRUE)
rob_anx_without_overall    <- risk_of_bias(anx, "Anxiety", include_overall = FALSE)
rob_stress_with_overall    <- risk_of_bias(stress, "Stress", include_overall = TRUE)
rob_stress_without_overall <- risk_of_bias(stress, "Stress", include_overall = FALSE)

###########################################
# 3.  Risk of Bias Summary
############################################


# Depression

summary_plot <- rob_summary_quips_custom(
  data = rob_depr_with_overall,       
  overall = TRUE,        
  weighted = FALSE,      
  rob_colours = rob_colours  
) + 
  ggtitle("Summary of Risk of Bias - Depression") +
  theme(plot.title = element_text(hjust = 0.5))  

rob_save(summary_plot, file.path(output_path, paste0("Risk of bias/", "depr_summary_with_overall.png")))


summary_plot <- rob_summary_quips_custom(
  data = rob_depr_without_overall,       
  overall = FALSE,        
  weighted = FALSE,      
  rob_colours = rob_colours  
) + 
  ggtitle("Summary of Risk of Bias - Depression") +
  theme(plot.title = element_text(hjust = 0.5))  

rob_save(summary_plot, file.path(output_path, paste0("Risk of bias/", "depr_summary_without_overall.png")))


# Anxiety
summary_plot <- rob_summary_quips_custom(
  data = rob_anx_with_overall,       
  overall = TRUE,        
  weighted = FALSE,      
  rob_colours = rob_colours  
) + 
  ggtitle("Summary of Risk of Bias - Anxiety") +
  theme(plot.title = element_text(hjust = 0.5))  

rob_save(summary_plot, file.path(output_path, paste0("Risk of bias/", "anx_summary_with_overall.png")))


summary_plot <- rob_summary_quips_custom(
  data = rob_anx_without_overall,       
  overall = FALSE,        
  weighted = FALSE,      
  rob_colours = rob_colours  
) + 
  ggtitle("Summary of Risk of Bias - Anxiety") +
  theme(plot.title = element_text(hjust = 0.5))  

rob_save(summary_plot, file.path(output_path, paste0("Risk of bias/", "anx_summary_without_overall.png")))

# Stress
summary_plot <- rob_summary_quips_custom(
  data = rob_stress_with_overall,       
  overall = TRUE,        
  weighted = FALSE,      
  rob_colours = rob_colours  
) + 
  ggtitle("Summary of Risk of Bias - Stress") +
  theme(plot.title = element_text(hjust = 0.5))  

rob_save(summary_plot, file.path(output_path, paste0("Risk of bias/", "stress_summary.png")))


summary_plot <- rob_summary_quips_custom(
  data = rob_stress_without_overall,       
  overall = FALSE,        
  weighted = FALSE,      
  rob_colours = rob_colours  
) + 
  ggtitle("Summary of Risk of Bias - Stress") +
  theme(plot.title = element_text(hjust = 0.5))  

rob_save(summary_plot, file.path(output_path, paste0("Risk of bias/", "stress_summary_without_overall.png")))


################################
# 3. ROB PLOT                #
###############################

### Depression
rob_depr_with_overall <- rob_depr_with_overall[order(rob_depr_with_overall$overall, rob_depr_with_overall$Study), ]
traffic_plot <- rob_traffic_light_quips(
  data        = rob_depr_with_overall,       
  rob_colours = rob_colours,
  psize       = 4,
  overall     = TRUE
) + 
  ggtitle("Risk of Bias by domains - Depression") +
  theme(plot.title = element_text(hjust = 0.5))  

traffic_plot

rob_save(traffic_plot, file.path(output_path, paste0("Risk of bias/", "depr_traffic_light_with_overall.png")))


### Anxiety
rob_anx_with_overall <- rob_anx_with_overall[order(rob_anx_with_overall$overall, rob_anx_with_overall$Study), ]
traffic_plot <- rob_traffic_light_quips(
  data        = rob_anx_with_overall,       
  rob_colours = rob_colours,
  psize       = 6,
  overall     = TRUE
) + 
  ggtitle("Risk of Bias by domains - Anxiety") +
  theme(plot.title = element_text(hjust = 0.5))  

traffic_plot

rob_save(traffic_plot, file.path(output_path, paste0("Risk of bias/", "anx_traffic_light_with_overall.png")))


### Stress
rob_stress_with_overall <- rob_stress_with_overall[order(rob_stress_with_overall$overall, rob_stress_with_overall$Study), ]
traffic_plot <- rob_traffic_light_quips(
  data        = rob_stress_with_overall,       
  rob_colours = rob_colours,
  psize       = 6,
  overall     = TRUE
) + 
  ggtitle("Risk of Bias by domains - Stress") +
  theme(plot.title = element_text(hjust = 0.5))  

traffic_plot

rob_save(traffic_plot, file.path(output_path, paste0("Risk of bias/", "stress_traffic_light_with_overall.png")))

#################################################################################
########################       LONGITUDINAL STUDIES      ######################## 
#################################################################################



# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Functions to create Risk of bias
source(file.path(functions_path, "prep_data.R"))
source(file.path(functions_path, "functions2.R"))

rob_longitudinal_with_overall      <- risk_of_bias(data, "Longitudinal", include_overall = TRUE)

rob_longitudinal_with_overall  <- rob_longitudinal_with_overall [order(rob_longitudinal_with_overall $overall, rob_longitudinal_with_overall $Study), ]
traffic_plot <- rob_traffic_light_quips(
  data        = rob_longitudinal_with_overall ,       
  rob_colours = rob_colours,
  psize       = 6,
  overall     = TRUE
) + 
  ggtitle("Risk of Bias by domains - Study design: Longitudinal") +
  theme(plot.title = element_text(hjust = 0.5))  
rob_save(traffic_plot, file.path(output_path, paste0("Risk of bias/", "longitudinal_traffic_light.png")))
traffic_plot


################### High ##################################
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))
data$attrition[data$author_year == "Ding  2023"] <- "High"

# Functions to create Risk of bias
source(file.path(functions_path, "prep_data.R"))
source(file.path(functions_path, "functions2.R"))

rob_longitudinal_with_overall      <- risk_of_bias(data, "Longitudinal", include_overall = TRUE)
rob_longitudinal_with_overall  <- rob_longitudinal_with_overall [order(rob_longitudinal_with_overall $overall, rob_longitudinal_with_overall $Study), ]

traffic_plot <- rob_traffic_light_quips(
  data        = rob_longitudinal_with_overall ,       
  rob_colours = rob_colours,
  psize       = 6,
  overall     = TRUE
) + 
  ggtitle("Risk of Bias by domains - Study design: Longitudinal") +
  theme(plot.title = element_text(hjust = 0.5))  
rob_save(traffic_plot, file.path(output_path, paste0("Risk of bias/", "longitudinal_traffic_light_high.png")))
traffic_plot

################### Low ##################################
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))
data$attrition[data$author_year == "Ding  2023"]  <- "Low"

# Functions to create Risk of bias
source(file.path(functions_path, "prep_data.R"))
source(file.path(functions_path, "functions2.R"))

rob_longitudinal_with_overall      <- risk_of_bias(data, "Longitudinal", include_overall = TRUE)

rob_longitudinal_with_overall  <- rob_longitudinal_with_overall [order(rob_longitudinal_with_overall $overall, rob_longitudinal_with_overall $Study), ]
traffic_plot <- rob_traffic_light_quips(
  data        = rob_longitudinal_with_overall ,       
  rob_colours = rob_colours,
  psize       = 6,
  overall     = TRUE
) + 
  ggtitle("Risk of Bias by domains - Study design: Longitudinal") +
  theme(plot.title = element_text(hjust = 0.5))  
rob_save(traffic_plot, file.path(output_path, paste0("Risk of bias/", "longitudinal_traffic_light_low.png")))
traffic_plot

#### Load data

data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))
data$attrition[data$author_year == "Ding  2023"]  <- "Moderate"

# Functions to create Risk of bias
source(file.path(functions_path, "prep_data.R"))
source(file.path(functions_path, "functions2.R"))
rob_longitudinal_with_overall      <- risk_of_bias(data, "Longitudinal", include_overall = TRUE)

rob_longitudinal_with_overall  <- rob_longitudinal_with_overall [order(rob_longitudinal_with_overall $overall, rob_longitudinal_with_overall $Study), ]
traffic_plot <- rob_traffic_light_quips(
  data        = rob_longitudinal_with_overall ,       
  rob_colours = rob_colours,
  psize       = 6,
  overall     = TRUE
) + 
  ggtitle("Risk of Bias by domains - Study design: Longitudinal") +
  theme(plot.title = element_text(hjust = 0.5))  
rob_save(traffic_plot, file.path(output_path, paste0("Risk of bias/", "longitudinal_traffic_light_moderate.png")))
traffic_plot