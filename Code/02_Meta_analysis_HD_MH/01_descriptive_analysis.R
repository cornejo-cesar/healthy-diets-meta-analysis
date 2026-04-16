##########################
# 1.  LOAD LIBRARIES     #
#########################

library(readxl)
library(ggplot2)
library(dplyr)

##########################
# 2.  PREPARE THE DATA   #
#########################

# Load data
data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))

# Prep. data
source(file.path(functions_path, "prep_data.R"))

# Calculate total studies included: Filter the number of included studies
study_included <- data %>%
  filter(unique_report == 1) 

total_studies_included <- nrow(study_included)


###########################
# 3. DESCRIPTIVE ANALYSIS #
##########################


# Load function generate_barplot
source(file.path(functions_path, "generate_barplot.R"))


### Sample size
p <- generate_barplot(
  data = data,                     
  category_var = "sample_size", 
  title_name = "sample size",
  annotation_positions = list(x1 = 4.35, y1 = 30, x2 = 4.5, y2 = 33.5),
  show_custom_annotation = FALSE)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/sample_size_distribution.png"),
  plot = p,  width = 11, height = 6, dpi = 300)


### Income level
p <- generate_barplot(
  data                   = data,                    
  category_var           = "income_level",    
  title_name             = "income level",
  annotation_positions   = list(x1 = 2.8, y1 = 76, x2 = 0.5, y2 = 72),
  show_custom_annotation = FALSE)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/income_level_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)


### Region
data_region <- data[data$region != "Arab Countries", ]
p <- generate_barplot(
  data                   = data_region,                    
  category_var           = "region",    
  title_name             = "region",
  annotation_positions   = list(x1 = 2.7, y1 = 76, x2 = 2.7, y2 = 72),
  show_custom_annotation =FALSE
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/region_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)


### Study design
p <- generate_barplot(
  data                 = data,                    
  category_var         = "study_design",    
  title_name           = "study design",
  annotation_positions = list(x1 = 3.5, y1 = 77, x2 = 3.5, y2 = 74),
  custom_annotation    = "Total study design: "
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/study_design_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)

### Literature
p <- generate_barplot(
  data = data,                    
  category_var = "literature",    
  title_name = "literature",
  output_file = file.path(output_path, "Descriptive analysis/literature_distribution.png"),
  annotation_positions = list(x1 = 4.3, y1 = 39, x2 = 4.5, y2 = 37.5),
  show_custom_annotation = FALSE
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/literature_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)

### direction
p <- generate_barplot(
  data = data,                    
  category_var = "direction",    
  title_name = "direction",
  output_file = file.path(output_path, "Descriptive analysis/direction_distribution.png"),
  annotation_positions = list(x1 = 1.9, y1 = 80, x2 = 1.9, y2 = 77),
  show_custom_annotation = FALSE
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/direction_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)

### claim
p <- generate_barplot(
  data = data,                    
  category_var = "claim",    
  title_name = "claim",
  output_file = file.path(output_path, "Descriptive analysis/clain_distribution.png"),
  annotation_positions = list(x1 = 1.9, y1 = 89, x2 = 1.5, y2 = 90)
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/clain_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)




########## POPULATION ##########

data <- data %>%
  mutate(across(
    c(pop_type, fns_population, mh_population, demographic), 
    ~ case_when(
      . == "Adults (General and representative)" ~ "Adults (General)",
      TRUE ~ . 
    )
  ))



### MH population
p <- generate_plot(
  data = data,                    
  category_var = "mh_population",    
  title_name = "MH population",
  annotation_positions = list(x1 = 9, y1 = 59, x2 = 9, y2 = 55.5),
  angle=75,
  custom_annotation = "Total MH population:"
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/mh_population_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)


### FNS population
p <- generate_plot(
  data = data,                    
  category_var = "fns_population",    
  title_name = "FNS population",
  output_file = file.path(output_path, "Descriptive analysis/fns_population_distribution.png"),
  annotation_positions = list(x1 = 12, y1 = 59, x2 = 12, y2 = 56),
  angle=75,
  custom_annotation = "Total FNS population:"
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/fns_population_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)


### Pop type
p <- generate_plot(
  data = data,                    
  category_var = "pop_type",    
  title_name = "type population",
  output_file = file.path(output_path, "Descriptive analysis/pop_type_distribution.png"),
  annotation_positions = list(x1 = 13, y1 = 46, x2 = 13, y2 = 43),
  angle=75,
  custom_annotation = "Total type population:"
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/pop_type_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)

### Demographic
p <- generate_plot(
  data = data,                    
  category_var = "demographic",    
  title_name = "population",
  output_file = file.path(output_path, "Descriptive analysis/population_distribution.png"),
  annotation_positions = list(x1 = 6.8, y1 = 57, x2 = 6.8, y2 = 53.5),
  angle=75,
  custom_annotation = "Total population:"
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/population_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)



# Year
p <- generate_plot_year(
  data = data,                    
  category_var = "year",    
  title_name = "year",
  output_file = file.path(output_path, "Descriptive analysis/year_distribution.png"),
  annotation_positions = list(x1 = 10, y1 = 21, x2 = 10, y2 = 20),
  angle=0,
  show_custom_annotation = FALSE
)
print(p)
ggsave(
  filename = file.path(output_path, "Descriptive analysis/year_distribution.png"),
  plot = p, width = 11, height = 6, dpi = 300)


#########################
# 4. CUSTOMIZED CHARTS #
########################

################
#  EXPOSURE   #
###############

exposure_group_data <- data %>%                                                 # Create the  sample size data
  distinct(report_id, exposure_group, .keep_all = TRUE)

total_observations   <- nrow(exposure_group_data)                               # Calculate total observations

# Bar plot
p <- ggplot(exposure_group_data, aes(x = exposure_group)) +
  geom_bar(fill = "#611343", color = "#611343") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of studies included by exposure",
       x = " ",
       y = " ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),                                           # Remove grid lines
        axis.text.y = element_blank(),                                          # Remove Y-axis text
        axis.ticks.y = element_blank(),                                         # Remove Y-axis ticks
        axis.line.y = element_blank()                                           # Remove Y-axis line
  ) +
  scale_x_discrete(
    labels = c(
      "G1-\nAdherence\nand adequacy",
      "G2-\nDietary pattern\n(NRCD reducing)",
      "G3-\nDiet diversity\nindices",
      "G4-\nDiet quality\nindices",
      "G5-\nFactor analysis\nand others"
    )
  )+
  annotate(
    "text", x = 4.4,  
    y = max(table(exposure_group_data$exposure_group)) + 6, 
    label = paste("Total studies included:", total_studies_included), 
    color = "black", size = 4, hjust = 0
  ) 


ggsave(file.path(output_path, "Descriptive analysis/exposure_specific_distribution.png"), plot = p, width = 9, height = 6, dpi = 300)
print(p)


###############
#   OUTCOME  #
##############

depr <- data[!is.na(data$depr_z), ] %>% distinct(report_id, .keep_all = TRUE)
anx <- data[!is.na(data$anx_z), ] %>% distinct(report_id, .keep_all = TRUE)
stress <- data[!is.na(data$stress_z) & data$unique_report == 1, ]

total_studies_depr <- length(unique(depr$report_id))
total_studies_anx <- length(unique(anx$report_id))
total_studies_stress <- length(unique(stress$report_id))

outcome_data <- data.frame(
  Outcome = c( "Stress", "Anxiety", "Depression"),
  Total_Studies = c(total_studies_stress, total_studies_anx, total_studies_depr)
)


p <- ggplot(outcome_data, aes(x = reorder(Outcome, -Total_Studies), y = Total_Studies)) +
  geom_bar(stat = "identity", fill = "#611343", color = "#611343", width = 0.6) + 
  geom_text(aes(label = Total_Studies), vjust = -0.5) + 
  labs(title = "Distribution of studies included by mental health",
       x = " ",
       y = " ") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    panel.grid = element_blank(),           
    axis.text.y = element_blank(),          
    axis.ticks.y = element_blank(),          
    axis.line.y = element_blank()            
  ) +
  annotate(
    "text", 
    x = 2.8,  
    y = max(outcome_data$Total_Studies) * 0.9,  
    label = paste("Total studies included: 83"), 
    color = "black", size = 4, hjust = 0
  )

ggsave(file.path(output_path, "Descriptive analysis/outcome_distribution.png"), plot = p, width = 8, height = 6, dpi = 300)

print(p)


#######################################
#  INCOME LEVEL  (WITH HIGH INCOME)   #
######################################

data <- data.frame(
  IncomeLevel = c("Low income", "Lower middle income", "Upper middle income", "High income"),
  Frequency = c(4, 11, 69, 247)
)

data$IncomeLevel <- factor(data$IncomeLevel, levels = data$IncomeLevel[order(data$Frequency)])

p <- ggplot(data, aes(x = IncomeLevel, y = Frequency)) +
  geom_bar(stat = "identity", fill = "#611343", color = "#611343") + 
  geom_text(aes(label = Frequency), vjust = -0.5) + 
  labs(title = "Distribution of studies included by income level", 
       x = "Low income                                    Middle income                                          High income", 
       y = " ") + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 30)), 
    panel.grid = element_blank(),           
    axis.text.y = element_blank(),          
    axis.ticks.y = element_blank(),         
    axis.line.y = element_blank(),          
    plot.margin = margin(20, 20, 20, 20)    
  ) +
  geom_vline(xintercept = c(1.5, 3.5), linetype = "dashed", color = "black") 

ggsave(file.path(output_path, "Descriptive analysis/income_level_with_high_distribution.png"), plot = p, width = 8, height = 6, dpi = 300)
print(p)


#############
#  REGION  #
############

# # Load data
# data <- read_excel(file.path(data_path, "Raw data 3.xlsx"))
# 
# # Prep. data
# source(file.path(functions_path, "prep_data.R"))
# 
# # Calculate total studies included: Filter the number of included studies
# study_included <- data %>%
#   filter(unique_report == 1) 
# 
# total_studies_included <- nrow(study_included)
# 
# 
# ###
# data_region <- data %>%
#   filter(region != "arab countries" & unique_statistic == 1) %>%
#   distinct(report_id, region, .keep_all = TRUE) %>%
#   mutate(country = as.character(country))
# 
# top_countries_asia <- data_region %>%
#   filter(region == "asia") %>%
#   count(country, sort = TRUE) %>%
#   top_n(3, n) %>%
#   pull(country)
# 
# print(top_countries_asia)
# 
# data_region_processed <- data_region %>%
#   mutate(
#     country_group = case_when(
#       region == "asia" & country %in% top_countries_asia ~ country,
#       region == "asia" ~ "other asia",
#       TRUE ~ "other regions"
#     ),
#     country_group = factor(
#       country_group,
#       levels = c("iran", "china", "nepal", "turkey", "other asia", "other regions")
#     ),
#     region = factor(region, levels = c("asia", "south america", "africa")) 
#   )
# 
# table(data_region_processed$country_group, usena = "always")
# 
# p <- ggplot(data_region_processed, aes(x = region, fill = country_group)) +
#   geom_bar(position = "stack") +
#   geom_text(
#     stat = "count",
#     aes(label = after_stat(count)),
#     position = position_stack(vjust = 0.5),
#     size = 3,
#     color = "white"
#   ) +
#   labs(
#     title = "distribution of studies included by region",
#     x = " ",
#     y = " "
#   ) +
#   scale_fill_manual(
#     values = c(
#       "other asia" = "#0a5256",
#       "other regions" = "#611343",
#       "iran" = "#02bf70",
#       "china" = "#05aec6",
#       "nepal" = "#0a5256",
#       "turkey" = "#ffb81a"
#     ),
#     name = " "
#   ) +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         panel.grid = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_text(angle = 0, hjust = 0.5)
#   ) +
#   annotate(
#     "text", x = 2.5,  y = 76,
#     label = "total studies included: 83",
#     color = "black", size = 4, hjust =0
#   )
# 
# ggsave(file.path(output_path, "descriptive analysis/income_level_with_high_distribution.png"), plot = p, width = 8, height = 6, dpi = 300)
# print(p)

print("Script '1_descriptive_analysis.R' ran successfully")