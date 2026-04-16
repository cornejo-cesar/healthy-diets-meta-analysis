# Convert selected columns to factors and define factor levels  
# This script ensures categorical variables are correctly structured  
# and ordered for analysis and visualization.  

cols_to_factor <- c("literature", "literature_group", "study_design", 
                    "claim", "exposure", "exposure_group", "outcome",
                    "direction", "mh_population", "fns_population",
                    "pop_type", "sample_size", "age_range", "country",   
                    "region", "income_level", "diet_scale","mh_scale",
                    "participation", "predictor_measurement", "attrition",
                    "outcome_measurement", "confounding","analysis_and_reporting")
data[cols_to_factor] <- lapply(data[cols_to_factor], as.factor)
str(data[cols_to_factor])


factor_levels <- list(
  sample_size = c("11-100", "101-500", "501-1000", "1001-5000", ">5000"),
  income_level = c("Low income", "Lower middle income", "Upper middle income"),
  exposure_group = c("G1- Adherence and adequacy", "G2- Dietary pattern (NRCD reducing)", "G3- Diet diversity indices", "G4- Diet quality indices","G5- Factor analysis and others"),
  region = c("Arab Countries", "Africa",  "South America",  "Asia"),
  exposure = c("Mental Health", "Food security and nutrition"),
  study_design = c("Experimental", "Nested design", "Case-control", "Longitudinal", "Cross-sectional"),
  literature = c("General", "Public health", "Medicine", "Mental health", "Nutrition"),
  direction = c("Food security and nutrition → Mental Health", "Mental Health → Food security and nutrition"),
  claim = c("Causal", "Association"),
  participation = c("Low", "Moderate", "High", "No information"),
  predictor_measurement = c("Low", "Moderate", "High", "No information"),
  attrition = c("Low", "Moderate", "High", "No information"),
  outcome_measurement = c("Low", "Moderate", "High", "No information"),
  confounding = c("Low", "Moderate", "High", "No information"),
  analysis_and_reporting = c("Low", "Moderate", "High", "No information")
)

data <- within(data, {
  sample_size <- factor(sample_size, levels = factor_levels$sample_size)
  income_level <- factor(income_level, levels = factor_levels$income_level)
  exposure_group <- factor(exposure_group, levels = factor_levels$exposure_group)
  region <- factor(region, levels = factor_levels$region)
  exposure <- factor(exposure, levels = factor_levels$exposure)
  study_design <- factor(study_design, levels = factor_levels$study_design)
  literature <- factor(literature, levels = factor_levels$literature)
  direction <- factor(direction, levels = factor_levels$direction)
  claim <- factor(claim, levels = factor_levels$claim)
  participation <- factor(participation, levels = factor_levels$participation)
  predictor_measurement <- factor(predictor_measurement, levels = factor_levels$predictor_measurement)
  attrition <- factor(attrition, levels = factor_levels$attrition)
  outcome_measurement <- factor(outcome_measurement, levels = factor_levels$outcome_measurement)
  confounding <- factor(confounding, levels = factor_levels$confounding)
  analysis_and_reporting <- factor(analysis_and_reporting, levels = factor_levels$analysis_and_reporting)
})


# create overall

# Calculate Overall Risk of Bias
data <- data %>%
  rowwise() %>%
  mutate(
    overall = if_else(
      any(c_across(c(participation, predictor_measurement, outcome_measurement, confounding, analysis_and_reporting)) == "High"),
      "High",
      "Low"
    )
  ) %>%
  ungroup()

# Convert 'overall' to Factor
data <- data %>%
  mutate(overall = factor(overall, levels = c("Low", "High")))


print("prep_data")