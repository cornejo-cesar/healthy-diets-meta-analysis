####################
#  FOREST   depression
########################

library(forestplot)

# Create the results dataframe
effect_indices_results <- data.frame(
  effect_indices = c("RE - REML (COR)", "RE - REML (Hedges' g)", "RE - REML (Cohen's d)"),
  estimate = c(-0.149, -0.29, -0.163),
  se = c( 0.017, 0.030, 0.045),
  ci_lb = c( -0.183, -0.345, -0.253),
  ci_ub = c( -0.12, -0.23, -0.074),
  total_n = c(473236, 473236, 473236),
  total_estimates = c(70, 70, 70)
)


table_text <- rbind(
  c("Effect size indices", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = effect_indices_results$effect_indices,
    "N. Estimates" = as.character(effect_indices_results$total_estimates),  # Add N. Estimates column
    "n" = as.character(effect_indices_results$total_n),
    Effect = format(round(effect_indices_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(effect_indices_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(effect_indices_results$ci_ub, 2), nsmall = 2), "]")
  )
)


png(file.path(output_path, "Subgroups/depr_effect_indices.png"), width = 1200, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, effect_indices_results$estimate),   
  lower = c(NA, effect_indices_results$ci_lb),     
  upper = c(NA, effect_indices_results$ci_ub),     
  zero = 0,                                     # Null effect line
  xlog = FALSE,                                 # Do not transform x-axis
  boxsize = 0.2,                                # Box size
  lineheight = "auto",                          # Automatic line height
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",                # X-axis label
  lwd.ci = 2,                                   # Line width for confidence intervals
  title = paste0("Depression by effect sizes indices \nTotal Studies Included:  70"),
  graph.pos = 2
)

dev.off()




####################
#  FOREST  anxiety
########################



# Create the dataframe
effect_indices_results <- data.frame(
  effect_indices = c("RE - REML (COR)", "RE - REML (Hedges' g)", "RE - REML (Cohen's d)"),
  estimate = c(-0.133, -0.253, -0.138),
  se = c( 0.026, 0.046, 0.043),
  ci_lb = c( -0.186, -0.346, -0.229),
  ci_ub = c( -0.080, -0.160, -0.047),
  total_n = c(146217, 146217, 146217),
  total_estimates = c(43, 43, 43)
)

# Create the text table for the plot
table_text <- rbind(
  c("Study Design", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = effect_indices_results$effect_indices,
    "N. Estimates" = as.character(effect_indices_results$total_estimates),  # N. Estimates column
    "n" = as.character(effect_indices_results$total_n),
    Effect = format(round(effect_indices_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(effect_indices_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(effect_indices_results$ci_ub, 2), nsmall = 2), "]")
  )
)

 # Generate the forest plot
png(file.path(output_path, "Subgroups/anx_effect_indices.png"), width = 1200, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, effect_indices_results$estimate),   
  lower = c(NA, effect_indices_results$ci_lb),     
  upper = c(NA, effect_indices_results$ci_ub),     
  zero = 0,                                     # Null effect line
  xlog = FALSE,                                 # Do not transform x-axis
  boxsize = 0.2,                                # Box size
  lineheight = "auto",                          # Automatic line height
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",                # X-axis label
  lwd.ci = 2,                                   # Line width for confidence intervals
  title = paste0("Anxiety by effect sizes indices \nTotal Studies Included:  43"),
  graph.pos = 2
)

dev.off()


####################
#  FOREST  stress
########################

# Create the updated dataframe
effect_indices_results <- data.frame(
  effect_indices = c("RE - REML (COR)", "RE - REML (Hedges' g)", "RE - REML (Cohen's d)"),
  estimate = c(-0.120, -0.236, -0.15),
  se = c( 0.023, 0.043, 0.048),
  ci_lb = c( -0.169, -0.327, -0.251),
  ci_ub = c( -0.072, -0.144, -0.039),
  total_n = c(24690, 24690, 24690),
  total_estimates = c(26, 26, 26)
)

# Create the text table for the plot
table_text <- rbind(
  c("Study Design", "N. Estimates", "n", "Effect", "95%-CI"),
  cbind(
    Subgroup = effect_indices_results$effect_indices,
    "N. Estimates" = as.character(effect_indices_results$total_estimates),  # N. Estimates column
    "n" = as.character(effect_indices_results$total_n),
    Effect = format(round(effect_indices_results$estimate, 2), nsmall = 2),
    CI = paste0("[", format(round(effect_indices_results$ci_lb, 2), nsmall = 2), "; ",
                format(round(effect_indices_results$ci_ub, 2), nsmall = 2), "]")
  )
)

 # Generate the forest plot
png(file.path(output_path, "Subgroups/stress_effect_indices.png"), width = 1200, height = 800, res = 150)

forestplot(
  labeltext = table_text,
  mean = c(NA, effect_indices_results$estimate),   
  lower = c(NA, effect_indices_results$ci_lb),     
  upper = c(NA, effect_indices_results$ci_ub),     
  zero = 0,                                     # Null effect line
  xlog = FALSE,                                 # Do not transform x-axis
  boxsize = 0.2,                                # Box size
  lineheight = "auto",                          # Automatic line height
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  xlab = "Effect Size (95% CI)",                # X-axis label
  lwd.ci = 2,                                   # Line width for confidence intervals
  title = paste0("Stress by effect sizes indices \nTotal Studies Included:  26"),
  graph.pos = 2
)

dev.off()

