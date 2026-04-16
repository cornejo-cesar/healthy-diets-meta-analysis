### RISK OF BIAS SCRIPT 

# ------------------------------------------------------------
# **1. Generate bar plot - ordinal Function (generate_barplot):**
# Create a function to generate bar charts automatically with sorting logic.
generate_barplot <- function(data, category_var, title_name, output_file, 
                             annotation_positions = list(x1 = 4, y1 = 29, x2 = 4, y2 = 27.5), 
                             angle = 0, show_custom_annotation = TRUE, custom_annotation = "Total observations") {
  
  if (!category_var %in% colnames(data)) {                                      # Check if the categorical variable exists                                    
    stop(paste("The variable", category_var, "does not exist in the dataset."))
  }
  
  category_data <- data %>%                                                     # Filter unique data
    distinct(report_id, .data[[category_var]], .keep_all = TRUE)                       
  
  total_observations <- nrow(category_data)                                     # Calculate total observations and included studies
  total_studies_included <- length(unique(category_data$report_id))
  
  if (is.factor(data[[category_var]]) && is.ordered(data[[category_var]])) {    # Check if the variable is a factor (ordinal or nominal)
    # For ordinal variables, keep the natural order
    category_data[[category_var]] <- factor(category_data[[category_var]], levels = levels(data[[category_var]]), ordered = TRUE)
  } else {
    # For nominal variables, reorder by descending count
    category_data[[category_var]] <- factor(category_data[[category_var]], 
                                            levels = names(sort(table(category_data[[category_var]]), decreasing = TRUE)))
  }
  
  # Generate the bar plot
  p <- ggplot(category_data, aes(x = .data[[category_var]])) +
    geom_bar(fill = "#611343", color = "#611343", width = 0.8) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +  
    labs(#title = paste("Distribution of studies included by", title_name),
         x = " ",
         y = " ") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),                                         # Remove grid lines
          axis.text.y = element_blank(),                                        # Remove Y-axis text
          axis.ticks.y = element_blank(),                                       # Remove Y-axis ticks
          axis.line.y = element_blank(),                                        # Remove Y-axis line
          axis.text.x = element_text(angle = angle, hjust = 0.5,  size = 12)
    ) +
    annotate(
      "text", x = annotation_positions$x1,  y = annotation_positions$y1,
      label = paste("Total studies included:", total_studies_included), 
      color = "black", size = 4, hjust = 0
    ) 
  
  
  if (show_custom_annotation) {
    p <- p + annotate(
      "text", x = annotation_positions$x2,  y = annotation_positions$y2,
      label = paste(custom_annotation, total_observations), 
      color = "black", size = 4, hjust = 0
    )
  }
  
  return(p)                                                                     
}

# ------------------------------------------------------------
# **2. Generate  plot Function (generate_plot):**
# Create a function to generate bar charts automatically with sorting logic.
generate_plot <- function(data, category_var, title_name, output_file, 
                             annotation_positions = list(x1 = 4, y1 = 29, x2 = 4, y2 = 27.5), 
                             angle = 0, show_custom_annotation = TRUE, custom_annotation = "Total observations") {
  
  if (!category_var %in% colnames(data)) {                                      # Check if the categorical variable exists                                    
    stop(paste("The variable", category_var, "does not exist in the dataset."))
  }
  
  category_data <- data %>%                                                     # Filter unique data
    distinct(report_id, .data[[category_var]], .keep_all = TRUE)                       
  
  total_observations <- nrow(category_data)                                     # Calculate total observations and included studies
  total_studies_included <- length(unique(category_data$report_id))
  
  if (is.factor(data[[category_var]]) && is.ordered(data[[category_var]])) {    # Check if the variable is a factor (ordinal or nominal)
    # For ordinal variables, keep the natural order
    category_data[[category_var]] <- factor(category_data[[category_var]], levels = levels(data[[category_var]]), ordered = TRUE)
  } else {
    # For nominal variables, reorder by descending count
    category_data[[category_var]] <- factor(category_data[[category_var]], 
                                            levels = names(sort(table(category_data[[category_var]]), decreasing = TRUE)))
  }
  
  # Generate the bar plot
  p <- ggplot(category_data, aes(x = .data[[category_var]])) +
    geom_bar(fill = "#611343", color = "#611343", width = 0.8) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +  
    labs(title = paste("Distribution of studies included by", title_name),
         x = " ",
         y = " ") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),                                         # Remove grid lines
          axis.text.y = element_blank(),                                        # Remove Y-axis text
          axis.ticks.y = element_blank(),                                       # Remove Y-axis ticks
          axis.line.y = element_blank(),                                        # Remove Y-axis line
          axis.text.x = element_text(angle = angle, hjust = 1)
    ) +
    annotate(
      "text", x = annotation_positions$x1,  y = annotation_positions$y1,
      label = paste("Total studies included:", total_studies_included), 
      color = "black", size = 4, hjust = 0
    ) 
  
  
  if (show_custom_annotation) {
    p <- p + annotate(
      "text", x = annotation_positions$x2,  y = annotation_positions$y2,
      label = paste(custom_annotation, total_observations), 
      color = "black", size = 4, hjust = 0
    )
  }
  
  return(p)                                                                     
}

# ------------------------------------------------------------
# **3. Generate  plot year Function (generate_plot_year):**
# Create a function to generate bar charts automatically with sorting logic.

generate_plot_year <- function(data, category_var, title_name, output_file, 
                             annotation_positions = list(x1 = 4, y1 = 29, x2 = 4, y2 = 27.5), 
                             angle = 0, show_custom_annotation = TRUE, custom_annotation = "Total observations") {
  
  if (!category_var %in% colnames(data)) {                                      # Check if the categorical variable exists                                    
    stop(paste("The variable", category_var, "does not exist in the dataset."))
  }
  
  category_data <- data %>%                                                     # Filter unique data
    distinct(report_id, .data[[category_var]], .keep_all = TRUE)                       
  
  total_observations <- nrow(category_data)                                     # Calculate total observations and included studies
  total_studies_included <- length(unique(category_data$report_id))
  
  if (is.factor(data[[category_var]]) && is.ordered(data[[category_var]])) {    # Check if the variable is a factor (ordinal or nominal)
    # For ordinal variables, keep the natural order
    category_data[[category_var]] <- factor(category_data[[category_var]], levels = levels(data[[category_var]]), ordered = TRUE)
  } else {
    # For nominal variables, reorder by descending count
    category_data[[category_var]] <- factor(category_data[[category_var]], 
                                            levels = sort(unique(category_data[[category_var]]))) 
  }
  
  # Generate the bar plot
  p <- ggplot(category_data, aes(x = .data[[category_var]])) +
    geom_bar(fill = "#611343", color = "#611343", width = 0.8) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +  
    labs(title = paste("Distribution of studies included by", title_name),
         x = " ",
         y = " ") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),                                         # Remove grid lines
          axis.text.y = element_blank(),                                        # Remove Y-axis text
          axis.ticks.y = element_blank(),                                       # Remove Y-axis ticks
          axis.line.y = element_blank(),                                        # Remove Y-axis line
          axis.text.x = element_text(angle = angle, hjust = 0.5)
    ) +
    annotate(
      "text", x = annotation_positions$x1,  y = annotation_positions$y1,
      label = paste("Total studies included:", total_studies_included), 
      color = "black", size = 4, hjust = 0
    ) 
  
  
  if (show_custom_annotation) {
    p <- p + annotate(
      "text", x = annotation_positions$x2,  y = annotation_positions$y2,
      label = paste(custom_annotation, total_observations), 
      color = "black", size = 4, hjust = 0
    )
  }
  
  return(p)                                                                     
}
