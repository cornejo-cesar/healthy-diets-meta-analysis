### RISK OF BIAS SCRIPT 

# ------------------------------------------------------------
# **1. Risk of Bias Calculation Function (risk_of_bias):**
risk_of_bias <- function(data, outcome_name, include_overall = TRUE) {
  # Filter Unique Statistics
  filtered_data <- switch(outcome_name,
                          "Depression" = data[data$unique_statistic == 1, ],
                          "Anxiety" = data[data$unique_statistic == 1, ],
                          "Stress" = data[data$unique_statistic == 1, ],
                          stop("Invalid outcome name. Use 'Depression', 'Anxiety', or 'Stress'.")
  )
  
  # Create Risk of Bias (RoB) DataFrame
  rob_data <- data.frame(
    Study = filtered_data[["author_year"]],
    D1 = filtered_data[["participation"]],
    D2 = filtered_data[["predictor_measurement"]],
    D3 = filtered_data[["outcome_measurement"]],
    D4 = filtered_data[["confounding"]],
    D5 = filtered_data[["analysis_and_reporting"]]
  )
 
  if (include_overall) {
    rob_data$overall <- filtered_data[["overall"]]
  } 
  
  return(rob_data)
}

# ------------------------------------------------------------
# **2. Summary Plot Function (rob_summary_quips_custom):**
# Creates summary plot using ggplot2 and robvis.
rob_summary_quips_custom <- function(data,
                                     overall,
                                     weighted,
                                     rob_colours) {
  
  # Define domain names for bias assessment
  domain_names <- c(
    "Study",
    "Bias due to participation",
    "Bias due to prognostic factor measurement",
    "Bias due to outcome measurement",
    "Bias due to confounding",
    "Bias in statistical analysis and reporting",
    "Overall",
    "Weights"
  )
  max_domain_column <- 7  # Set the maximum number of domain columns
  
  # Process data for visualization
  rob_tidy <- tidy_data_summ(data, max_domain_column, overall, weighted, domain_names, levels = c("x","n", "h", "m", "l"))

  # Generate Summary Plot
  plot <- ggplot2::ggplot(data = rob_tidy) +
    theme_rob_summ(overall, max_domain_column - 2) +
    ggplot2::scale_fill_manual(
      values = c(
        n = rob_colours$ni_colour,
        h = rob_colours$high_colour,
        m = rob_colours$moderate_colour,
        l = rob_colours$low_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        n = "No information",
        h = "High",
        m = "Moderate",
        l = "Low",
        x = "N/A"
      ),
      drop = TRUE
    )
  
  plot$rec_height <- get_height(type = "summ")
  plot$rec_width <- get_width(type = "summ")
  plot$rob_type <- "summary"
  
  return(plot)
}


# ------------------------------------------------------------
# **3. Tidy Data Function (tidy_data_summ):**
# Processes dataset into long format for visualization.
tidy_data_summ <- function(data, max_domain_column, overall, weighted, domain_names, levels) {
  
  # Handle legacy versions of datasets by adjusting column selections
  if (ncol(data) == max_domain_column + 1) {
    if (overall == FALSE) {
      data <- data[,c(1:max_domain_column-1,max_domain_column + 1)]
    }
    if (weighted == FALSE) { # Remove the last column if "weighted" is FALSE
      data <- data[,-ncol(data)]
    }
  }
  
  # Verify that the dataset has the expected structure
  check_cols(
    data = data,
    max_domain_column = max_domain_column,
    overall = overall,
    type = "summ",
    weight = weighted
  )
  
  # Adjust domain names if "overall" is not included
  if (overall ==  FALSE) {
    max_domain_column <- max_domain_column - 1
    domain_names <- domain_names[c(1:max_domain_column, length(domain_names))]
  }
  
  # Assign default weight if "weighted" is FALSE
  if (weighted == FALSE) {
    data[, max_domain_column + 1] <- rep(1, length(nrow(data)))
  }
  
  # Clean data by applying text formatting functions
  data.tmp <-
    cbind(data[,1],data.frame(lapply(data[, 2:max_domain_column], clean_data),
                              data[, ncol(data)],
                              stringsAsFactors = F))
  
  # Rename dataset columns based on domain names
  names(data.tmp) <- domain_names
  
  # Convert data into long format for visualization
  rob.tidy <- suppressWarnings(tidyr::gather(
    data.tmp[-1],
    domain, judgement, -Weights
  ))
  
  # Convert domain names into factors for ordering in plots
  rob.tidy$domain <- as.factor(rob.tidy$domain)
  
  # Reverse factor levels for better visualization in plots
  rob.tidy$domain <-
    factor(rob.tidy$domain,
           levels = rev(domain_names))
  
  # Ensure that "judgement" values are categorized correctly
  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = levels)
  
  # Return the formatted dataset
  rob.tidy
}


# Set colours

rob_colours <- list(
  ni_colour       = "#4EA1F7",         # No information
  high_colour     = "#BF0000",        # High risk
  moderate_colour = "#E2DF07",    # Moderate risk
  low_colour      = "#02C100",         # Low risk
  na_colour       = "gray"              # Not applicable
)


# ------------------------------------------------------------
# **4. Check cols Function (check_cols):**
check_cols <- function(data,
                       max_domain_column,
                       overall,
                       type = "tf",
                       weight = FALSE){
  
  # Calculate the expected number of columns
  expected_col <- max_domain_column + 1      
  
  # Adjust the expected column count based on the presence of "Overall" and "Weight"
  if (!overall & !weight) {
    expected_col <- expected_col - 2
    domain_text = paste0(expected_col,
                         ": a \"Study\" column and ",
                         max_domain_column - 2,
                         " \"Domain\" columns.")
    var_ind <- "neither"
    
  }
  
  if (!overall & weight) {
    expected_col <- expected_col - 1
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 1,
      " \"Domain\" columns, and \"Weight\" column."
    )
    var_ind <- "weight"
  }
  
  if (overall & !weight) {
    expected_col <- expected_col - 1
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 1,
      " \"Domain\" columns, and an \"Overall\" column."
    )
    var_ind <- "overall"
  }
  
  if (overall & weight) {
    expected_col <- expected_col
    domain_text = paste0(
      expected_col,
      ": a \"Study\" column, ",
      max_domain_column - 2,
      " \"Domain\" columns, an \"Overall\" column and a \"Weight\" column."
    )
    var_ind <- "both"
  }
  
  # Create a text description for the weighting option  
  if (type == "summ") {
    weighted_text <- paste(" and weighted =", weight)
  } else {
    weighted_text <- ""
  }
  
  # Validate if the dataset has the expected number of columns
  if (ncol(data) == expected_col) {
    if ((var_ind %in% c("both", "weight")) &&
        unique(grepl("^[-]{0,1}[0-9]{0,}.{0,1}[0-9]{1,}$",
                     data[[ncol(data)]])) == FALSE) {
      stop(
        "Error. The final column does not seem to contain numeric values ",
        "(expected for weighted = TRUE)."
      )
    }} else {
      if (ncol(data) != expected_col) {
        stop(
          "The number of columns in your data (",
          ncol(data),
          ") does not match the number expected for this",
          " tool when using overall = ", overall, weighted_text,
          ". The expected number of columns is ",
          domain_text
        )}
    }
}


# ------------------------------------------------------------
# **5. Clean data Function (clean_data):**
clean_data <- function(col) {
  col <- trimws(tolower(col))                                                   # Convert text to lowercase and trim whitespace
  col <- ifelse(col %in% c("na", "n") | is.na(col), "x", col)                   # Replace missing values or specific invalid values ("na", "n") with "x"
  col <- substr(col, 0, 1)                                                      # Extract only the first character of each entry
  return(col)
}


# ------------------------------------------------------------
# **6. Theme risk of bias summary Function (theme_rob_summ):**
theme_rob_summ <- function(overall = TRUE, max_domain_column){
  standard <- list(
    ggplot2::geom_bar(                                                          # Create a stacked bar plot for bias levels
      mapping = ggplot2::aes(
        x = domain,
        fill = judgement,
        weight = Weights
      ),
      width = 0.7,
      position = "fill",
      color = "black"
    ),
    
    
    ggplot2::coord_flip(ylim = c(0, 1)),                                        # Flip coordinates for a horizontal bar chart
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = T)),                 # Reverse the legend order
    ggplot2::scale_y_continuous(labels = scales::percent),                      # Format the y-axis as percentages
    
    ggplot2::theme(                                                             # Define plot theme settings
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(
        colour = "black",
        linewidth = 0.5,
        linetype = "solid"
      ),
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(
        linetype = "solid",
        colour = "black"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.75, "lines"),
      legend.text = ggplot2::element_text(size = 6)
    ),
    
    bold_overall = ggplot2::theme(axis.text.y = ggplot2::element_text(          # Define text formatting for the y-axis labels
      size = 10,
      color = "black"
    ))
  )
  
  if (overall) {                                                                # Apply bold text to the overall bias assessment if enabled
    standard[["bold_overall"]] <-
      ggplot2::theme(axis.text.y = suppressWarnings(ggplot2::element_text(size = 10,
                                                                          color = "black",
                                                                          face = c("bold", rep("plain", max_domain_column)))))
  }
  
  return(standard)
}


# ------------------------------------------------------------
# **7. Save Plot Function (rob_Save):**
rob_save <- function(rob_object,
                     file = "rob_figure.png",
                     height = "default",
                     width = "default",
                     dpi = 800) {
  
  check_extension(file)
  width <- ifelse(width == "default", rob_object$rec_width, width)
  height <- ifelse(height == "default", rob_object$rec_height, height)
  
  ggplot2::ggsave(
    file,
    plot = rob_object,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    limitsize = FALSE
  )
}

check_extension <- function(file){
  ex <- strsplit(basename(file), split="\\.")[[1]]
  if (!(ex[-1] %in% c("png","jpeg","tiff","eps"))) {
    stop(paste0("Saving to this file type is not supported by robvis. ",
                "Acceptable file types are \".png\", \".jpeg\", ",
                " \".tiff\", and \".eps\". "))
  }
}



# ------------------------------------------------------------
# **8. Get height Function (get_height):**
get_height <- function(data, psize, type = "tf") {
  if (type == "tf") {
    nrows <- nrow(data)  
    height <- 2 + nrows * .4 / (10 / psize)  #
  } else {
    height <- 2.41  
  }
  return(height)
}


# ------------------------------------------------------------
# **9. Get width Function (get_width):**
get_width <- function(data, psize, type = "tf") {
  if (type == "tf") {
    
    nchar_study <- max(nchar(as.character(data$Study)), na.rm = TRUE)
    nchar_domain <- max(nchar(as.character(colnames(data))), na.rm = TRUE) + 3
    
    width_adj <- ifelse(nchar_study > 8, 6 + nchar_study * 0.05, 6)
    width <- ifelse(nchar_domain > 42, width_adj + (nchar_domain - 42) * 0.05, width_adj)
  } else {
    width <- 8  
  }
  return(width)
}


# ------------------------------------------------------------
# **10. Risk of bias traffic ligth Function (rob_traffic_light_quips):**
rob_traffic_light_quips <- function(data,
                                    rob_colours,
                                    psize,
                                    overall) {
  
  max_domain_column <- 7  
  domain_names <- c("Study", "D1", "D2", "D3", "D4", "D5", "Overall")
  
  rob.tidy <- tidy_data_tf(data,
                           max_domain_column = max_domain_column,
                           domain_names = domain_names,
                           overall = overall,
                           levels = c("h", "m", "l", "n", "x"))
  
  ssize <- psize - (psize / 4)
  adjust_caption <- get_caption_adjustment(rob.tidy)
  
  trafficlightplot <- ggplot2::ggplot(rob.tidy,
                                      ggplot2::aes(x = 1,
                                                   y = 1,
                                                   colour = judgement)) +
    theme_rob_tf(rob.tidy,
                 domain_names,
                 psize,
                 ssize,
                 adjust_caption,
                 overall) +
    ggplot2::labs(
      caption = "  Domains:
  D1: Bias due to participation.
  D2: Bias due to prognostic factor measurement.
  D3: Bias due to outcome measurement.
  D4: Bias due to confounding.
  D5: Bias in statistical analysis and reporting.
                  "
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        h = rob_colours$high_colour,
        m = rob_colours$moderate_colour,
        l = rob_colours$low_colour,
        n = rob_colours$ni_colour,
        x = rob_colours$na_colour
      ),
      labels = c(
        h = "High",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    ) +
    ggplot2::scale_shape_manual(
      values = c(
        h = 120,
        m = 45,
        l = 43,
        n = 63,
        x = 32
      ),
      labels = c(
        h = "High",
        m = "Moderate",
        l = "Low",
        n = "No information",
        x = "Not applicable"
      ),
      drop = TRUE,
      limits = force
    )
  # ✅ Assign expected attributes for rob_save
  trafficlightplot$rec_height <- get_height(
    data = data,
    psize = psize,
    type = "tf"
  )
  
  trafficlightplot$rec_width <- get_width(
    data = data,
    psize = psize,
    type = "tf"
  )
  
  trafficlightplot$rob_type <- "traffic"
  
  return(trafficlightplot)
}

# ------------------------------------------------------------
# **11. Tidy data Function (tidy_data_tf):**
tidy_data_tf <- function(data, max_domain_column, domain_names, overall, levels) {
  
  check_cols(data = data,
             max_domain_column = max_domain_column,
             overall = overall,
             weight = FALSE)
  
  if (!overall) {
    max_domain_column <- max_domain_column - 1
    domain_names <- domain_names[1:max_domain_column]
  }
  
  data.tmp <-
    cbind(data[, 1], data.frame(lapply(data[, 2:max_domain_column], clean_data),
                                stringsAsFactors = F))
  
  names(data.tmp) <- domain_names
  
  rob.tidy <- suppressWarnings(tidyr::gather(data.tmp,
                                             domain, judgement,-Study))
  
  
  rob.tidy$Study <-
    factor(rob.tidy$Study, levels = unique(data.tmp$Study))
  
  rob.tidy$judgement <- as.factor(rob.tidy$judgement)
  
  rob.tidy$judgement <-
    factor(rob.tidy$judgement, levels = levels)
  
  rob.tidy
}

get_caption_adjustment <- function(data){
  -0.7 + length(unique(data$judgement)) * -0.6
}

# ------------------------------------------------------------
# **12. Tidy data Function (tidy_data_tf):**
theme_rob_tf <-function(rob.tidy,
                        domain_names,
                        psize,
                        ssize,
                        adjust_caption,
                        overall,
                        judgement_title = "Judgement",
                        overall_name = "Overall",
                        x_title = "Risk of bias domains",
                        y_title = "Study"){
  standard <- list(
    ggplot2::facet_grid(Study ~
                          factor(domain, levels = domain_names),
                        switch = "y",
                        space = "free"),
    ggplot2::geom_point(size = 6),
    ggplot2::geom_point(size = 4,
                        colour = "black",
                        ggplot2::aes(shape = judgement)),
    ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain !=
                              overall_name),],
      fill = "#ffffff",
      color = "#ffffff",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      show.legend = FALSE
    ),
    overall_name = ggplot2::geom_rect(
      data = rob.tidy[which(rob.tidy$domain ==
                              overall_name),],
      fill = "#d3d3d3",
      color = "#d3d3d3",
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      show.legend = FALSE
    ),
    ggplot2::geom_point(size = psize, show.legend = FALSE),
    ggplot2::geom_point(
      data = rob.tidy[which(rob.tidy$judgement !=
                              "x"),],
      shape = 1,
      colour = "black",
      size = psize,
      show.legend = FALSE
    ),
    ggplot2::geom_point(
      size = ssize,
      colour = "black",
      ggplot2::aes(shape = judgement),
      show.legend = FALSE
    ),
    ggplot2::scale_x_discrete(position = "top", name = x_title),
    ggplot2::scale_y_continuous(
      limits = c(1, 1),
      labels = NULL,
      breaks = NULL,
      name = y_title,
      position = "left"
    ),
    ggplot2::scale_size(range = c(5,20)),
    ggplot2::theme_bw(),
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey"),
      panel.spacing = ggplot2::unit(0, "line"), legend.position = "bottom",
      legend.justification = "right", legend.direction = "vertical",
      legend.margin = ggplot2::margin(
        t = -0.2, r = 0,
        b = adjust_caption, l = -10, unit = "cm"
      ),
      strip.text.x = ggplot2::element_text(size = 10),
      strip.text.y.left = ggplot2::element_text(
        angle = 0,
        size = 10
      ), legend.text = ggplot2::element_text(size = 9),
      legend.title = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "#a9a9a9"),
      plot.caption = ggplot2::element_text(
        size = 10,
        hjust = 0, vjust = 1
      )
    ),
    ggplot2::guides(shape = ggplot2::guide_legend(
      override.aes = list(fill = NA))),
    ggplot2::labs(shape = judgement_title, colour = judgement_title)
    
  )
  
  # Remove element that draws dark box for "Overall" column
  if (!overall) {
    standard[["overall_name"]] <- ggplot2::geom_blank()
  }
  
  return(standard)
  
}

print("functions")
