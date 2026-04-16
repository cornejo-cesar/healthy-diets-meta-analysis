rob_blobbogram <- function(rma,
                           rob,
                           rob_tool = "ROB2",
                           rob_colour = "cochrane",
                           subset_col = "Overall",
                           space_last = TRUE,
                           subset_col_order = NULL,
                           RoB_Group_estimate = TRUE,
                           add_tests = TRUE,
                           arrow_labels = c("Lower", "Higher"),
                           x_scale_log = FALSE,
                           x_breaks = NULL,
                           x_labels = NULL,
                           dpi = 600,
                           ...) {
  
  rob <- rob_append_weights(rob, rma)
  forester_args <- list(...)
  
  # Set default parameters if not provided
  if (!"file_path" %in% names(forester_args)) {
    forester_args$file_path <- tempfile(fileext = ".png")
  }
  if (!"font_family" %in% names(forester_args)) {
    forester_args$font_family <- "Fira Sans"
  }
  if (!"estimate_precision" %in% names(forester_args)) {
    forester_args$estimate_precision <- 2
  }
  if (!"null_line_at" %in% names(forester_args)) {
    forester_args$null_line_at <- 0
  }
  
  data <- metafor_object_to_table(
    rma,
    rob,
    subset_col = subset_col,
    rob_tool = rob_tool,
    rob_colour = rob_colour,
    subset_col_order = subset_col_order,
    RoB_Group_estimate = RoB_Group_estimate,
    add_tests = add_tests
  )
  
  
  if (!is.null(rma$data) && "n" %in% names(rma$data)) {
    sample_sizes <- rma$data[, c("slab", "n")]
  } else if (!is.null(rma$data) && "sample_size" %in% names(rma$data)) {
    sample_sizes <- rma$data[, c("slab", "sample_size")]
    names(sample_sizes)[2] <- "n"
    print("success 1")
  } else {
    stop("Column 'n' or 'sample_size' not found in rma$data.")
  }
  
  rob_plot <- select_rob_columns(data, rob_tool) %>%
    appendable_rob_ggplot(
      rob_tool = rob_tool,
      rob_colour = rob_colour,
      space_last = space_last,
      font_family = forester_args$font_family
    )
  
  print(names(data)) 
  
  
  plot_obj <- do.call(forester::forester, c(
    list(
      left_side_data = dplyr::select(data, Study, n),
      estimate = data$est,
      ci_low = data$ci_low,
      ci_high = data$ci_high,
      add_plot = rob_plot,
      arrow_labels = arrow_labels
    ),
    forester_args
  ))
  
  ######################  
  plot_obj <- plot_obj +
    ggplot2::geom_text(
      data = data,
      ggplot2::aes(label = n),
      x = 1.1,  # Horizontal position (adjust as needed)
      hjust = 0,
      size = 8,
      family = forester_args$font_family
    )
  
  #####################    
  
  if (x_scale_log) {
    if (!is.null(x_breaks) && !is.null(x_labels)) {
      plot_obj <- plot_obj +
        ggplot2::scale_x_log10(breaks = x_breaks, labels = x_labels)
    } else {
      plot_obj <- plot_obj +
        ggplot2::scale_x_log10()
    }
  }
  
  return(plot_obj)
}

# Helpers for rob_blobbogram ====
metafor_function <- function(res, data = dat){
  eval(rlang::call_modify(res$call, data = quote(data)))
}

create_subtotal_row <- function(rma,
                                name = "Subtotal",
                                single_group = FALSE,
                                add_tests = FALSE,
                                add_blank = TRUE){
  if (single_group == FALSE) {
    row <- data.frame(Study = name,
                      est = (rma$b),
                      ci_low = (rma$ci.lb),
                      ci_high = (rma$ci.ub))
    if (add_tests) {
      df_Q   <- if (!is.null(rma$QEdf)) rma$QEdf else (rma$k - rma$p)
      I2_calc <- if (!is.null(rma$I2) && !is.na(rma$I2)) {
        rma$I2
      } else if (!is.null(rma$QE) && is.finite(rma$QE) && rma$QE > 0) {
        max(0, 100 * (rma$QE - df_Q) / rma$QE)
      } else {
        NA_real_
      }
      tests <- data.frame(
        Study = paste0(
          "Q=",
          formatC(rma$QE, digits = 2, format = "f"),
          ", df=",
          rma$k - rma$p,
          ", p=",
          formatC(rma$QEp, digits = 2, format = "f")
        ),
        est = c(NA),
        ci_low = c(NA),
        ci_high = c(NA)
      )
      row <- rbind(row, tests)
    }
    if(add_blank){ row <- dplyr::add_row(row) }
    return(row)
  }
}

create_title_row <- function(title){
  return(data.frame(Study = title, est = NA, ci_low = NA, ci_high = NA))
}

appendable_rob_ggplot <- function(rob_gdata, space_last = TRUE, rob_tool = "ROB2", rob_colour = "cochrane", font_family = "sans") {
  columns <- colnames(rob_gdata)
  rob_gdata$row_num <- (nrow(rob_gdata) - 1):0
  
  # Keep the original structure but create a working copy
  rob_gdata_original <- rob_gdata
  
  # First pivot the data to long format
  rob_gdata_long <- tidyr::pivot_longer(rob_gdata, 
                                        !c(row_num), 
                                        names_to = "domain", 
                                        values_to = "judgement")
  
  # Assign specific colors for the Overall column
  rob_colours <- get_colour(rob_tool, rob_colour)
  
  # Create a color column based on domain and judgement, adapted by tool type
  if(rob_tool == "QUIPS") {
    rob_gdata_long$colour <- dplyr::case_when(
      rob_gdata_long$domain == "Overall" & rob_gdata_long$judgement == "High" ~ rob_colours$overall_high_colour,
      rob_gdata_long$domain == "Overall" & rob_gdata_long$judgement == "Low" ~ rob_colours$overall_low_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "High" ~ rob_colours$high_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Moderate" ~ rob_colours$moderate_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Low" ~ rob_colours$low_colour,
      TRUE ~ rob_colours$ni_colour
    )
  } else if(rob_tool == "ROBINS-I") {
    rob_gdata_long$colour <- dplyr::case_when(
      rob_gdata_long$domain == "Overall" & rob_gdata_long$judgement == "Critical" ~ rob_colours$overall_high_colour, 
      rob_gdata_long$domain == "Overall" & rob_gdata_long$judgement == "Low" ~ rob_colours$overall_low_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Critical" ~ rob_colours$critical_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Serious" ~ rob_colours$high_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Moderate" ~ rob_colours$concerns_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Low" ~ rob_colours$low_colour,
      TRUE ~ rob_colours$ni_colour
    )
  } else {
    # For ROB2, QUADAS-2, ROB2-Cluster, etc.
    rob_gdata_long$colour <- dplyr::case_when(
      rob_gdata_long$domain == "Overall" & rob_gdata_long$judgement == "High" ~ rob_colours$overall_high_colour,
      rob_gdata_long$domain == "Overall" & rob_gdata_long$judgement == "Low" ~ rob_colours$overall_low_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "High" ~ rob_colours$high_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Some concerns" ~ rob_colours$concerns_colour,
      rob_gdata_long$domain != "Overall" & rob_gdata_long$judgement == "Low" ~ rob_colours$low_colour,
      TRUE ~ rob_colours$ni_colour
    )
  }
  
  # Filter NA and assign column position (x)
  rob_gdata_long <- rob_gdata_long[!is.na(rob_gdata_long$judgement), ]
  
  # Create a consistent domain-to-position-x mapping
  domain_positions <- setNames(seq_along(columns), columns)
  
  # Apply extra spacing if needed (BEFORE using domain_positions)
  if(space_last == TRUE) {
    # Find the maximum x value (last column)
    max_x_val <- max(domain_positions, na.rm = TRUE)
    # Find the *name* of that column (e.g. "Overall")
    last_col_name <- names(domain_positions)[which.max(domain_positions)]
    # Apply the offset to that element in domain_positions
    domain_positions[last_col_name] <- max_x_val + 0.5
  }
  
  # Now use the domain_positions vector (possibly modified) for both
  rob_gdata_long$x <- domain_positions[rob_gdata_long$domain]
  
  # Ensure titles use the same x positions as the data
  titles <- data.frame(
    names = columns,
    y = max(rob_gdata_long$row_num, na.rm = TRUE) + 1,
    x = domain_positions # Now includes the offset for "Overall"!
  )
  
  # Create rectangles for the plot
  rectangles <- rob_gdata_long
  rectangles$xmin <- rectangles$x - 0.5
  rectangles$xmax <- rectangles$x + 0.5
  rectangles$ymin <- rectangles$row_num - 0.5
  rectangles$ymax <- rectangles$row_num + 0.5
  
  # Configure shapes by risk of bias tool
  if (rob_tool == "ROBINS-I") {
    shapes <- c("Critical" = 120, "Serious" = 120, "Moderate" = 45, "Low" = 43, "No information" = 63)
  } else if (rob_tool == "QUIPS") {
    shapes <- c("High" = 120, "Moderate" = 45, "Low" = 43, "No information" = 63)
  } else {
    shapes <- c("High" = 120, "Some concerns" = 45, "Low" = 43, "No information" = 63)
  }
  
  # Define a diamond shape for "Overall"
  overall_shape <- 23 # Code 23 is a diamond in ggplot2
  
  # Create the plot
  rob_plot <- ggplot2::ggplot(data = rob_gdata_long) +
    ggplot2::geom_rect(data = rectangles,
                       ggplot2::aes(xmin = .data$xmin,
                                    ymin = .data$ymin,
                                    xmax = .data$xmax,
                                    ymax = .data$ymax),
                       fill = "white",
                       colour = "#a9a9a9") +
    ggplot2::geom_point(size = 6, ggplot2::aes(x = .data$x, y = .data$row_num, colour = .data$colour)) +
    ggplot2::geom_point(size = 4.25, ggplot2::aes(x = .data$x, y = .data$row_num, shape = .data$judgement)) +
    ggplot2::scale_y_continuous(expand = c(0.3, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, (max(rob_gdata_long$x) + 1))) +
    ggplot2::scale_color_identity() +  # Use colors directly from the 'colour' column
    ggplot2::scale_shape_manual(values = shapes, na.translate = FALSE) +
    ggplot2::geom_text(data = titles, ggplot2::aes(label = .data$names, x = .data$x, y = .data$y),
                       family = font_family, fontface = "bold")
  
  return(rob_plot)
}


metafor_object_to_table <- function(rma,
                                    rob,
                                    add_tests = TRUE,
                                    RoB_Group_estimate = TRUE,
                                    subset_col = "Overall",
                                    rob_colour = "cochrane",
                                    rob_tool = "ROB2",
                                    subset_col_order = NULL){
  meta_data <- data.frame(
    Study    = rma$slab,
    yi       = rma$yi,
    vi       = rma$vi,
    est      = (rma$yi),
    ci_low   = (rma$yi - 1.96 * sqrt(rma$vi)),
    ci_high  = (rma$yi + 1.96 * sqrt(rma$vi)),
    n        = ifelse("n" %in% names(rma$data), rma$data$n, NA),
    stringsAsFactors = FALSE
  )
  print(colnames(rma$meta_data))
  
  
  if(!is.null(rma$data) &&
     all(c("pop_cohort_dataset_id", "report_id") %in% names(rma$data))){
    meta_data <- merge(meta_data,
                       rma$data[, c("slab", "pop_cohort_dataset_id", "report_id",  "n")],
                       sample_sizes,
                       by.x = "Study",
                       by.y = "slab",
                       all.x = TRUE)
  } else if(!is.null(rma$data) && "sample_size" %in% names(rma$data)){
    
    meta_data <- merge(meta_data,
                       rma$data[, c("slab", "pop_cohort_dataset_id", "report_id", "sample_size")],
                       by.x = "Study",
                       by.y = "slab",
                       all.x = TRUE)
    names(meta_data)[names(meta_data) == "sample_size"] <- "n"  
  } else {
    message("⚠️ Warning: Columns 'n' or 'sample_size' not found in rma$data. Proceeding without them.")
  }
  
  table <- merge(meta_data, rob, by = "Study")
  print(names(table))
  
  # After merge(), rename 'n' columns
  if ("n.x" %in% names(meta_data) && "n.y" %in% names(meta_data)) {
    meta_data$n <- ifelse(is.na(meta_data$n.x), meta_data$n.y, meta_data$n.x)
    meta_data <- meta_data[, !(names(meta_data) %in% c("n.x", "n.y"))]
  }
  
  
  # if ("n" %in% names(table)) {  #####
  #   table <- dplyr::select(table, Study, n, Weight, dplyr::everything())
  # } else {
  #   message("⚠️ ❌ Warning: column 'n' not found in the meta-analysis table.")
  #   table <- dplyr::select(table, Study, Weight, dplyr::everything())  # Avoid selection error
  # }
  
  #table <- dplyr::select(table, Study, n, Weight, dplyr::everything())
  
  if (!is.null(subset_col)) {
    if(!subset_col %in% names(table)){
      stop(paste0("Error: Column ", subset_col, " not found in the data."))
    }
    table[[subset_col]] <- stringr::str_to_sentence(table[[subset_col]])
    levels <- unique(table[[subset_col]])
    
    if(!(is.null(subset_col_order))){
      levels <- intersect(subset_col_order, levels)
    } else if(rob_tool %in% c("ROB2", "QUADAS-2", "ROB2-Cluster") & subset_col == "Overall"){
      levels <- intersect(c("High", "Some concerns", "Low", "No information"), levels)
    } else if(rob_tool == "Robins" & subset_col == "Overall"){
      levels <- intersect(c("Critical", "Serious", "Moderate", "Low", "No information"), levels)
    } else if(rob_tool == "QUIPS" & subset_col == "Overall"){
      levels <- intersect(c("High", "Moderate", "Low", "No information"), levels)
    }
    
    single_group <- ifelse(length(levels)==1, TRUE, FALSE)
    
    subset <- lapply(levels, function(level){
      dplyr::filter(table, !!as.symbol(subset_col) == level)
    })
    names(subset) <- levels
    
    subset_res <- lapply(levels, function(level){
      metafor_function(rma, data = subset[[level]])
    })
    names(subset_res) <- levels
    
    subset_tables <- lapply(levels, function(level){
      rbind(
        create_title_row(" "),
        dplyr::select(subset[[level]], Study, est, ci_low, ci_high),
        create_subtotal_row(subset_res[[level]], single_group = single_group, add_tests = add_tests)
      )
    })
    
    subset_table <- do.call("rbind", lapply(subset_tables, function(x) x))
  } else {
    levels <- ""
    subset_table <- rbind(
      create_title_row(""),
      dplyr::select(table, Study, est, ci_low, ci_high)
    )
  }
  
  ordered_table <- rbind(subset_table,
                         if (RoB_Group_estimate) {
                           create_subtotal_row(rma, "Overall", add_blank = FALSE)
                         })
  
  ordered_table$Study <- as.character(ordered_table$Study)
  ordered_table$Study <- ifelse(!(ordered_table$Study %in% levels) & ordered_table$Study != "Overall",
                                paste0("  ", ordered_table$Study),
                                ordered_table$Study)
  
  rob$Study <- paste0("  ", rob$Study)
  
  return(dplyr::left_join(ordered_table, rob, by = "Study", relationship = "many-to-many"))
}



####

select_rob_columns <- function(dataframe, tool){
  if(tool == "QUADAS-2"){
    return_data <- dplyr::select(dataframe, D1, D2, D3, D4, `Overall`)
  } else if(tool == "ROB1"){
    return_data <- dplyr::select(dataframe,
                                 RS = Random.sequence.generation.,
                                 A = Allocation.concealment.,
                                 BP = Blinding.of.participants.and.personnel.,
                                 BO = Blinding.of.outcome.assessment,
                                 I = Incomplete.outcome.data,
                                 SR = Selective.reporting.,
                                 Oth = Other.sources.of.bias.,
                                 `Overall` = `Overall`)
  } else if(tool == "ROB2"){
    return_data <- dplyr::select(dataframe, D1, D2, D3, D4, D5, `Overall`)
  } else if(tool == "ROB2-Cluster"){
    return_data <- dplyr::select(dataframe, D1, D1b, D2, D3, D4, D5, `Overall`)
  } else if(tool == "Robins"){
    return_data <- dplyr::select(dataframe, D1, D2, D3, D4, D5, D6, D7, `Overall`)
  } else if(tool == "QUIPS"){
    return_data <- dplyr::select(dataframe, D1, D2, D3, D4, D5, `Overall`)
  } else {
    stop("Tool is not supported.")
  }
  return(return_data)
}
#######

get_colour <- function(tool, colour) {
  rob_colours <- c()
  
  rob_colours$na_colour <- "#cccccc"
  
  if (tool == "ROB2" || tool == "ROB2-Cluster" || tool == "QUADAS-2") {
    if (length(colour) > 1) {
      rob_colours$low_colour <- colour[c(1)]
      rob_colours$concerns_colour <- colour[c(2)]
      rob_colours$high_colour <- colour[c(3)]
      rob_colours$ni_colour <- colour[c(4)]
    } else {
      if (colour == "colourblind") {
        rob_colours$low_colour <- "#fed98e"
        rob_colours$concerns_colour <- "#fe9929"
        rob_colours$high_colour <- "#d95f0e"
        rob_colours$ni_colour <- "#ffffff"
      }
      if (colour == "cochrane") {
        rob_colours$low_colour <- "#02C100"
        rob_colours$concerns_colour <- "#E2DF07"
        rob_colours$high_colour <- "#BF0000"
        rob_colours$ni_colour <- "#4EA1F7"
      }
    }
  } else {
    if (length(colour) > 1) {
      rob_colours$low_colour <- colour[c(1)]
      rob_colours$concerns_colour <- colour[c(2)]
      rob_colours$high_colour <- colour[c(3)]
      rob_colours$critical_colour <- colour[c(4)]
      rob_colours$ni_colour <- colour[c(5)]
    } else {
      if (colour == "colourblind") {
        rob_colours$low_colour <- "#fed98e"
        rob_colours$concerns_colour <- "#fe9929"
        rob_colours$high_colour <- "#d95f0e"
        rob_colours$critical_colour <- "#993404"
        rob_colours$ni_colour <- "#ffffff"
      }
      if (colour == "cochrane") {
        rob_colours$low_colour <- "#02C100"
        rob_colours$moderate_colour <- "#E2DF07"
        rob_colours$high_colour <- "#BF0000"
        rob_colours$critical_colour <- "#820000"
        rob_colours$ni_colour <- "#4EA1F7"
        rob_colours$overall_high_colour = "#FF8C00"
        rob_colours$overall_low_colour  = "#7FFF00"
      }
    }
  }
  
  return(rob_colours)
}

#############

rob_append_weights <- function(data, res){
  
  if (!("rma" %in% class(res))) {
    stop("Result objects need to be of class \"meta\" - output from metafor package functions")
  }
  
  # Extract weights
  weights <- data.frame(Study = names(stats::weights(res)),
                        Weight = stats::weights(res),
                        row.names = NULL)
  
  if (!is.null(res$data) && "n" %in% names(res$data)) {
    sample_sizes <- res$data[, c("slab", "n")]
    names(sample_sizes)[1] <- "Study"  # Rename slab column to Study
  } else {
    sample_sizes <- data.frame(Study = weights$Study, n = NA)
  }
  
  
  # Merge by Study name to create new dataframe
  rob_df <- dplyr::left_join(data, weights, by = "Study")
  rob_df <- dplyr::left_join(rob_df, sample_sizes, by = "Study")
  
  # Employ check to see if data has merged properly If a merge has failed, one
  # of the Weight cells will be NA, meaning the sum will also be NA
  if (is.na(sum(rob_df$Weight))) {
    stop(paste0("Problem with matching - weights do not equal 100. ",
                "Check that the names of studies are the same in the ROB ",
                "data and the res object (stored in slab)"))
  }
  
  return(rob_df)
}

print("rob_blobbogram")