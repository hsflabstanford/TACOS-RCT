# =========================================================================
# Formatting Functions for Hypothesis Testing Results
# =========================================================================

# Format H1 results into separate tables by hypothesis
# Params: 
#   results_list - List of H1 results from multiple datasets
#   dataset_names - Optional vector of display names (default: uses names from results_list)
#   sig_level - Significance level (default: 0.05)
# Returns: List containing tables by hypothesis, all results, comparison view
format_h1_results <- function(results_list, dataset_names = NULL, sig_level = 0.05) {
  # Define hypotheses information
  hypotheses <- c("h1a", "h1b", "h1c", "h1d")
  terms <- c("treatment_arm2", "treatment_arm3", "PMA_count", "combined_arms")
  
  # Setup dataset names
  if (is.null(dataset_names)) {
    dataset_names <- names(results_list)
  } else if (length(dataset_names) != length(results_list)) {
    warning("Length of dataset_names doesn't match results_list; using default names")
    dataset_names <- names(results_list)
  }
  name_map <- stats::setNames(dataset_names, names(results_list))
  
  # Extract results and build a single data frame
  results_df <- data.frame()
  
  for (ds_name in names(results_list)) {
    subset_label <- name_map[ds_name]
    ds_results <- results_list[[ds_name]]
    
    # Get sample sizes for this dataset
    sample_n <- if (!is.null(ds_results$N)) ds_results$N else NA
    
    for (i in seq_along(hypotheses)) {
      h <- hypotheses[i]
      term <- terms[i]
      
      if (h %in% names(ds_results)) {
        result_df <- ds_results[[h]]
        term_row <- which(result_df$term == term)
        
        if (length(term_row) > 0) {
          row_data <- data.frame(
            subset = subset_label,
            hypothesis = h,
            coefficient = result_df$estimate[term_row],
            std_error = result_df$std.error[term_row],
            p_value = result_df$p.value[term_row],
            conf_low = if("conf.low" %in% names(result_df)) result_df$conf.low[term_row] else NA,
            conf_high = if("conf.high" %in% names(result_df)) result_df$conf.high[term_row] else NA,
            n = sample_n,  # Sample size
            significant = result_df$p.value[term_row] < sig_level,
            stringsAsFactors = FALSE
          )
          
          results_df <- rbind(results_df, row_data)
        }
      }
    }
  }
  
  # Create separate tables by hypothesis
  h1a <- results_df[results_df$hypothesis == "h1a", ]
  h1b <- results_df[results_df$hypothesis == "h1b", ]
  h1c <- results_df[results_df$hypothesis == "h1c", ]
  h1d <- results_df[results_df$hypothesis == "h1d", ]
  
  # Create a wide-format table for coefficients (datasets x hypotheses)
  wide_table <- reshape2::dcast(
    results_df, 
    subset ~ hypothesis, 
    value.var = "coefficient"
  )
  
  # Return all formats
  return(list(
    # Individual hypothesis tables
    h1a = h1a,
    h1b = h1b,
    h1c = h1c,
    h1d = h1d,
    
    # All results in one data frame (long format)
    all_results = results_df,
    
    # Side-by-side comparison (wide format)
    comparison = wide_table
  ))
}

# Format H2a results for presentation
# Params:
#   results_list - List of H2 results from multiple datasets
#   dataset_names - Optional vector of display names (default: uses names from results_list)
#   sig_level - Significance level (default: 0.05)
# Returns: Data frame with formatted H2a results
format_h2a_results <- function(results_list, dataset_names = NULL, sig_level = 0.05) {
  # Setup dataset names
  if (is.null(dataset_names)) {
    dataset_names <- names(results_list)
  } else if (length(dataset_names) != length(results_list)) {
    warning("Length of dataset_names doesn't match results_list; using default names")
    dataset_names <- names(results_list)
  }
  name_map <- stats::setNames(dataset_names, names(results_list))
  
  # Extract values for each dataset directly into a data frame
  h2a_results <- do.call(rbind, lapply(names(results_list), function(ds_name) {
    result <- results_list[[ds_name]]
    
    if (!is.null(result$chicken_model) && !is.null(result$chicken_model$df)) {
      data.frame(
        subset = name_map[ds_name],
        effect = result$chicken_model$coefficients["arm"],
        p_value = result$chicken_model$p.value["arm"],
        ci_lower = result$chicken_model$conf.low["arm"],
        ci_upper = result$chicken_model$conf.high["arm"],
        df = result$chicken_model$df["arm"],  # Correctly accessing lowercase df
        significant = result$chicken_model$p.value["arm"] < sig_level & 
          result$chicken_model$coefficients["arm"] < 0,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }))
  
  # Reset row names and return the simple table
  if (!is.null(h2a_results)) {
    rownames(h2a_results) <- NULL
  }
  return(h2a_results)
}

# Format H2b results for presentation
# Params:
#   results_list - List of H2 results from multiple datasets
#   dataset_names - Optional vector of display names (default: uses names from results_list)
#   sig_level - Significance level (default: 0.05)
# Returns: Data frame with formatted H2b results
format_h2b_results <- function(results_list, dataset_names = NULL, sig_level = 0.05) {
  # Setup dataset names
  if (is.null(dataset_names)) {
    dataset_names <- names(results_list)
  } else if (length(dataset_names) != length(results_list)) {
    warning("Length of dataset_names doesn't match results_list; using default names")
    dataset_names <- names(results_list)
  }
  name_map <- stats::setNames(dataset_names, names(results_list))
  
  # Extract values for each dataset directly into a data frame
  h2b_results <- do.call(rbind, lapply(names(results_list), function(ds_name) {
    result <- results_list[[ds_name]]
    
    # Check components exist
    if (!is.null(result$specificity_difference) && 
        !is.null(result$bca_result) && 
        !is.null(result$bca_result$lims) &&
        !is.null(result$chicken_model) &&
        !is.null(result$chicken_model$df)) {
      
      # Extract df correctly - using lowercase
      model_df <- result$chicken_model$df["arm"]
      
      data.frame(
        subset = name_map[ds_name],
        effect = result$specificity_difference,
        ci_lower = result$bca_result$lims["0.025", "bca"],
        ci_upper = result$bca_result$lims["0.975", "bca"],
        df = model_df,  # Include the degrees of freedom
        supported = result$bca_result$lims["0.975", "bca"] < 0,
        stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  }))
  
  # Reset row names and return the simple table
  if (!is.null(h2b_results)) {
    rownames(h2b_results) <- NULL
  }
  return(h2b_results)
}