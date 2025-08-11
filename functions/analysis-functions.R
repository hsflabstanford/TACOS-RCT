# =========================================================================
# Analysis Functions for Hypothesis Testing
# =========================================================================

# Simple function to run regression and format results consistently
# Params: data, formula, filter_expr (optional)
# Returns: Tidy data frame of regression results with confidence intervals
h1_analysis <- function(data, formula, filter_expr = NULL) {
  # Apply filter if provided
  if (!is.null(filter_expr)) {
    data <- data |> dplyr::filter(!!rlang::parse_expr(filter_expr))
  }
  
  # Run model and tidy results in one pipe
  lm_robust(formula, data) |>
    tidy() |>
    dplyr::mutate(across(where(is.numeric), ~round(., 5))) |>
    dplyr::select(-outcome)
}

# Function to run all H1 analyses on a dataset
# Params: data, outcome_var (default: "meat_select")
# Returns: List containing results for each H1 hypothesis (a-d)

run_h1_analyses <- function(data, outcome_var = "meat_select") {
  # First, we need the compare_arms function (assuming it's fixed to have data first)
  compare_arms <- function(data, outcome_var, arm1, arm2) {
    formula <- as.formula(paste(outcome_var, "~ treatment_arm"))
    filter_expr <- paste("treatment_arm ==", arm1, "| treatment_arm ==", arm2)
    h1_analysis(data, formula, filter_expr)
  }
  
  # Run all four H1 analyses
  h1a <- compare_arms(data, outcome_var, 1, 2)
  h1b <- compare_arms(data, outcome_var, 1, 3)
  h1c <- h1_analysis(data, as.formula(paste(outcome_var, "~ PMA_count")))
  h1d <- h1_analysis(data, as.formula(paste(outcome_var, "~ combined_arms")))
  
  # Return all results in a list
  return(list(
    h1a = h1a,
    h1b = h1b,
    h1c = h1c,
    h1d = h1d
  ))
}

# Function to test both H2a and H2b hypotheses
# Params: data, bootstrap_reps (default: 1000), sig_level (default: 0.05)
# Returns: List with model results, bootstrap results, and summary statistics
test_h2_hypotheses <- function(data, bootstrap_reps = 1000, sig_level = 0.05) {
  # Define helper function for bootstrapping within the main function
  calculate_statistics <- function(data, indices = NULL) {
    # If indices are provided (for bootstrap), use them to subset the data
    if (!is.null(indices)) {
      data <- data[indices, ]
    }
    
    # Model for chicken selection
    cm <- lm_robust(chicken_select ~ arm, data, se_type = 'HC0')
    
    # Model for other meat selection
    om <- lm_robust(other_meat_select ~ arm, data, se_type = 'HC0')
    
    # Calculate the difference in coefficients
    return(cm$coefficients['arm'] - om$coefficients['arm'])
  }
  
  # Run models
  chicken_model <- lm_robust(chicken_select ~ arm, data, se_type = 'HC0')
  other_meat_model <- lm_robust(other_meat_select ~ arm, data, se_type = 'HC0')
  
  # Calculate the effect difference directly
  specificity_difference <- chicken_model$coefficients['arm'] - other_meat_model$coefficients['arm']
  
  # Run bootstrap for confidence interval
  bca_result <- bcajack(data, B = bootstrap_reps, func = calculate_statistics)
  
  # Create a data frame of core results for easy display with kable
  core_results <- data.frame(
    Hypothesis = c("H2a (Chicken)", "H2b (Specificity)"),
    Effect = c(chicken_model$coefficients['arm'], specificity_difference),
    `P-value` = c(chicken_model$p.value['arm'], NA),
    `CI Lower` = c(chicken_model$conf.low['arm'], bca_result$lims['0.025', 'bca']),
    `CI Upper` = c(chicken_model$conf.high['arm'], bca_result$lims['0.975', 'bca']),
    Supported = c(
      chicken_model$p.value['arm'] < sig_level & chicken_model$coefficients['arm'] < 0,
      bca_result$lims['0.975', 'bca'] < 0
    )
  )
  
  # Return a list containing all components
  return(list(
    chicken_model = chicken_model,
    other_meat_model = other_meat_model,
    bca_result = bca_result,
    specificity_difference = specificity_difference,
    core_results = core_results,
    
    # Extract numeric values for inline code
    H2a_effect = chicken_model$coefficients['arm'],
    H2a_pvalue = chicken_model$p.value['arm'],
    H2a_CI_lower = chicken_model$conf.low['arm'],
    H2a_CI_upper = chicken_model$conf.high['arm'],
    H2b_effect = specificity_difference,
    H2b_CI_lower = bca_result$lims['0.025', 'bca'],
    H2b_CI_upper = bca_result$lims['0.975', 'bca']
  ))
}

# Function to prepare data for H2 analysis
# Params: data frame containing untidied variables
# Returns: Data frame prepared for H2 analysis
prepare_h2_data <- function(data) {
  data |>
    filter(treatment_arm %in% c("2", "3")) |>
    mutate(
      chicken_select = as.numeric(taco_choice == "Chicken"),
      other_meat_select = as.numeric(taco_choice %in% c("Beef barbacoa", "Carnitas", "Steak")),
      arm = as.numeric(treatment_arm == "3")  # Arm 3 = 1, Arm 2 = 0
    )
}

# I should probably make this more general (e.g. group by something else)
grouped_sum_tab <- function(.data, var) {
  # Capture the variable expression
  var_expr <- enquo(var)
  var_name <- quo_name(var_expr)
  
  # Create a temporary column with the result of the expression
  .data |>
    # We're receiving an already grouped dataframe
    # Count within each group
    count(!!var_expr, .drop = FALSE) |>
    # Pivot the result
    pivot_wider(
      id_cols = !!var_expr,
      names_from = treatment_arm,
      values_from = n,
      values_fill = 0
    ) |>
    # Add a total column
    mutate(Total = rowSums(across(where(is.numeric)))) |>
    # Rename the first column to match what was passed in
    rename_with(~ var_name, 1)
}


robustness_results <- function(model_object) {
  # Helper function to format a single row of tidy output into the desired string
  format_single_h1_report_string <- function(h1_data_row) { # Renamed input for clarity
    # Extract values from the *single row*
    estimate_val <- h1_data_row$estimate
    ci_low_val <- h1_data_row$conf.low
    ci_high_val <- h1_data_row$conf.high
    p_value_val <- h1_data_row$p.value
    
    # Round and multiply by 100 for percentage points
    estimate_pp <- round(estimate_val * 100, 2)
    ci_low_pp <- round(ci_low_val * 100, 2)
    ci_high_pp <- round(ci_high_val * 100, 2)
    
    # Format p-value (e.g., .26, or < .001)
    p_value_formatted <- format.pval(p_value_val, digits = 2, eps = 0.001, scientific = FALSE)
    
    # Construct the final formatted string
    paste0(
      estimate_pp, " (95% CIs: [", ci_low_pp, ", ", ci_high_pp, "], p = ", p_value_formatted, ")"
    )
  }
  
  # Access the specific tibbles (h1a and h1b) from the input list
  h1a_full_tibble <- model_object$h1a
  h1b_full_tibble <- model_object$h1b
  
  # Filter to get the row corresponding to treatment_arm2 from h1a
  h1a_treatment_row <- h1a_full_tibble |>
    dplyr::filter(term == "treatment_arm2")
  
  # Filter to get the row corresponding to treatment_arm3 from h1b
  h1b_treatment_row <- h1b_full_tibble |>
    dplyr::filter(term == "treatment_arm3")
  
  # Use the helper function to format each *filtered* tibble (which is now a single row)
  formatted_h1a_string <- format_single_h1_report_string(h1a_treatment_row)
  formatted_h1b_string <- format_single_h1_report_string(h1b_treatment_row)
  
  # Print the results in the requested format
  cat("h1a: ", formatted_h1a_string, "\n", sep = "")
  cat("h1b: ", formatted_h1b_string, "\n", sep = "")
}
