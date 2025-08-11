# helper functions
# Function to extract counts from data
get_counts_from_data <- function(dat) {
  complete_dat <- dat |> 
    filter(!is.na(meat_select) & !is.na(combined_arms))
  
  n_table <- table(complete_dat$combined_arms, complete_dat$meat_select)
  
  # Extract counts (positive outcome = plant choice = meat_select==0)
  list(
    n11 = as.numeric(n_table["1", "0"]),  # PMA + Plant
    n10 = as.numeric(n_table["1", "1"]),  # PMA + Meat
    n01 = as.numeric(n_table["0", "0"]),  # Control + Plant
    n00 = as.numeric(n_table["0", "1"]),  # Control + Meat
    n_retained = nrow(complete_dat)
  )
}

calculate_mvalue_type_II <- function(n11, n10, n01, n00,
                                     retention_rate,
                                     true_effect = 0.05,
                                     RDAY_R0 = 0) {
  
  # Calculate observed parameters
  n_treated <- n11 + n10
  n_control <- n01 + n00
  p1 <- n11 / n_treated  # P(outcome|treated,retained)
  p0 <- n01 / n_control  # P(outcome|control,retained)
  pa <- n_treated / (n_treated + n_control)  # P(treated|retained)
  
  observed_rd <- p1 - p0
  
  # Calculate lambda - what the RD among retained should be to support true effect
  lambda <- (1 - 1/retention_rate) * RDAY_R0 + true_effect / retention_rate
  
  cat("Observed RD:", round(observed_rd, 4), "\n")
  cat("Lambda (target RD):", round(lambda, 4), "\n")
  
  if (lambda <= observed_rd) {
    # Standard case - can use evalues.RD directly
    result <- evalues.RD(n11 = n11, n10 = n10, n01 = n01, n00 = n00, true = lambda)
    return(list(
      m_value = result$est.Evalue,
      scenario = "standard",
      observed_rd = observed_rd,
      lambda = lambda
    ))
  }
  
  # Type II error case - lambda > observed_rd
  # We need to create hypothetical data with effect = lambda
  # Then ask what confounding reduces it to observed_rd
  
  # Method: Adjust the control group to create lambda effect
  # Keep treatment group the same, solve for new p0
  p0_hyp <- p1 - lambda
  
  if (p0_hyp < 0) {
    # If impossible, adjust treatment group instead
    p1_hyp <- p0 + lambda
    if (p1_hyp > 1) {
      stop("Lambda too large - cannot be achieved with any reasonable data")
    }
    # Create hypothetical treatment group data
    n11_hyp <- round(n_treated * p1_hyp)
    n10_hyp <- n_treated - n11_hyp
    n01_hyp <- n01
    n00_hyp <- n00
  } else {
    # Create hypothetical control group data
    n01_hyp <- round(n_control * p0_hyp)
    n00_hyp <- n_control - n01_hyp
    n11_hyp <- n11
    n10_hyp <- n10
  }
  
  # Now use evalues.RD to find confounding that reduces lambda to observed_rd
  result <- evalues.RD(
    n11 = n11_hyp,
    n10 = n10_hyp,
    n01 = n01_hyp,
    n00 = n00_hyp,
    true = observed_rd
  )
  
  return(list(
    m_value = result$est.Evalue,
    scenario = "type_II_flipped",
    observed_rd = observed_rd,
    lambda = lambda,
    interpretation = paste0(
      "Confounding of ", round(result$est.Evalue, 2), 
      " or greater could mask a true effect of ", true_effect,
      " (with RDAY_R0 = ", RDAY_R0, ")"
    )
  ))
}

# Wrapper function that does everything
calculate_mvalue_from_data <- function(dat, 
                                       true_effect = 0.05,
                                       RDAY_R0 = 0,
                                       total_n = 6318) {
  

  counts <- get_counts_from_data(dat)
  retention_rate <- counts$n_retained / total_n
  
  calculate_mvalue_type_II(
    n11 = counts$n11,
    n10 = counts$n10,
    n01 = counts$n01,
    n00 = counts$n00,
    retention_rate = retention_rate,
    true_effect = true_effect,
    RDAY_R0 = RDAY_R0
  )
}
