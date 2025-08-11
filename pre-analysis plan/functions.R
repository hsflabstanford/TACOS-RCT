# functions.R

#############################
# Synthetic data generation #
#############################
# Function to simulate the experiment. arm_0_P take a length-5 probability
# vector representing the distribution of selections in the control arm. 
# arm_1_P is the same as arm_0_P but for the treatment arm
generate_synth_data = function(N, arm_0_P, arm_1_P){
  
  arm0 = tibble(
    arm = rep(0, N/2),
    Chose=fabricatr::draw_categorical(arm_0_P, N = N/2))
  
  arm1 = tibble(
    arm = rep(1, N/2),
    Chose=fabricatr::draw_categorical(arm_1_P, N = N/2))
  
  d = bind_rows(arm1, arm0) |> mutate(
    ID = 1:N,
    meat_outcome_chicken_binary = as.numeric(Chose == 1),
    meat_outcome_not_chicken_binary = as.numeric(Chose %in% c(2, 3, 4)))
  
  return(d)
}


####################
# Study simulation #
####################
simulate_H2 = function(
    N_sims, # Number of times to simulate the experiment
    N_participants, # Number of participants in the experiment
    N_bootstraps, # Number of times to run the bootstrap for inference
    boot_function, # Function to implement the bootstrap (two different packages)
    arm_0_P, # Length-5 probability vector representing the distribution of selections in the control arm
    arm_1_P # Same as arm_0_P but for the treatment arm
){
  
  sims = tibble()
  
  for (i in 1:N_sims){
    
    d = generate_synth_data(N_participants, arm_0_P, arm_1_P)
    
    H2_result = boot_function(d, N_bootstraps)
    ci_95_width = abs(H2_result['H2_Specificity_CI_hi'] -
                        H2_result['H2_Specificity_CI_lo'])
    
    sim = tibble(
      sim_ID=i,
      ci_width=ci_95_width,
    )
    
    sims = bind_rows(sims, sim)
  }
  
  return(sims)
}

########################################
# Power analysis of H2 using `bcaboot` #
########################################
# Helper function that generates the point estimate of the intervention effect
calculate_statistics_2 = function(data) {
  Chicken_model = estimatr::lm_robust(meat_outcome_chicken_binary ~ arm, data, se_type='HC0')
  Meat_model = estimatr::lm_robust(meat_outcome_not_chicken_binary ~ arm, data, se_type='HC0')
  
  # Interpretation: How many percentage points larger was the reduction in
  # chicken selection than for other meats.
  return(Chicken_model$coefficients['arm'] - Meat_model$coefficients['arm'])
}

H2_Specificity_CI_2 = function(data, N_bootstraps) {
  
  
  # This works, but the BCA implementation is significantly slower than `boot`,
  # presumably because `boot` allows multicore parallelization
  bca_ci = bcaboot::bcajack(data, N_bootstraps, calculate_statistics_2)
  
  return(c(
    # Get the mean of the bootstrap estimates
    H2_Specificity_point = bca_ci$B.mean[2],
    # Get upper and lower bounds for a 95% CI
    H2_Specificity_CI_lo = bca_ci$lims['0.025', 'bca'],
    H2_Specificity_CI_hi = bca_ci$lims['0.975', 'bca'])
  )
}
