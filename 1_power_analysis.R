library(assertthat)
library(bcaboot)
library(dplyr, warn.conflicts = FALSE)
library(estimatr)
library(fabricatr)

#############################
# Synthetic data generation #
#############################
# For reproducibility, set a fixed random seed
set.seed(65086934)

# We'll treat this outcome as having only 5 categories combining all plant-based
# dishes as well as opt-outs in each arm as a single category.
arm_uniform =          c(.20, .2, .2, .2, .2)
arm_small_specific =   c(.17, .2, .2, .2, .23) # +3% ATE entirely from chicken
arm_med_specific =     c(.15, .2, .2, .2, .25) # +5% ATE, entirely from chicken
arm_large_specific =   c(.08, .2, .2, .2, .32) # +12% ATE, entirely from chicken
arm_med_non_specific = c(.175, .175, .175, .175, .3) # + 10% ATE, from all categories

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
H2_Specificity_CI_2 = function(data, N_bootstraps) {
  
  # Helper function that generates the point estimate of the intervention effect
  calculate_statistics_2 = function(data) {
    Chicken_model = estimatr::lm_robust(meat_outcome_chicken_binary ~ arm, data, se_type='HC0')
    Meat_model = estimatr::lm_robust(meat_outcome_not_chicken_binary ~ arm, data, se_type='HC0')
    
    # Interpretation: How many percentage points larger was the reduction in
    # chicken selection than for other meats.
    return(Chicken_model$coefficients['arm'] - Meat_model$coefficients['arm'])
  }
  
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

# now run the simulations
sims = simulate_H2(5, 3332, N_bootstraps=10, H2_Specificity_CI_2,
                   arm_0_P = arm_uniform,
                   arm_1_P = arm_med_specific)

print(sims)

# Plot the difference between the BCA CIs and 'standard' CIs across different
# confidence levels. Note, they are quite close.
bcaplot(bcajack(generate_synth_data(3332, 
                                    arm_0_P = arm_uniform, 
                                    arm_1_P = arm_med_specific),
                500, calculate_statistics_2))


#########
# Tests #
#########
# Check data generation is working properly
generate_synth_data(100000,
                    arm_0_P = arm_uniform,
                    arm_1_P = arm_med_specific) |> group_by(arm, Chose) |>
  summarise(Count = n(), .groups = 'drop') |>
  group_by(arm) |>
  mutate(Percentage = Count / sum(Count))x

# determine sample size
# starting from
bcajack(x = generate_synth_data(N = 3332, 
                                arm_0_P = arm_uniform, 
                                arm_1_P = arm_med_specific), 
        500, 
        func = calculate_statistics_2)


#####################################
# NON-WORKING STUFF BELOW

#####################################
# library(boot)
# XXX Broken Still raises an cryptic error:
# Error in bca.ci(boot.out, conf, index[1L], L = L, t = t.o, t0 = t0.o,  :
#                  estimated adjustment 'a' is NA
#####################################
# Power analysis of H2 using `boot` #
#####################################
H2_Specificity_CI_1 = function(data, N_bootstraps) {
  
  # Helper function that generates the point estimate of the intervention effect
  calculate_statistics_1 = function(data, bootstrap_sample_index) {
    data = data[bootstrap_sample_index,]
    
    Chicken_model = lm_robust(meat_outcome_chicken_binary ~ arm, data,
                              se_type = 'HC0')
    Meat_model = lm_robust(meat_outcome_not_chicken_binary ~ arm, data,
                           se_type = 'HC0')
    
    return(c(Chicken_mean = Chicken_model$coefficients['arm'],
             Chicken_var = Chicken_model$vcov['arm','arm'],
             
             Meat_mean = Meat_model$coefficients['arm'],
             Meat_var = Meat_model$vcov['arm','arm'],
             
             Chicken_minus_Meat = Chicken_model$coefficients['arm'] -
               Meat_model$coefficients['arm'])
    )
  }
  
  bs = boot::boot(data, calculate_statistics_1, R=N_bootstraps, parallel='multicore')
  
  # Index is getting the point estimate (5) and standard error (2) via
  # `calculate_statistics`
  bs_ci = boot::boot.ci(bs, type="bca", index=c(5,2))
  
  return(c(
    # Get upper and lower bounds for a 95% CI
    H2_Specificity_CI_lo=bs_ci$perc[4],
    H2_Specificity_CI_hi=bs_ci$perc[5])
  )
  
}

# library(boot)

# error here 
# sims = simulate_H2(N_sims=10, N_participants=3332, N_bootstraps=10,
#                    boot_function = H2_Specificity_CI_1,
#                    arm_0_P = arm_uniform,
#                    arm_1_P = arm_small_specific)
