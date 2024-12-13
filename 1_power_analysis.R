library(assertthat)
library(bcaboot)
library(boot)
library(fabricatr)
# library(tidyverse)
library(DeclareDesign)
library(table1)

#############################
# Synthetic data generation #
#############################
# For reproducibility, set a fixed random seed
set.seed(65086934)

# We'll treat this outcome as having only 5 categories combining all plant-based
# dishes as well as opt-outs in each arm as a single category.
arm_uniform =          c(.20, .2, .2, .2, .2)
arm_small_specific =   c(.17, .2, .2, .2, .23)
arm_med_specific =     c(.15, .2, .2, .2, .25)
arm_large_specific =   c(.08, .2, .2, .2, .32)
arm_med_non_specific = c(.175, .175, .175, .175, .3)

# Function to simulate the experiment
generate_synth_data = function(N, arm_0_P, arm_1_P){

  arm0 = tibble(
    arm = rep(0, N/2),
    Chose=draw_categorical(arm_0_P, N=N/2))

  arm1 = tibble(
      arm = rep(1, N/2),
      Chose=draw_categorical(arm_1_P, N=N/2))

  d = bind_rows(arm1, arm0) %>% mutate(
    ID = 1:N,
    meat_outcome_chicken_binary = as.numeric(Chose == 1),
    meat_outcome_not_chicken_binary = as.numeric(Chose %in% c(2,3,4)))

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


#####################################
# Power analysis of H2 using `boot` #
#####################################
H2_Specificity_CI = function(data, N_bootstraps) {

  # Helper function that generates the point estimate of the intervention effect
  calculate_statistics = function(data, bootstrap_sample_index) {
    data = data[bootstrap_sample_index,]

    Chicken_model = lm_robust(meat_outcome_chicken_binary ~ arm, data,
                              se_type='HC0')
    Meat_model = lm_robust(meat_outcome_not_chicken_binary ~ arm, data,
                           se_type='HC0')

    return(c(Chicken_mean=Chicken_model$coefficients['arm'],
             Chicken_var=Chicken_model$vcov['arm','arm'],

             Meat_mean=Meat_model$coefficients['arm'],
             Meat_var=Meat_model$vcov['arm','arm'],

             Chicken_minus_Meat=Chicken_model$coefficients['arm'] -
               Meat_model$coefficients['arm'])
           )
  }

  bs = boot(data, calculate_statistics, R=N_bootstraps, parallel='multicore')

  # Index is getting the point estimate (5) and standard error (2) via
  # `calculate_statistics`
  bs_ci = boot.ci(bs, type="bca", index=c(5,2))

  return(c(
    # Get upper and lower bounds for a 95% CI
    H2_Specificity_CI_lo=bs_ci$perc[4],
    H2_Specificity_CI_hi=bs_ci$perc[5])
  )

}

sims = simulate_H2(N_sims=10, N_participants=3332, N_bootstraps=10,
                   boot_function = H2_Specificity_CI,
                   arm_0_P = arm_uniform,
                   arm_1_P = arm_small_specific)



# XXX Broken Still raises an cryptic error:
# Error in bca.ci(boot.out, conf, index[1L], L = L, t = t.o, t0 = t0.o,  :
#                  estimated adjustment 'a' is NA

########################################
# Power analysis of H2 using `bcaboot` #
########################################
# This works, but the BCA implementation is significantly slower than `boot`,
# presumably because the later allows multicore parallelization
H2_Specificity_CI_depr = function(data, N_bootstraps) {

  # Helper function that generates the point estimate of the intervention effect
  calculate_statistics = function(data) {
    Chicken_model = lm_robust(meat_outcome_chicken_binary ~ arm, data, se_type='HC0')
    Meat_model = lm_robust(meat_outcome_not_chicken_binary ~ arm, data, se_type='HC0')

    # Interpretation: How many percentage points larger was the reduction in
    # chicken selection than for other meats.
    return(Chicken_model$coefficients['arm'] - Meat_model$coefficients['arm'])
  }

  bca_ci = bcajack(data, N_bootstraps, calculate_statistics)

  return(c(
    # Get the mean of the bootstrap estimates
    H2_Specificity_point=bca_ci$B.mean[2],
    # Get upper and lower bounds for a 95% CI
    H2_Specificity_CI_lo=bca_ci$lims['0.025', 'bca'],
    H2_Specificity_CI_hi=bca_ci$lims['0.975', 'bca'])
  )

}

sims = simulate_H2(5, 3332, N_bootstraps=1, H2_Specificity_CI_depr,
                   arm_0_P = arm_uniform,
                   arm_1_P = arm_med_specific)


#########
# Tests #
#########
# Check data generation is working properly
generate_synth_data(100000) %>% group_by(arm, Chose) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(arm) %>%
  mutate(Percentage = Count / sum(Count))