library(assertthat)
library(bcaboot)
library(boot)
library(dplyr)
library(estimatr)
library(fabricatr)
library(tibble)

#############################
# Synthetic data generation #
#############################
set.seed(65086934)

arm_uniform =          c(.20, .2, .2, .2, .2)
arm_small_specific =   c(.17, .2, .2, .2, .23)
arm_med_specific =     c(.15, .2, .2, .2, .25)
arm_large_specific =   c(.08, .2, .2, .2, .32)
arm_med_non_specific = c(.175, .175, .175, .175, .3)

# Generate synthetic data for one simulated experiment
generate_synth_data = function(N, arm_0_P, arm_1_P){
  arm0 = tibble(
    arm = rep(0, N/2),
    Chose = fabricatr::draw_categorical(arm_0_P, N=N/2)
  )
  
  arm1 = tibble(
    arm = rep(1, N/2),
    Chose = fabricatr::draw_categorical(arm_1_P, N=N/2)
  )
  
  d = bind_rows(arm1, arm0) |> 
    mutate(
      ID = 1:N,
      meat_outcome_chicken_binary = as.numeric(Chose == 1),
      meat_outcome_not_chicken_binary = as.numeric(Chose %in% c(2,3,4))
    )
  
  return(d)
}

####################
# Study simulation #
####################
simulate_H2 = function(
    N_sims,
    N_participants,
    N_bootstraps,
    boot_function,
    arm_0_P,
    arm_1_P
){
  sims = tibble()
  
  for (i in 1:N_sims){
    d = generate_synth_data(N_participants, arm_0_P, arm_1_P)
    H2_result = boot_function(d, N_bootstraps)
    ci_95_width = abs(H2_result['H2_Specificity_CI_hi'] - H2_result['H2_Specificity_CI_lo'])
    
    sim = tibble(
      sim_ID = i,
      ci_width = ci_95_width
    )
    
    sims = bind_rows(sims, sim)
  }
  
  return(sims)
}

#####################################
# Power analysis of H2 using `boot` #
#####################################
H2_Specificity_CI_1 = function(data, N_bootstraps) {
  
  calculate_statistics_1 = function(data, bootstrap_sample_index) {
    data = data[bootstrap_sample_index,]
    
    Chicken_model = lm_robust(meat_outcome_chicken_binary ~ arm, data, se_type='HC0')
    Meat_model = lm_robust(meat_outcome_not_chicken_binary ~ arm, data, se_type='HC0')
    
    return(c(Chicken_mean = Chicken_model$coefficients['arm'],
             Chicken_var  = Chicken_model$vcov['arm','arm'],
             Meat_mean    = Meat_model$coefficients['arm'],
             Meat_var     = Meat_model$vcov['arm','arm'],
             Chicken_minus_Meat = Chicken_model$coefficients['arm'] - Meat_model$coefficients['arm']))
  }
  
  bs = boot(data, calculate_statistics_1, R=N_bootstraps)
  
  # Request BCa interval for the 5th statistic (Chicken_minus_Meat)
  bs_ci = boot.ci(bs, type="bca", index=5)
  
  # Extract BCa interval from bs_ci$bca
  # According to boot.ci documentation, for BCa intervals:
  #   The lower and upper bounds for the default conf (95%) are in bca[4] and bca[5].
  
  return(c(
    H2_Specificity_CI_lo = bs_ci$bca[4],
    H2_Specificity_CI_hi = bs_ci$bca[5]
  ))
}

########################################
# Power analysis of H2 using `bcaboot` #
########################################
calculate_statistics_2 = function(data) {
  Chicken_model = lm_robust(meat_outcome_chicken_binary ~ arm, data, se_type='HC0')
  Meat_model = lm_robust(meat_outcome_not_chicken_binary ~ arm, data, se_type='HC0')
  
  return(Chicken_model$coefficients['arm'] - Meat_model$coefficients['arm'])
}

H2_Specificity_CI_2 = function(data, N_bootstraps) {
  bca_ci = bcajack(data, N_bootstraps, calculate_statistics_2)
  
  return(c(
    H2_Specificity_point = bca_ci$B.mean[2],
    H2_Specificity_CI_lo = bca_ci$lims['0.025', 'bca'],
    H2_Specificity_CI_hi = bca_ci$lims['0.975', 'bca']
  ))
}

# Example simulation with a reasonable number of bootstrap replications
# Note: 500 is still somewhat low for final inference, but it's much better than 1.
sims = simulate_H2(
  N_sims = 5, 
  N_participants = 3332, 
  N_bootstraps = 500, 
  boot_function = H2_Specificity_CI_2,
  arm_0_P = arm_uniform,
  arm_1_P = arm_med_specific
)
print(sims)

# Diagnostic plot to compare BCa intervals
bcaplot(bcajack(generate_synth_data(3332, arm_0_P=arm_uniform, arm_1_P=arm_med_specific),
                500, calculate_statistics_2))

#########
# Tests #
#########
generate_synth_data(100000,
                    arm_0_P = arm_uniform,
                    arm_1_P = arm_med_specific) |> 
  group_by(arm, Chose) |>
  summarise(Count = n(), .groups = 'drop') |>
  group_by(arm) |>
  mutate(Percentage = Count / sum(Count))
