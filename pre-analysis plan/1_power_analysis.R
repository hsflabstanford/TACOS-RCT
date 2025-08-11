# libraries
library(assertthat)
library(bcaboot)
library(dplyr, warn.conflicts = FALSE)
library(estimatr)
library(fabricatr)

# For reproducibility, set a fixed random seed
set.seed(65086934)

# data generation functions
source('./functions.R')

#############################
# Simulations #
#############################
# (we will add 10% buffer but here we assume we lose that 10% to dropout etc.)

# 1st simulation: uniform vs medium effect size specifically drawn from chicken
arm_uniform =          c(.2, .2, .2, .2, .2)
arm_med_specific =     c(.15, .2, .2, .2, .25) 

# First simulation
first_sim <- simulate_H2(
  N_sims = 500, N_participants = 2600, 
  N_bootstraps = 10, 
  boot_function = H2_Specificity_CI_2,
  arm_0_P = arm_uniform,
  arm_1_P = arm_med_specific
)
paste0("first sim average: ", round(mean(first_sim$ci_width, na.rm = TRUE), 4))
paste0("first sim power: ", sum(first_sim$ci_width / 2 < 0.05, na.rm = T) / nrow(first_sim) * 100, "%")

# Second simulation: more chicken-heavy distribution
treatment_one_more_chicken <- c(0.4, 0.15, 0.15, 0.15, 0.15)
treatment_more_chicken_specific <- c(0.35, 0.15, 0.15, 0.15, 0.2)

second_sim <- simulate_H2(
  N_sims = 500, N_participants = 2600, 
  N_bootstraps = 10, 
  boot_function = H2_Specificity_CI_2,
  arm_0_P = treatment_one_more_chicken,
  arm_1_P = treatment_more_chicken_specific
)
paste0("second sim average: ", round(mean(second_sim$ci_width, na.rm = TRUE), 4))
paste0("second sim power: ", sum(second_sim$ci_width / 2 < 0.05, na.rm = T) / nrow(first_sim) * 100, "%")

# Third simulation: with plant-based/opt-out at a much lower percentage to start
treatment_one_more_meat <- c(0.24, 0.24, 0.24, 0.24, 0.04)
treatment_low_plant_based <- c(0.19, 0.24, 0.24, 0.24, 0.09)

third_sim <- simulate_H2(
  N_sims = 500, N_participants = 2600, 
  N_bootstraps = 10, 
  boot_function = H2_Specificity_CI_2,
  arm_0_P = treatment_one_more_meat,
  arm_1_P = treatment_low_plant_based
)
paste0("third sim average: ", round(mean(third_sim$ci_width, na.rm = TRUE), 4))
paste0("third sim power: ", sum(third_sim$ci_width / 2 < 0.05, na.rm = T) / nrow(third_sim) * 100, "%")

# Fourth simulation: assume high baseline rates of opt-out/vegetarian
treatment_one_more_veg <- c(0.125, 0.125, 0.125, 0.125, 0.5)
treatment_two_plant_based <- c(0.075, 0.125, 0.125, 0.125, 0.55)

fourth_sim <- simulate_H2(
  N_sims = 500, N_participants = 2600, 
  N_bootstraps = 10, 
  boot_function = H2_Specificity_CI_2,
  arm_0_P = treatment_one_more_veg,
  arm_1_P = treatment_two_plant_based
)
paste0("fourth sim average: ", round(mean(fourth_sim$ci_width, na.rm = TRUE), 4))
paste0("fourth sim power: ", sum(fourth_sim$ci_width / 2 < 0.05, na.rm = T) / nrow(fourth_sim) * 100, "%")

# Fifth simulation: very unbalanced for whatever reason
treatment_one_unbalanced <- c(0.1, 0.4, 0.05, 0.4, 0.05)
treatment_two_unbalanced <- c(0.05, 0.4, 0.05, 0.4, 0.1)

fifth_sim <- simulate_H2(
  N_sims = 500, N_participants = 2400, 
  N_bootstraps = 10, 
  boot_function = H2_Specificity_CI_2,
  arm_0_P = treatment_one_unbalanced,
  arm_1_P = treatment_two_unbalanced
)
paste0("fifth sim average: ", round(mean(fifth_sim$ci_width, na.rm = TRUE), 4))
paste0("fifth sim power: ", sum(fifth_sim$ci_width / 2 < 0.05, na.rm = T) / nrow(fifth_sim) * 100, "%")

combined_data <- bind_rows(first_sim |> mutate(source = 'first sim'),
                           second_sim |> mutate(source = 'second sim'),
                           third_sim |> mutate(source = 'third sim'),
                           fourth_sim |> mutate(source = 'fourth sim'),
                           fifth_sim |> mutate(source = 'fifth sim')
                           ) |> 
  mutate(source = factor(source, levels = c("first sim", "second sim", "third sim", "fourth sim", "fifth sim")))

paste0("overall sim average: ", round(mean(combined_data$ci_width, na.rm = TRUE), 4))
paste0("overall sim power: ", sum(combined_data$ci_width / 2 < 0.05, na.rm = T) / nrow(combined_data) * 100, "%")
paste0("NAs: ", sum(is.na(combined_data$ci_width)))
paste0("NA percentage: ", sum(is.na(combined_data$ci_width)) / nrow(combined_data) * 100, "%")

# now by groups:
# a previous run of these simulations is saved as the following
# combined_data <- read.csv('./sim-data-01-09-2025.csv') |>
# mutate(source = factor(source, levels = c("first sim", "second sim", "third sim", "fourth sim", "fifth sim")))

combined_data |> 
  group_by(source) |> 
  summarise(
    avg_ci_width = round(mean(ci_width, na.rm = TRUE), 4),                  # Average CI width
    power = round(sum(ci_width / 2 < 0.05, na.rm = TRUE) / n() * 100, 2)   # Power as a percentage
  )

# finally, a 10 pp ATE sim
treatment_one_most_chicken <- c(0.4, 0.15, 0.15, 0.15, 0.15)
treatment_most_chicken_specific <- c(0.3, 0.15, 0.15, 0.15, 0.25)

sixth_sim <- simulate_H2(
  N_sims = 500, N_participants = 2600, 
  N_bootstraps = 10, 
  boot_function = H2_Specificity_CI_2,
  arm_0_P = treatment_one_most_chicken,
  arm_1_P = treatment_most_chicken_specific
)

paste0("sixth sim average: ", round(mean(sixth_sim$ci_width, na.rm = TRUE), 4))
paste0("sixth sim power: ", sum(sixth_sim$ci_width /2 < 0.1, na.rm = T) / nrow(sixth_sim) * 100, "%")

##################i###########
# Verification #
#############################

# Check data generation is working properly
synth_data = generate_synth_data(2400,
                    arm_0_P = arm_uniform,
                    arm_1_P = arm_med_specific) 

# just look at the main dataset
result <- bcajack(x = synth_data, B = 500, func = calculate_statistics_2)
result

# Plot the difference between the BCA CIs and 'standard' CIs across different
# confidence levels. Note, they are quite close.
bcaplot(bcajack(generate_synth_data(2400, 
                                    arm_0_P = arm_uniform, 
                                    arm_1_P = arm_med_specific),
                500, calculate_statistics_2))