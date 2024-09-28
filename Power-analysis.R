library(assertthat)
library('boot')
library("DeclareDesign")
library(fabricatr)
library("logitr")
library(nnet)
library("pwr")
library("tidyverse")
library("lmtest")
library("sandwich")

# Description of study arms
# https://docs.google.com/spreadsheets/d/10PaZyi6ZnENczE7jCKRfyDxIv_7LTSXFQIc5nFp69fI/edit#gid=1059021257
# We'll treat this as a 5 x 3, combining all plant-based dishes in each arm
# to form a single category
P_large_unif_change = matrix(
  c(.2 , .2 , .2 , .2 ,  .2,
    .175, .175, .175, .175, .3,
    .2, .2, .2, .2, .2),
  ncol=5, byrow=TRUE)

P_large_single_change = matrix(
  c(.2 , .2 , .2 , .2 ,  .2,
    .12, .2, .2, .2, .28,
    .08, .2, .2, .2, .32),
  ncol=5, byrow=TRUE)

P_small_change = matrix(
  c(.2, .2, .2, .2, .2,
    .17, .2, .2, .2, .23,
    .17, .2, .2, .2, .23),
  ncol=5, byrow=TRUE)

P_med_single_change = matrix(
  c(.2, .2, .2, .2, .2,
    .15, .2, .2, .2, .25,
    .15, .2, .2, .2, .25),
  ncol=5, byrow=TRUE)

P_med_single_one_arm_change = matrix(
  c(.2, .2, .2, .2, .2,
    .2, .2, .2, .2, .2,
    .15, .2, .2, .2, .25),
  ncol=5, byrow=TRUE)

P_small_single_one_arm_change = matrix(
  c(.2, .2, .2, .2, .2,
    .2, .2, .2, .2, .2,
    .17, .2, .2, .2, .23),
  ncol=5, byrow=TRUE)


# Chi-squared test power analysis ##############################################
power = function(P) {
  P = P/sum(P)

  # Test that the matrix sums to 1 (note, we use a 1e-10 to allow very small
  # deviations)
  if (sum(P) - 1 > 1e-10) {
    stop("Probability matrix does not sum to 1")
  }

  print("Effect size:")
  print(ES.w2(P))

  pwr.chisq.test(w=ES.w2(P), df=prod(dim(P) - 1), power=0.9)$N
}

power(P_small_change)

power(P_large_single_change)
power(P_med_single_change)

# Scenario 1
power(P_small_single_one_arm_change)
# Scenario 2
power(P_large_unif_change)
# Secnario 3
power(P_med_single_one_arm_change)


# Sample size estimation based on restaurant owner's needs
100 / 7 *.7




# Fake data generation #########################################################
generate_fake_data = function(N){

  arm1= tibble(
      Arm = rep(1, N/2),
      Chose=draw_categorical(c(.17, .2, .2, .2, .23), N=N/2))
  arm0 = tibble(
      Arm = rep(0, N/2),
      Chose=draw_categorical(c(.2, .2, .2, .2, .2), N=N/2))

  d = bind_rows(arm1, arm0) %>% mutate(
    ID = 1:N,
    Chose_Chicken = as.numeric(Chose == 1),
    Chose_Meat = as.numeric(Chose %in% c(2,3,4)))

  return(d)
}

calculate_statistics = function(data, bootstrap_sample_index) {
  data = data[bootstrap_sample_index,]
  #Chicken_coef = broom::tidy(lm(Chose_Chicken ~ Arm, data)) %>% filter(term=="Arm")
  #Meat_coef = broom::tidy(lm(Chose_Meat ~ Arm, data)) %>% filter(term=="Arm")
  Chicken_coef = lm_robust(Chose_Chicken ~ Arm, data, se_type='HC0') %>% filter(term=="Arm")
  Meat_coef = lm_robust(Chose_Meat ~ Arm, data, se_type='HC0') %>% filter(term=="Arm")

  return(c(Chicken_mean=Chicken_coef$estimate,
           #TODO This should be the variance
           Chicken_se=Chicken_coef$std.error,
           Meat_mean=Meat_coef$estimate,
           Meat_se=Meat_coef$std.error,
           Chicken_minus_Meat=Chicken_coef$estimate - Meat_coef$estimate))
}


simulate_study = function(N_sims, N_samples){

  sims = tibble()
  for (i in 1:N_sims){
    print(i)
    d = generate_fake_data(N_samples)

    bs = boot(d, calculate_statistics, R=2000, parallel='multicore')

    # Index is getting the point estimate (5) and standard error (2) via
    # `calculate_statistics`
    bs_ci = boot.ci(bs, type="perc", index=c(5,2))

    sims = bind_rows(sims, tibble(
      sim_ID=i,
      ci_width=bs_ci$perc[5] - bs_ci$perc[4]))
  }

  return(sims)
}

sims = simulate_study(1, 3332)



# super simple testing setting up trying to get BCa to work...
calculate_statistics = function(data, bootstrap_sample_index) {
  data = data[bootstrap_sample_index,]
  Chicken_coef = broom::tidy(lm(Chose_Chicken ~ Arm, data)) %>% filter(term=="Arm")
  Meat_coef = broom::tidy(lm(Chose_Meat ~ Arm, data)) %>% filter(term=="Arm")

  return(Chicken_coef$estimate - Meat_coef$estimate)
}

d = generate_fake_data(3332)

bs = boot(d, calculate_statistics, R=2000, parallel='multicore')

bs_ci = boot.ci(bs, type="bca")



calculate_statistics(generate_fake_data(3332), 1:3332)


# Descriptive statistics #######################################################
# Check we've created the right number of samples
assert_that(d$ID %>% unique() %>% length() == N)

d %>% group_by(Arm, Chose) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Arm) %>%
  mutate(Percentage = Count / sum(Count))



# Gravyard #
## Fit some models ##
# Multinomial logit power analysis #############################################
# Considered 4 packages implementations
# [nnet](https://www.rdocumentation.org/packages/nnet/versions/7.3-19/topics/multinom)
# [VGAM](https://www.rdocumentation.org/packages/VGAM/versions/1.1-9/topics/multinomial)
# [Mlogit](https://www.rdocumentation.org/packages/mlogit/versions/1.1-1/topics/mlogit)
# [Logitr](https://www.rdocumentation.org/packages/logitr/versions/1.1.1/topics/logitr)
# Only this last one obviously provides the covariance matrix

### Logitr ###
# this goes in the for loop of generate_fake_data
d_logitr = rbind(d_logitr,
                 expand.grid(ID=i, Arm=arm, Choices=1:5, Chose=chose))

# To accomodate the rather...unique...format required by Logitr
d_logitr$Chose[d_logitr$Choices != d_logitr$Chose] = 0
d_logitr$Chose[d_logitr$Choices == d_logitr$Chose] = 1
# Logitr gets what logitr wants
d_logitr$Choices = as.character(d_logitr$Choices)

# Check the distributions
d_logitr %>% dplyr::filter(Chose == 1) %>% group_by(Arm, Choices) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Arm) %>%
  mutate(Percentage = Count / sum(Count))


regression_logitr = logitr::logitr(data=d_logitr, outcome="Chose", obsID="ID",
                                   pars=c("Choices", "Arm"), vcov=TRUE)

regression_logitr$coefficients
head(d_logitr)
summary(regression_logitr)

# Logitr example code
# mnl_pref <- logitr(
#   data    = yogurt,
#   outcome = 'choice',
#   obsID   = 'obsID',
#   pars    = c('price', 'feat', 'brand')
# )
# summary(mnl_pref)
# coef(mnl_pref)
# head(yogurt)
#
# data <- subset(
#   yogurt, obsID %in% c(42, 13),
#   select = c('obsID', 'alt', 'price', 'feat', 'brand'))
# data
#
#
# predict(
#   mnl_pref,
#   newdata = data,
#   obsID = "obsID"
# )

### NNET ###
r_nnet = nnet::multinom(Chose ~ Arm, d)
summary(r_nnet)

r_nnet


### XXX Attempt to use DeclareDesign ###
declaration_13.2 <-
  declare_model(N = 1000,
                U = rnorm(N),
                X = U + rnorm(N, sd = 0.5),
                potential_outcomes(Y ~  0.2 * Z + U)) +
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(S = simple_rs(N, prob = 0.2),
                   filter = S == 1) +
  declare_assignment(Z = complete_ra(N)) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "ATE", label = "ZIM") +
  declare_estimator(Y ~ Z + X, inquiry = "ATE", label = "OLS")
declaration_13.2

diagnosis_13.1 <-
  diagnose_design(declaration_13.2,
                  sims = 500,
                  bootstrap_sims = 100)
reshape_diagnosis(diagnosis_13.1)
