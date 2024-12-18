# XXX This is just a dumping ground. It's likely this script will not run and it
# does not need to be.

library(assertthat)
library(boot)
library(DeclareDesign)
library(fabricatr)
library(logitr)
library(nnet)
library(pwr)
library(tidyverse)
library(lmtest)
library(sandwich)

###################################
# Chi-squared test power analysis #
###################################
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


######################
# Multinomial models #
######################
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


################################
# Attempt to use DeclareDesign #
################################
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
