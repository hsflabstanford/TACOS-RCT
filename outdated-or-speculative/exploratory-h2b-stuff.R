# speculative h2b stuff

# H2b is a bit more complicated
# Baseline scenario (Arm 2)
p_chicken_arm2 <- 0.40
p_meat_arm2 <- 0.48

# Hypothesized scenario with chick'nitas (Arm 3)
# Suppose chick'nitas reduces chicken more significantly:
p_chicken_arm3 <- 0.30
p_meat_arm3 <- 0.46

# Calculate differences
delta_chicken <- p_chicken_arm2 - p_chicken_arm3
delta_meat <- p_meat_arm2 - p_meat_arm3

# Check H2b
diff_in_diff <- delta_chicken - delta_meat
cat("Difference-in-differences:", diff_in_diff, "\n")

if (diff_in_diff > 0) {
  cat("H2b is supported: The reduction in chicken is greater than the reduction in other meat.\n")
} else {
  cat("H2b is not supported.\n")
}

# Desired parameters
p_C2 <- 0.40
p_C3 <- 0.35
p_M2 <- 0.48
p_M3 <- 0.46

did <- (p_C2 - p_C3) - (p_M2 - p_M3) # This should be 0.03
did

# Significance and power
alpha <- 0.05
power <- 0.8

# Z-scores
z_alpha <- qnorm(1 - alpha/2)       # ~1.96 for 95% CI
z_power <- qnorm(power)             # ~0.84 for 80% power

# For a two-sided test with equal sample sizes:
# We want: DID = (z_alpha + z_power)*SE(DID)
# SE(DID) = sqrt(Var(DID))
#
# Var(DID) = Var(Delta_chicken) + Var(Delta_meat)
# Var(Delta_chicken) = p_C2*(1-p_C2)/n + p_C3*(1-p_C3)/n
# Var(Delta_meat) = p_M2*(1-p_M2)/n + p_M3*(1-p_M3)/n
#
# Thus:
# did = (z_alpha + z_power) * sqrt((p_C2*(1-p_C2) + p_C3*(1-p_C3) + p_M2*(1-p_M2) + p_M3*(1-p_M3))/n)

var_sum <- (p_C2*(1-p_C2) + p_C3*(1-p_C3) + p_M2*(1-p_M2) + p_M3*(1-p_M3))

# Solve for n:
# did = (z_alpha + z_power)*sqrt(var_sum/n)
# did^2 = (z_alpha + z_power)^2 * var_sum / n
# n = (z_alpha + z_power)^2 * var_sum / did^2

n <- ((z_alpha + z_power)^2 * var_sum) / (did^2)
n <- ceiling(n)
n

cat("Required sample size per arm:", n, "\n")
cat("Total sample size (2 arms):", 2*n, "\n")

# Check the resulting standard error and implied detectability:
SE_DID <- sqrt(var_sum/n)
cat("Resulting standard error of DID:", SE_DID, "\n")

# Expected z-value at this sample size:
observed_z <- did / SE_DID
cat("Expected Z-score:", observed_z, "\n")
