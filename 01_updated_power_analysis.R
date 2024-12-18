# Desired parameters
p_control <- 0.98   # Baseline proportion
diff <- -0.03         # Desired detectable difference
p_treatment <- p_control + diff
desired_width <- 0.03 # total width of CI for the difference
z <- 1.96             # z-score for 95% CI

# The margin of error (half-width of the CI): half-width = desired_width / 2
half_width <- desired_width / 2

# We need to solve for n in:
# half_width = z * sqrt( (p_control*(1-p_control))/n + (p_treatment*(1-p_treatment))/n )
# half_width = z * sqrt( (p_control*(1-p_control) + p_treatment*(1-p_treatment)) / n )
#
# Rearrange to solve for n:
# n = (z^2 * (p_control*(1-p_control) + p_treatment*(1-p_treatment))) / (half_width^2)

numerator <- z^2 * (p_control*(1-p_control) + p_treatment*(1-p_treatment))
denominator <- half_width^2
n_per_group <- ceiling(numerator / denominator)

# H2 is the lower powered test
cat("Required total sample size for H2:", n_per_group * 2, "\n") 

# for H1
H1_total_sample = ceiling(n_per_group * 2 * 1.33)