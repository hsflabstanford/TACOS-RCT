library(pwr)

# Under the null hypothesis, assume a uniform distribution across all 15 cells 
# in a 5 (categories) x 3 (arms) table:
p0 <- rep(1/15, 15)  # Each cell = 1/15 ≈ 0.06667

# Scenario: A 5 percentage-point (pp) decline in one meat category within one arm, 
# with a compensating increase in another category (for simplicity).
#
# Let's say under the alternative:
# - One "meat" category in Arm 3 decreases from 0.0667 to 0.0167 (a 5pp decrease, since 0.0667 - 0.0167 ≈ 0.05)
# - Another category (e.g., the plant-based option) in Arm 3 increases by 5pp from 0.0667 to 0.1167
# The rest remain at 0.0667.

alt <- p0
# Let's pick cell #15 (last cell, representing, say, plant-based in Arm 3) to increase by 0.05
# and cell #14 (a meat category in Arm 3) to decrease by 0.05.
alt[14] <- p0[14] - 0.05  # from ~0.0667 down to ~0.0167
alt[15] <- p0[15] + 0.05  # from ~0.0667 up to ~0.1167

# Check that alt sums to 1 (it should)
if(abs(sum(alt) - 1) > 1e-10) {
  stop("Alternative distribution does not sum to 1")
}

# Compute Cohen's w for the chi-square test:
# w = sqrt(Σ((p_alt - p0)^2 / p0))
w <- sqrt(sum((alt - p0)^2 / p0))

w

# Suppose we want 90% power and an alpha of 0.05 (not Bonferroni corrected here,
# but you can adjust alpha accordingly).
# The degrees of freedom for a 5x3 table = (5-1)*(3-1) = 4*2 = 8.

df <- (5-1)*(3-1)
alpha <- 0.05
power_target <- 0.90

# Use pwr.chisq.test to find required N
res <- pwr.chisq.test(w = w, df = df, sig.level = alpha, power = power_target)
res

# This result gives you the total sample size needed to achieve 90% power 
# under this scenario with a given effect size (w).
#
# If the result doesn't closely match the exact numbers cited in the pre-registration,
# adjust the distributions (how you allocated the percentage-point changes across cells)
# to replicate the exact scenario they described. 
#
# For example, if the pre-registration scenario had a different baseline, 
# or spread the effect across multiple categories or arms differently, 
# you would modify `alt` accordingly and re-run this calculation.
