#install.packages("tolerance")   # run once
library(tolerance)

# Interval Estimates Template

############################################################
# Confidence Intervals
############################################################
# Given values
mean_val <- 23500  # sample mean
sample_sd <- 3900 # sample standard deviation
n <- 100        # sample size
conf_level <- 0.95
degrees_of_freedom <- n - 1

# Calculate critical t-value
# The qt function is used to find the quantile of the t-distribution
t_critical <- qt(1 - (1 - conf_level) / 2, df = degrees_of_freedom)

# Calculate the margin of error
margin_of_error <- t_critical * (sample_sd / sqrt(n))

# Calculate the confidence interval
lower_bound <- mean_val - margin_of_error
upper_bound <- mean_val + margin_of_error

# Print the result
cat(sprintf(
  "95%% confidence interval for the population mean is: [%.2f, %.2f]\n",
  lower_bound,
  upper_bound
))



##############################################################
#Prediction Intervals
#############################################################
# Define your parameters
sample_mean <- 23500
sample_sd <- 3900
sample_size <- 100
confidence_level <- 0.95

# Calculate the t-multiplier (for 95% interval, alpha = 0.05, alpha/2 = 0.025)
alpha <- 1 - confidence_level
t_multiplier <- qt(1 - alpha / 2, df = sample_size - 1)

# Calculate the margin of error component K * s
# Note the formula for the standard error of prediction is slightly different
# than the standard error of the mean (it includes the '1 +' term)
margin_of_error <- t_multiplier * sample_sd * sqrt(1 + 1/sample_size)

# Calculate the prediction interval bounds
lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

# Print the results

cat(sprintf(
  "The %g%% prediction interval is [%.2f, %.2f]\n",
  confidence_level * 100,
  lower_bound,
  upper_bound
))


###################################################################
#TOLERANCE LIMITS
###################################################################

# Define your parameters
sample_mean <- 23500       # The sample mean (x.bar)
sample_sd <- 3900         # The sample standard deviation (s)
sample_size <- 100       # The sample size (n)
coverage_proportion <- 0.95  # The proportion of the population you want to cover (e.g., 90%)
confidence_level <- 0.99     # The confidence level for the interval (e.g., 95%)



K <- K.factor(
  n = sample_size,
  alpha = 1 - confidence_level,
  P = coverage_proportion,
  side = 2
)
lower <- sample_mean - K * sample_sd
upper <- sample_mean + K * sample_sd
cat(sprintf(
  "A %.0f%% two-sided tolerance interval covering %.0f%% of the population is [%.2f, %.2f].\n",
  (confidence_level) * 100,
  coverage_proportion * 100,
  lower,
  upper
))
