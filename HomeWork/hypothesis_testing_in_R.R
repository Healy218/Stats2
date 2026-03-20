### HYPOTHESIS TESTING IN R ###########

### The main idea is to use the built in r function t.test() ### 
## In fact, if even we have a large sample, we can use a T test statistic.
## The T test statistic may be very similar to the Z test statistic if n is large
## This is not done by hand because we do not have T-tables for any df. 

### A test of hypothesis is done as follows: 

# Import data 
data <- c(10, 12, 11, 13, 10, 14, 12, 11, 13, 10)

# Define alpha- the level of significance
alpha <- 0.05


# Perform t-test. 
#The default is H_0: mu=11 (as entered below) and H_a: mu != 11
t_test_result <- t.test(data, mu = 11)

# Print results
#The line commented out below is the standard output of the t.test.
#Below I have added a code to explicitly say whether to reject H_0 or not. 
#print(t_test_result)

# Print my conclusion:
if (t_test_result$p.value < alpha) {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should be rejected.\n")
} else {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should fail to be rejected.\n")
}
#############################################################


## This example outlines how to do one-sided t-test
# Sample data
data_sample <- c(10.2, 10.5, 9.8, 10.1, 10.4, 9.9, 10.3, 10.6)

# Define alpha
alpha <- 0.05

# Perform one-sided t-test
#In this t test we are checking H_0: mu=10 H_a: mu>10
results_greater <- t.test(x = data_sample, mu = 10, alternative = "greater")

# Print results
print(results_greater)

# Print custom conclusion
if (results_greater$p.value < alpha) {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should be rejected.\n")
} else {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should fail to be rejected.\n")
}
#########################################################
## Test for population proportion

x <- 280 # Number of successes
n <- 500 # Sample size

p_hypothesized <- 0.60

# Define alpha
alpha <- 0.05
# Perform proportion test
results_proportion_one_sample <- prop.test(x = x, n = n, p = p_hypothesized)

# Print results
print(results_proportion_one_sample)

# Print custom conclusion
if (results_proportion_one_sample$p.value < alpha) {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should be rejected.\n")
} else {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should fail to be rejected.\n")
}

#############################################################################
# This test is for determining whether two samples from different normal populations
# support the H_0: mu_1 = mu_2 and H_a: mu_1 != mu_2

#See Section 9.2 
group1 <- c(10, 12, 11, 13, 10, 14, 12, 11, 13, 10)
group2 <- c(10.2, 10.5, 9.8, 10.1, 10.4, 9.9, 10.3, 10.6)
# Welch's two-sample t-test (default) 
t_test_result <- t.test(group1, group2, var.equal = FALSE)

# Print results
print(t_test_result)

# Print formatted conclusion
if (t_test_result$p.value < alpha) {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should be rejected.\n")
} else {
  cat("The data suggests that at an alpha value =", alpha,
      "the null hypothesis should fail to be rejected.\n")
}