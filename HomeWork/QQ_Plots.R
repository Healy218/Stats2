
# I am going to generate a random sample from a normal distribution and then look at the QQ plot

sample1=rnorm(30,mean=5,sd=3)

#To create normal QQ plots we use the following:

#qqnorm(data): Plots the quantiles of the data against the theoretical quantiles of a normal distribution.
#qqline(data): Adds a reference line to help assess normality. If the points closely follow the line, the data is approximately normal.

# Create a normal probability plot
qqnorm(sample1, main = "Normal Q-Q Plot")
qqline(sample1, col = "red", lwd = 2)  # Add a reference line

# Now I will sample from a non-normal distribution and look at the QQ-plots
sample2=rexp(30,rate=1/2)

# Create a normal probability plot
qqnorm(sample2, main = "Normal Q-Q Plot")
qqline(sample2, col = "red", lwd = 2)  # Add a reference line

#The following data is precipitation values during March over a 30-year period in Minneapolis-St. Paul:
#a)
precip_values=c(.77,1.20,3.00,1.62,2.81,2.48,1.74,.47,3.09,1.31,1.87,.96,.81,1.43,1.51,.32,1.18,1.89,1.20,3.37,2.10,.59,1.35,.90,1.95,2.20,.52,.81,4.75,2.05)


# Create a normal probability plot
qqnorm(precip_values, main = "Normal Q-Q Plot")
qqline(precip_values, col = "red", lwd = 2)  # Add a reference line

#This does not seem to follow a normal distribution because the values deviate from the line on either end
#This suggest a heavy tailed distribution on either end.. The graph does not decrease to 0 rapidly

#b)
#Take the sqrt of all the precip values
sqrt_precip_values=sqrt(precip_values)
#Try again
# Create a normal probability plot
qqnorm(sqrt_precip_values, main = "Normal Q-Q Plot")
qqline(sqrt_precip_values, col = "red", lwd = 2)  # Add a reference line

#This is better! There is still an indication of a heavy tail but not as dramatic as before.

#c)
#Take the sqrt of all the precip values
cubert_precip_values=precip_values^(1/3)
#Try again
# Create a normal probability plot
qqnorm(cubert_precip_values, main = "Normal Q-Q Plot")
qqline(cubert_precip_values, col = "red", lwd = 2)  # Add a reference line

#This is maybe a little better even! 

#d)# I am just trying something! Have a look at what the log of all the precip values does
log_precip_values=log(precip_values)
#Try again
# Create a normal probability plot
qqnorm(log_precip_values, main = "Normal Q-Q Plot")
qqline(log_precip_values, col = "red", lwd = 2)  # Add a reference line