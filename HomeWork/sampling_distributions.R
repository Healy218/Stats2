################# SAMPLING DISTRIBUTIONS LAB #################################
## Your Name ### Your Section Time####

set.seed(123)  #set.seed() is a function for random number generation.
#When you use randomness (generating random numbers, sampling,etc), the results vary each time you run the code.
#By using set.seed(), you ensure the same sequence of random numbers is generated every time as long as the seed value remains the same.

#Problem 1) 
# a) Generate a sample of size 50 from an exponential distribution with lamba=1/5. 
lambda=1/5
sample_1 <- rexp(50,lambda) 
print(sample_1)

#b) What is the sample mean, median, variance, and standard deviation?
print(summary(sample_1))

#c) Plot a histogram of your sample.
hist(sample_1)

#Problem 2) 
# a) Generate a sample of size 50 from a normal distribution with mu=5 and sigma=3. 
mu=5
sigma=3
sample_2 <- rnorm(50,mean=mu, sd=3) 
print(sample_2)

#b) What is the sample mean, median, variance, and standard deviation?
print(summary(sample_2))

#c) Plot a histogram of your sample.
hist(sample_2)

#Problem 3) 
# a) Generate a sample of size 50 from a normal distribution with mu=5 and sigma=3 again. 
mu=5
sigma=3
sample_3 <- rnorm(50,mean=mu, sd=3) 
print(sample_3)

#b) What is the sample mean, median, variance, and standard deviation?
print(summary(sample_3))

#c) Plot a histogram of your sample.
hist(sample_3)

#Problem 4) Comment on the similarities and differences in the samples obtained from Problems 2 and 3.
# The mean if the first is a bit larger than the second.  The values are pretty similar though! Sample 3 seems a little heavy-tailed (for me).

#Problem 5) 
#a) Generate 500 samples of size 10 from an exponential distribution with lambda=1/5.
#The code below will help you replace rdist with the correct distribution and parameters 
n_samples <- 500  # Number of samples
sample_size <- 10   # Size of each sample
lambda <- 1/5
# Generate samples and compute sample means
sample_means <- replicate(n_samples, mean(rexp(sample_size,lambda)))

#b) Plot a histogram of your sample means
hist(sample_means)

#c) How does the distribution of the sample means compare/contrast with the underlying exponential distribution?
#It is a little more normally distributed.  It does not follow an exponential distribution for sure! 


#Problem 6) Repeat problem 5 with sample size 50.  What differences do you notice? 
n_samples <- 500  # Number of samples
sample_size <- 50   # Size of each sample
lambda <- 1/5
# Generate samples and compute sample means
sample_means_2 <- replicate(n_samples, mean(rexp(sample_size,lambda)))

#b) Plot a histogram of your sample means
hist(sample_means_2)

#Problem 7) 
#a) Conduct a test for normality on the samples from problem 6. Is it reasonable to assume this data is normally distributed?

#To create normal probability plots we use the following:
#qqnorm(data): Plots the quantiles of the data against the theoretical quantiles of a normal distribution.
#qqline(data): Adds a reference line to help assess normality. If the points closely follow the line, the data is approximately normal.


# Create a Q-Q plot
qqnorm(sample_means_2, main = "Q-Q Plot with Line Equation")
qqline(sample_means_2, col = "blue", lwd = 2)

# Calculate the slope and intercept of the Q-Q line
slope <- sd(sample_means_2)
intercept <- mean(sample_means_2)

# Add the equation of the line to the plot
equation <- paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x")
text(x = -2, y = max(sample_means_2), labels = equation, col = "red", pos = 4)

# It does seem reasonable to assume normality

#b) What do you think the expected value and variance of the sample means are? How about the underlying population
#From the data it suggests mu=5.01 and std dev=0.72

#The true underlying average is 5 and from the CLT we know 0.72 =std_dev(X)/sqrt(50)
# So std dev(X) is approximately 0.72sqrt(50)=5.09 