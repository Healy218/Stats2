Auto <- read.csv("Regression/Auto.csv", header=T, na.strings="?")
Auto <- na.omit(Auto)

dim(Auto)
names(Auto)
summary(Auto)

###===== Questions =====###
# 1. How many observations are in the dataset?
# 392
# 2. What is the response variable? 
#    Mpg
# 3. Which variables are quantitative? Which are categorical?
# Quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year
# Categorical: origin, name

model1 <- lm(mpg ~ horsepower, data=Auto)
summary(model1)

####===== Questions =====###
# 1. What is the equation of the fitted line?
# the fitted line is: mpg = 40.09 - 0.157*horsepower
# 2. Is there a realationship between horsepower and miles per gallon?
# how do you know?
# Yes, there is a relationship. The p-value for the horsepower coefficient is very small (2.69e-09), 
# which indicates that the relationship is statistically significant.
# 3. Interpret the coefficient for horsepower in this context.
# The coefficient for horsepower is -0.157, which means that for each additional unit of horsepower,
# 4. What does the R-squared value tell you?
# The R-squared value of 0.6059 indicates that approximately 60.59% of the variability in miles per gallon (mpg)
# can be explained by the linear relationship with horsepower. This suggests that 
# horsepower is a strong predictor of mpg, but there are still other factors that contribute to 
# the variability in mpg that are not captured by this model.

plot(Auto$horsepower, Auto$mpg)
abline(model1, col="red", lwd=2)

###===== Questions =====###
# 1. Does the relationship appear linear?
# Yes, the relationship appears to be linear as the points roughly follow a straight line pattern.
# 2. Are there obvious outliers?
# There are a few points that deviate from the general trend, which could be considered outliers.

par(mfrow=c(2,2))
plot(model1)

###===== Questions =====###
# 1. Do the models appear to be randomly scattered?
# Yes, the residuals appear to be randomly scattered around the horizontal line at zero, 
# which suggests that the model is appropriate for the data.
# 2. Is there evidence of non-linearity?
# There is no clear evidence of non-linearity in the residuals plot, 
# as the points do not show a systematic pattern.

model2 <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data=Auto)
summary(model2)

#####===== Questions =====###
# 1. Which variables appear statistically significant?
# The variables that appear statistically significant (p-value < 0.05) are:
# 2. Interpret the coefficient of weight.
# The coefficient of weight is -0.005, which means that for each additional unit of weight,
# 3. What happens to the effect of horsepower compared to model1?
# In model1, the coefficient for horsepower was -0.157, indicating a strong negative relationship with mpg.
# 4. What is the new R-squared? Is it better?
# The new R-squared value is 0.821, which is an improvement from the previous model's R-squared of 0.6059.

anova(model1, model2)

#####===== Questions =====###
# 1. Does model2 significantly improve the fit compared to model1?
# Yes, model2 significantly improves the fit compared to model1. The p-value for the ANOVA test is very small (2.2e-16),
# 2. Is there a downside of adding too many variables?
# Adding too many variables can lead to overfitting, where the model captures noise in the data rather than the underlying relationship.

model3 <- lm(mpg ~ horsepower + I(horsepower^2), data=Auto)
summary(model3)

#####===== Questions =====###
# 1. Is the quadratic term significant?
# Yes, the quadratic term (I(horsepower^2)) is significant with a p-value of 0.0001.
# 2. Does this model fit better than model1?
# Yes, model3 fits better than model1. The R-squared value for model3 is 0.657, which is higher than the R-squared value of 0.6059 for model1, indicating that the quadratic model explains more of the variability in mpg than the linear model.
# 3. What shape does this suggest for the relationship?
# The significant quadratic term suggests that the relationship between horsepower and mpg is not strictly linear, but rather has a curved shape. Specifically, it indicates that as horsepower increases, the rate of decrease in mpg may change, potentially leveling off or even increasing at higher horsepower values.

predict(model1, data.frame(horsepower=c(98)), interval="confidence")

#####===== Questions =====#####
# 1. What is the predicted mpg for a car with 98 horsepower?
# The predicted mpg for a car with 98 horsepower is approximately 24.47.
# 2. What does the confidence interval represent?
# The confidence interval represents the range of values within which we can be 
# 95% confident that the true mean mpg for cars with 98 horsepower lies. 
# In this case, the confidence interval is approximately (22.42, 26.52), 
# which means we are 95% confident that the average mpg for cars with 98 horsepower is between 22.42 and 26.52.

