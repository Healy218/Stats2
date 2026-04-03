Auto <- read.csv("Regression/Auto.csv", header=T, na.strings="?")
Auto <- na.omit(Auto)

dim(Auto)
names(Auto)
summary(Auto)

###===== Questions =====###
# 1. How many observations are in the dataset?
# 2. What is the response variable? 
#    Mpg
# 3. Which variables are quantitative? Which are categorical?

model1 <- lm(mpg ~ horsepower, data=Auto)
summary(model1)

####===== Questions =====###
# 1. What is the equation of the fitted line?
# 2. Is there a realationship between horsepower and miles per gallon?
# how do you know?
# 3. Interpret the coefficient for horsepower in this context.
# 4. What does the R-squared value tell you?

plot(Auto$horsepower, Auto$mpg)
abline(model1, col="red", lwd=2)

###===== Questions =====###
# 1. Does the relationship appear linear?
# 2. Are there obvious outliers?

par(mfrow=c(2,2))
plot(model1)

###===== Questions =====###
# 1. Do the models appear to be randomly scattered?
# 2. Is there evidence of non-linearity?

model2 <- lm(mpg - cylinders + displacement + horsepower + weight + acceleration + year + origin, data=Auto)
summary(model2)

#####===== Questions =====###
# 1. Which variables appear statistically significant?
# 2. Interpret the coefficient of weight.
# 3. What happens to the effect of horsepower compared to model1?
# 4. What is the new R-squared? Is it better?

anova(model1, model2)

#####===== Questions =====###
# 1. Does model2 significantly improve the fit compared to model1?
# 2. Is there a downside of adding too many variables?

model3 <- lm(mpg - horsepower + I(horsepower^2), data=Auto)
summary(model3)

#####===== Questions =====###
# 1. Is the quadratic term significant?
# 2. Does this model fit better than model1?
# 3. What shape does this suggest for the relationship?

predict(model1, data.frame(horsepower=c(98)), interval="confidence"

#####===== Questions =====###
)
