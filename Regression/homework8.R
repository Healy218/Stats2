# Matt Healy
# STAT 3612
# 04/15/2026
# Homework 8
#
#pg 436: Exercise 11.49
#pg 450: Exercise 12.1 (Use R) 
#pg 501: Exercise 12.61 (Use R again and logistic regression file on canvas) 

#pg 436: Exercise 11.49
# The Statistics Consulting Center at Virginia Tech analyzed data on normal woodchucks for the Department
# of Veterinary Medicine. The variables of interest were body weight in grams and heart weight in grams.
# It was desired to develop a linear regression equation in order to determine if there is a significant relationship between body weight and heart weight. The data for five woodchucks are given below.
# linear relationship between heart weight and total body weight. The data are as follows:
#
# a. Calculate r.
# b. Test the null hypothesis that p = 0 against the alternative hypothesis that p > 0 at the 0.01 significance level.
# c. What percentage of the variation in infant chest sizes is explained by difference in weight?

# =========== Weight and Heart Weight of Woodchucks =========== #

bw<-c(4050, 2465, 3120, 5700, 2595, 3640, 2050, 4235, 2935, 4975, 3690, 2800, 2775, 2170, 2370, 2055, 2025, 2645, 2675)
hw<-c(11.2, 12.4, 10.5, 13.2, 9.8, 11.0, 10.8, 10.4, 12.2, 11.2, 10.8, 14.2, 12.2, 10.0, 12.3, 12.5, 11.8, 16.0, 13.8)
dfw2h <- data.frame(bw, hw)
mean(bw)
mean(hw)
Sxx <- sum((bw - mean(bw))^2)
Syy <- sum((hw - mean(hw))^2)
Sxy <- sum((bw - mean(bw))*(hw - mean(hw)))
mean(hw)-Sxy/Sxx*mean(bw)
b1 <- Sxy/Sxx
b0 <- mean(hw)-b1*mean(bw)

b1 
b0 
model1<-lm(hw~bw)
summary(model1)



# a. Calculate r.
r <- Sxy/sqrt(Sxx*Syy)
r # [1] -0.07244006 which is a very weak negative correlation between body weight and heart weight. 

# b. Test the null hypothesis that p = 0 against the alternative hypothesis that p > 0 at the 0.01 significance level.
p <- r*sqrt((length(bw)-2)/(1-r^2))
p # [1] -0.2994648 which is less than the critical value of 2.552 so we fail to reject the null hypothesis that p = 0.

# c. What percentage of the variation in infant chest sizes is explained by difference in weight?
r^2 # [1] 0.005247562 meaning that only 0.52% of the variation in heart weight is explained by body weight.

# == Accidentally did 11.47 thinking it was also a part of 11.49 === #
# ========= Didn't want to delete it so here it is ================= #
# =========== Weight and Chest size of Infant Woodchucks =========== #

iw<-c(2.75, 2.15, 4.41, 5.52, 3.21, 4.32, 2.31, 4.30, 3.71)
cs<-c(29.5, 26.3, 32.2, 36.5, 27.2, 27.7, 28.3, 30.3, 28.7)
mean(iw)
mean(cs)
Sxx1 <- sum((iw - mean(iw))^2)
Sxy1 <- sum((iw - mean(iw))*(cs - mean(cs)))
mean(cs)-Sxy1/Sxx1*mean(iw)

model2<-lm(cs~iw)
summary(model2)

# pg 450: Exercise 12.01

# A set of experimental runs was made to determine a way of predicting cooking time y
# at various values of oven width x1 and and flue temperature x2. The data is as follows:

y <- c(6.40, 15.05, 18.75, 30.25, 44.85, 48.94, 51.55, 61.50, 100.44, 111.42)
x1 <- c(1.32, 2.69, 3.56, 4.41, 5.35, 6.20, 7.12, 8.87, 9.80, 10.65)
x2 <- c(1.15, 3.40, 4.10, 8.75, 14.82, 15.15, 15.32, 18.18, 35.19, 40.40)
dfcooking <- data.frame(y, x1, x2)

model3<-lm(y~x1+x2)
summary(model3)
# Estimate the multiple Linear regression equation
# y = 0.57999 + 2.71224*x1 + 2.04971*x2 from below output under coefficients section.
# Call:
# lm(formula = y ~ x1 + x2)

# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.85350 -0.35739 -0.00333  0.24429  1.15088 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.57999    0.60685   0.956    0.371    
# x1           2.71224    0.20209  13.421 2.99e-06 ***
# x2           2.04971    0.04808  42.630 1.02e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.657 on 7 degrees of freedom
# Multiple R-squared:  0.9997,	Adjusted R-squared:  0.9996 
# F-statistic: 1.269e+04 on 2 and 7 DF,  p-value: 3.482e-13

# pg 501: Exercise 12.61
# In an experiment to ascertain the effect of load, x, in lb/inches^2, on the probability
# of failure of specimens of a certain fabric type, and experiment was conducted 
# in which numbers of specimens were exposed to loads ranging from 
# 5lb/inches^2 to 90 lb/inches^2. The number of "failures" were observed. 
# The data are as follows:

x <- c(5, 35, 70, 80, 90)
n <- c(600, 500, 600, 300, 300)
y <- c(13, 95, 189, 95, 130)
dffailure <- data.frame(x, n, y)
model4<-glm(cbind(y, n-y)~x, family=binomial)
summary(model4)
# Use logistic regression to fit the model: p = 1/(1+e^-(b0+b1*x)) where p is the probability 
# of failure and x is the load. 
# Call:
# glm(formula = cbind(y, n - y) ~ x, family = binomial)
#
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -2.994903   0.145945  -20.52   <2e-16 ***
# x            0.030770   0.002094   14.69   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 319.274  on 4  degrees of freedom
# Residual deviance:  36.218  on 3  degrees of freedom
# AIC: 69.649
#
# Number of Fisher Scoring iterations: 5

b0 <- coef(model4)[1]
b1 <- coef(model4)[2]
# The logistic regression equation is: p = 1/(1+e^-(b0+b1*x)) where p is the probability of failure and x is the load. 
# The intercept b0 is -2.994903 and the slope b1 is 0.030770. 
p <- 1/(1+exp(-(-2.994903 + 0.030770*x)))
# This means that for every 1 lb/inches^2 increase in load, the odds of failure increases by 0.030770. 



# Use the odds ratio to determine the increase in odds of failure that results by 
# increasing the load from 20 lb/in^2.
# To find the increase in odds of failure that results by increasing the load from 20 lb/inches^2:
odds_ratio <- exp(b1 * (20))
odds_ratio # [1] 1.822118 which means that the odds of failure increase by a factor of 1.822118 when the load increases from 20 lb/inches^2 to 21 lb/inches^2.