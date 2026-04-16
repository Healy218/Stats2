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

model1<-lm(hw~bw)

# =========== Weight and Chest size of Infant Woodchucks =========== #
iw<-c(2.75, 2.15, 4.41, 5.52, 3.21, 4.32, 2.31, 4.30, 3.71)
cs<-c(29.5, 26.3, 32.2, 36.5, 27.2, 27.7, 28.3, 30.3, 28.7)
mean(iw)
mean(cs)
Sxx1 <- sum((iw - mean(iw))^2)
Sxy1 <- sum((iw - mean(iw))*(cs - mean(cs)))
mean(cs)-Sxy1/Sxx1*mean(iw)

model2<-lm(cs~iw)