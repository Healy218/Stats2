#This R script demonstrates the basics of logistic regression using the Default dataset
# This is customer default records for a credit card company.
library(readr)
Default <- read_csv("Default.csv")

#Orignally the default column was a Yes or No. Below I change Yes=1 and No=0
Default$default <- ifelse(Default$default == "Yes", 1, 0)

#View(Default)

#Here we plot a scatter plot of balance and default
plot(Default$balance, Default$default)

#We can fit a regression line to this.  But it doesnt really make sense
model1 <- lm(default ~ balance, data = Default)

print(summary(model1))

plot(Default$balance, Default$default,
     xlab = "Balance",
     ylab = "Probability of Default")
abline(model1, col = "red", lwd = 2)

#Instead we perform a logistic regression analysis.  This is done by using glm()
#This stands for "general linear model" notice family="binomial" because the 
#response default is a binary variable. 

model2 <- glm(default ~ balance, data = Default, family = "binomial")

print(summary(model2))

plot(Default$balance, Default$default,
     xlab = "Balance",
     ylab = "Probability of Default")

# Add logistic curve
curve(predict(model2, data.frame(balance = x), type = "response"),
      add = TRUE, col = "blue", lwd = 2)

#Print the odds ratio is: 
print(exp(coef(model2)))
# The balance part is saying for every 1$ increase in the balance, 
#the odds of default increase by 1-1.005% or by about 0.55%
#Better put: for every 100$ increase in the balance the odds go up by 73ish% (1.055)^100
#The intercept is the odds of default if the balance is 0. 

#The point logistic regression works best thinking in terms of odds. 
#If you want probability then you have to be sneaky with your algebra or do:

#This says predict the probability of default when the balance=1000
print(predict(model2, newdata = data.frame(balance = 1000), type = "response"))

#or predict the probability of default when the balance=2000
print(predict(model2, newdata = data.frame(balance = 2000), type = "response"))

#suppose you want to find the balance at which an individual customer 
#has a 25% predicted probability of default, given your model.

# Extract coefficients
beta0 <- coef(model2)[1]
beta1 <- coef(model2)[2]

# Target probability
p <- 0.25

# Solve for balance
balance_25 <- (log(p / (1 - p)) - beta0) / beta1

print(balance_25)

# You can imagine that this sort of game is played to set a credit limit on credit cards
# In that case it would be a multiple logistic regression with many variables 
#(basically every question you fill out on a credit card app becomes a variable)
