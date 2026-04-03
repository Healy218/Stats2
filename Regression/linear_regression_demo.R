# simulate example data
#x <- runif(5, 0, 100)
#y <- 5*x + rnorm(5, 0, 10)
#df1 <- data.frame(x, y)

x<-c(60.28,25.18,81.19,82.12,60.90)
y<-c(306.13,116.16,397.30,422.80,319.17)
df <- data.frame(x, y)
mean(x)
mean(y)
Sxx <- sum((x - mean(x))^2)
Sxy <- sum((x - mean(x))*(y - mean(y)))
mean(y)-Sxy/Sxx*mean(x)

model1<-lm(y~x)

x1<-c(26.46602,97.85281,58.38382,35.91708,77.78303)
y1<-c(125.1716,464.1498,282.5097,196.9285,390.4902)
mean(x1)
mean(y1)
Sxx1 <- sum((x1 - mean(x1))^2)
Sxy1 <- sum((x1 - mean(x1))*(y1 - mean(y1)))
mean(y1)-Sxy1/Sxx1*mean(x1)

model2<-lm(y1~x1)

plot(model1)