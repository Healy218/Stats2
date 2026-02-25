N <- 300
a <- 40
b <- 2
R <- 5050
X <- rgamma(N, shape = a, rate = b)
mean(X)
Y <- replicate(R, mean(rgamma(N, shape = a, rate = b)))
hist(Y, breaks = "fd")