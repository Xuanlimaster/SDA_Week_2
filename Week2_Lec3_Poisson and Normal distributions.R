##The Poisson distribution

#Example
#During a field trip, we expect to find on average one salamander for every 
#10m^2 we explore. What is the probability of finding three salamanders in 10m^2
dpois(3, 1)

#Compare the Poisson and Binomial distribution
par(mfrow = c(1,3))
theorypois <- dpois(0:10, 10*0.1)
theorybinom <- dbinom(0:10, size = 10, prob = 0.1)
barplot(theorybinom, names.arg = 0:10, space = 0, main = "Histogram of 10", 
        xlab = "Sucesses", ylab = "Probability")
lines(0:10, theorypois, type = "s", lwd = 2, lty = 2, col = "darkred")
lines(0:10, theorybinom, type = "s", lwd = 2, lty = 1, col = "darkblue")
legend("topright", legend = c("binom","pois"), bty = "n", lty = c(1,2), 
       col = c("darkblue","darkred"))

theorypois <- dpois(0:10, 100*0.01)
theorybinom <- dbinom(0:10, size = 100, prob = 0.01)
barplot(theorybinom, names.arg = 0:10, space = 0, main = "Histogram of 100", 
        xlab = "Sucesses", ylab = "Probability")
lines(0:10, theorypois, type = "s", lwd = 2, lty = 2, col = "darkred")
lines(0:10, theorybinom, type = "s", lwd = 2, lty = 1, col = "darkblue")
legend("topright", legend = c("binom","pois"), bty = "n", lty = c(1,2), 
       col = c("darkblue","darkred"))

theorypois <- dpois(0:10, 1000*0.001)
theorybinom <- dbinom(0:10, size = 1000, prob = 0.001)
barplot(theorybinom, names.arg = 0:10, space = 0, main = "Histogram of 1000", 
        xlab = "Sucesses", ylab = "Probability")
lines(0:10, theorypois, type = "s", lwd = 2, lty = 2, col = "darkred")
lines(0:10, theorybinom, type = "s", lwd = 2, lty = 1, col = "darkblue")
legend("topright", legend = c("binom","pois"), bty = "n", lty = c(1,2), 
       col = c("darkblue","darkred"))

#Increase p
theorypois <- dpois(0:10, 10*0.5)
theorybinom <- dbinom(0:10, size = 10, prob = 0.5)
barplot(theorybinom, names.arg = 0:10, space = 0, main = "Histogram of 10", 
        xlab = "Sucesses", ylab = "Probability")
lines(0:10, theorypois, type = "s", lwd = 2, lty = 2, col = "darkred")
lines(0:10, theorybinom, type = "s", lwd = 2, lty = 1, col = "darkblue")
lines(x = seq(0, 10, 0.1), 
      y = dnorm(seq(0, 10, 0.1), mean = 10*0.5, sd = sqrt(10*0.5*0.5)), 
      type = "l", lty = 2, lwd = 2, col = "green")
legend("topright", legend = c("binom","pois","norm"), bty = "n", lty = c(1,2,2), 
       col = c("darkblue","darkred","green"))

theorypois <- dpois(0:100, 100*0.5)
theorybinom <- dbinom(0:100, size = 100, prob = 0.5)
barplot(theorybinom, names.arg = 0:100, space = 0, main = "Histogram of 100", 
        xlab = "Sucesses", ylab = "Probability")
lines(0:100, theorypois, type = "s", lwd = 2, lty = 2, col = "darkred")
lines(0:100, theorybinom, type = "s", lwd = 2, lty = 1, col = "darkblue")
lines(x = seq(0, 100, 0.1), 
      y = dnorm(seq(0, 100, 0.1), mean = 100*0.5, sd = sqrt(100*0.5*0.5)), 
      type = "l", lty = 2, lwd = 2, col = "green")
legend("topright", legend = c("binom","pois"), bty = "n", lty = c(1,2,2), 
       col = c("darkblue","darkred","green"))

theorypois <- dpois(0:1000, 1000*0.5)
theorybinom <- dbinom(0:1000, size = 1000, prob = 0.5)
barplot(theorybinom, names.arg = 0:1000, space = 0, main = "Histogram of 1000", 
        xlab = "Sucesses", ylab = "Probability")
lines(0:1000, theorypois, type = "s", lwd = 2, lty = 2, col = "darkred")
lines(0:1000, theorybinom, type = "s", lwd = 2, lty = 1, col = "darkblue")
lines(x = seq(0, 1000, 0.1), 
      y = dnorm(seq(0, 1000, 0.1), mean = 1000*0.5, sd = sqrt(1000*0.5*0.5)), 
      type = "l", lty = 2, lwd = 2, col = "green")

legend("topright", legend = c("binom","pois"), bty = "n", lty = c(1,2,2), 
       col = c("darkblue","darkred","green"))



##Normal distribution
#Standard normal PDF
curve(dnorm, from = -3.5, to = 3.5, xlab = "Standard Deviations", 
      ylab = "dnorm(x)", main = "PDF of the Standard Normal")
abline(h = 0, v=c(seq(-3,3,1)), lty =2)

#Standard normal CDF
curve(pnorm, from = -3.5, to = 3.5, xlab = "Standard Deviations", 
      ylab = "dnorm(x)", main = "CDF of the Standard Normal")
abline(h = c(0,0.5,1), v=c(seq(-3,3,1)), lty =2)



##Exponential distribution
#Exp PDF
curve(dexp(x, 1), from = 0, to = 5, col = "darkred", lwd = 2, ylim = c(0,2), 
      main = "Exponential PDF", xlab = "Time", ylab = "Density")
curve(dexp(x, 2), from = 0, to = 5, col = "orange", lwd = 2, ylim = c(0,2), 
      add = TRUE)
abline(v = 0, lty = 2)
legend("topright", legend = c("lamda = 1", "lamda = 2"), lty = c(1,1),
       col = c("darkred","orange"), bty = "n")

#Exp CDF
curve(pexp(x, 1), from = 0, to = 5, col = "darkred", lwd = 2, ylim = c(0,1), 
      main = "Exponential CDF", xlab = "Time", ylab = "Density")
curve(pexp(x, 2), from = 0, to = 5, col = "orange", lwd = 2, ylim = c(0,1), 
      add = TRUE)
abline(h = c(0,1), v = 0, lty = 2)
legend("right", legend = c("lamda = 1", "lamda = 2"), lty = c(1,1),
       col = c("darkred","orange"), bty = "n")











