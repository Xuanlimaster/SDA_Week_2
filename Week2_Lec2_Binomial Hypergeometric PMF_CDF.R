library(ggplot2)
library(patchwork)

##Probability Mass Function (PMF)
#Toss coin 2 times
d2 <- dbinom(0:2, size = 2, prob = 0.5)
barplot(d2, names.arg = 0:2, main = "Toss coin 2 times", xlab = "Heads", 
        ylab = "P(Heads)", lwd = 2)

#Toss coin 10 times
d10 <- dbinom(0:10, size = 10, prob = 0.5)
barplot(d10, names.arg = 0:10, main = "Toss coin 10 times", xlab = "Heads", 
        ylab = "P(Heads)", lwd = 2)



##Cumulative Distribution Function
#Toss coin 2 times CDF
p2 <- pbinom(0:2, size = 2, prob = 0.5)
barplot(p2, names.arg = 0:2, space = 0, main = "CDF", xlab = "Heads", 
        ylab = "P(Heads)")
lines(x = 0:2, y = p2, type = "s", col = "darkred", lwd = 2)

plot(x = 0:2, y = p2, type = "s", col = "darkred", main = "CDF", xlab = "Heads", 
     ylab = "P(Heads)", lwd = 2, asp = 1)

#Toss coin 10 times CDF
p10 <- pbinom(0:10, size = 10, prob = 0.5)
barplot(p10, names.arg = 0:10, space = 0, main = "CDF", xlab = "Heads", 
        ylab = "P(Heads)")
lines(x = 0:10, y = p10, type = "s", col = "darkred", lwd = 2)

CDFplot <- plot(x = 0:10, y = p10, type = "s", col = "darkred", main = "CDF", 
                xlab = "Heads", ylab = "P(Heads)", lwd = 2, asp = NA)

#CDF and quantiles
p <- seq(0, 1, 0.1)
q10 <- qbinom(p, size = 10, prob = 0.5)
barplot(q10, names.arg = p, space = 0, main = "Quantiles",
        xlab = "Cumulative probability", ylab = "Heads")
lines(x = 0:10, y = q10, type = "s", col = "darkred", lwd = 2)

qplot <- plot(x = p, y = q10, type = "s", col = "darkred", main = "Quantiles", 
              xlab = "Heads", ylab = "P(Heads)", lwd = 2)

##Randombiom
#n = 20
random <- rbinom(20, size = 10, prob = 0.25)
theory <- dbinom(0:10, size = 10, prob = 0.25)
hist(random, breaks = 0:10, main = "Histogram of rbinom(20,10,0.25)", 
     probability = TRUE, right = FALSE)
lines(0:10, theory, type = "s", lwd = 2, lty = 2, col = "darkred")
legend("top", legend = "dbinom", bty = "n", lty = 2, col = "darkred")

#n = 100
random <- rbinom(100, size = 10, prob = 0.25)
theory <- dbinom(0:10, size = 10, prob = 0.25)
hist(random, breaks = 0:10, main = "Histogram of rbinom(100,10,0.25)", 
     probability = TRUE, right = FALSE)
lines(0:10, theory, type = "s", lwd = 2, lty = 2, col = "darkred")
legend("top", legend = "dbinom", bty = "n", lty = 2, col = "darkred")

#n = 10000
random <- rbinom(10000, size = 10, prob = 0.25)
theory <- dbinom(0:10, size = 10, prob = 0.25)
hist(random, breaks = 0:10, main = "Histogram of rbinom(10000,10,0.25)", 
     probability = TRUE, right = FALSE)
lines(0:10, theory, type = "s", lwd = 2, lty = 2, col = "darkred")
legend("top", legend = "dbinom", bty = "n", lty = 2, col = "darkred")



##Binomial vs Hypergeometric

#Example
#7000 proteins tested, 1000 with interaction affinity, 
#453 of these with DUF3808(success), 3000 proteins tested have DUF3808

#Binomial
p = 3000/7000
dbinom(453, 1000, p)
pbinom(453-1, 1000, p, lower.tail = FALSE)

#Hypergeometric
dhyper(453, 3000, 4000, 1000)
phyper(453-1, 3000, 4000, 1000, lower.tail = FALSE)

#P(360<=x<=500)
x <- 360:500
dhAll <- dhyper(x, 3000, 4000, 1000)
barplot(dhAll, names.arg = x, xlab = "Number of successes", ylab = "P(X=k)")
sum(dhAll)











