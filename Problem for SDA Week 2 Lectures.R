library(ggplot2)
library(tidyverse)

#1)Download the dataset "iris".

#a)Produce a figure or figures in R to explore the distributions of petal 
#  lengths in the three species of Iris. Which species has the longest petals, 
#  and which has the shortest?
petals <- iris |> select(Petal.Length, Species) |> group_by(Species)
petals |> 
  ggplot(aes(x = Petal.Length))+
  geom_bar()+
  geom_density(aes(fill = Species, colour = Species), linewidth = 1, 
               alpha = 0.5)+
  theme_classic()+
  labs(title = "Petal length in each species", x = "Petal Length", 
       y = "Count")

#b)Calculate the mean and standard deviation of the petal lengths for each 
#  species.
petals |> group_by(Species) |> summarise(mean(Petal.Length), sd(Petal.Length))



#2)Assume that the probability of a male birth in the UK is 0.5. Use the 
#  Binomial distribution functions in R to calculate the probability that in 
#  10 births

#a)There are the same number of boys as girls.
dbinom(5, 10, 0.5)

#b)There are more boys than girls.
pbinom(5, 10, 0.5, lower.tail = FALSE)

#c)Think first, then calculate: if there are 100 births, and you ask the 
#  previous two questions again, will the probabilities increase, decrease or
#  stay the same?
dbinom(50, 100, 0.5)
pbinom(50, 100, 0.5, lower.tail = FALSE)



#3)A pharmaceutical company has developed a new drug that is supposed to 
#  increase the activity of immune response genes. You decide to test if it 
#  actually works with a simple experiment. You take a well studied human cell
#  line, in which 11,000 genes are known to be active (of which 620 are immune
#  response genes), and treat these cells with the new drug. Your result 
#  consists of a list of 1,320 genes with increased activity, of which only 98
#  are immune reponse genes. Does the drug actually work?

#a)What would be the expected number of immune response genes in a random sample
#  of size 1,320?
p <- 620/11000
mean <- 1320*p

#b)Use the hypergeometric distribution to calculate the probability of observing
#  exactly this result by chance (98 immune response genes in a random sample of
#  1,320).
p1 <- dhyper(98, 620, 11000-620, 1320)

#c)As a negative control, you decide to check an unrelated category of genes. 
#  The cell line expresses 576 abiotic stress genes of which 62 had increased 
#  activity in your experiment. Use the hypergeometric distribution to calculate
#  the probability of getting this exact result, just by chance.
p2 <- dhyper(62, 576, 11000-576, 1320)

#d)How do you reconcile your answers to the previous two questions? What further
#  calculation should you make?
plargerthan98 <- phyper(98, 620, 11000-620, 1320, lower.tail = FALSE)
plargerthan62 <- phyper(62, 576, 11000-576, 1320, lower.tail = FALSE)

Reason <- "The probability of getting, by chance, at least 98 immune response 
           genes is only 0.0022321. This is highly unlikely to occur just by
           chance. On the other hand, the probability of getting at least 62
           abiotic stress genes is 0.8423385, which is much more compatible with
           a chance result."

#4)The frequency of twins in European populations is 12 in every 1000 births.

#a)Using the binomial distribution calculate the probability there will be no 
#  twins in 200 births
ptwins <- 12/1000
dbinom(0, 200, ptwins)

#b)Calculate the same probability using the Poisson approximation to the 
#  binomial distribution
dpois(0, 200*ptwins)

#c)Do you think these answers will get closer if the frequency of twins was 1 in
#  every 1000 births? Why?
Answer <- "Yes! The Poisson distribution arises as the limit of the Binomial, 
           when the sample is very large, and the probability of success is very
           small."

#5)Assume that the number of buses arriving at your bus stop follows a Poisson 
#  distribution. If, on average, 1 bus arrives every 90 seconds

#a)What is the probability that at least two buses will arrive in the next 
#  minute?
lamdabus <- 1/(90/60)
ppois(1, lamdabus, lower.tail = FALSE)

#b)How about the probability that two or more will arrive in the next three
#  minutes?
ppois(1, lamdabus*3, lower.tail = FALSE)

#c)A bus has just arrived, what’s the probability you would need to wait at
#  least three minutes for the next one?
pexp(3, lamdabus, lower.tail = FALSE) #==dpois(0, lambdabus*3)
pexp(1, lamdabus*3, lower.tail = FALSE)



#6)Daily growth rates in a population of cattle have a mean of 750g and a 
#  standard deviation of 100g. Assuming that growth rate is normally 
#  distributed, find the proportion of cattle with the following growth rates

#a)Between 650 and 850g
pnorm(850, mean = 750, sd = 100) - pnorm(650, 750, 100)

#b)Less than 600g
pnorm(600, 750, 100)

#c)Above what growth rate do you expect to find the 10% fastest growing cattle?
qnorm(0.1, 750, 100, lower.tail = FALSE)
