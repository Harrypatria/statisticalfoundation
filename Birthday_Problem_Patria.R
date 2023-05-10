# Birthday problem ~ Harry Patria
# Part 1
n <-10
bd <- sample(1:365, n, replace = TRUE)

duplicated(c(1,2,5,7,2,3,4,8,9))

N <- 10000
same_bd <-function(n){
  bd <- sample(1:365, n, replace = TRUE)
  any(duplicated(bd))
}
results <- replicate(N, same_bd(10))
mean(results)

compute_prob <- function(n, N=10000){
  results <- replicate(N, same_bd(n))
  mean(results)
}
mean(results, digits=3)

n <- seq(1,100)
prob <- sapply(n, compute_prob)

library(tidyverse)
prob <- sapply(n, compute_prob)
qplot(n, prob, main="Probability at least 2 people having same Birthday", xlab="number of people", ylab = "probability")

est_prob <-function(n){
  prob_bd <-seq(365, 365-n+1)/365
  1-prod(prob_bd)
}
eprob <- sapply(n, est_prob)
qplot(n, prob) + geom_line(aes(n, eprob), col="blue")

# Part 2
N <- 10^seq(1, 4, len = 100)
est_prob <- function(N, n=10){
  same_day <- replicate(N, same_bd(n))
  mean(same_day)
}
prob <- sapply(N, est_prob)
qplot(log10(N), prob, geom = "line")