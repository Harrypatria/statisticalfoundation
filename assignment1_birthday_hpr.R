n <- 10
bdays <- sample(1:365, n, replace = TRUE)

duplicated(c(1,2,3,1,4,3,5))

any(duplicated(bdays))

B <- 10000
same_birthday <- function(n){
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
}
results <- replicate(B, same_birthday(50))
mean(results)

compute_prob <- function(n, B=10000){
  results <- replicate(B, same_birthday(n))
  mean(results)
}

n <- seq(1,60)
prob <- sapply(n, compute_prob)

library(tidyverse)
prob <- sapply(n, compute_prob)
qplot(n, prob)

install.packages("tidyverse")
