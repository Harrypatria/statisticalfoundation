# Poission assignment by Harry Patria 
# read data from url
url = "http://www.mas.ncl.ac.uk/~nseg4/teaching/MAS8380/practical2.dat"
arr.times = scan(url)

# Problem 1
# Part a 
length(arr.times) # count length of arr.times which gives 189 times
pat.length=length(arr.times) # count the length of patient arrivals
pat = seq(1:pat.length) # generate sequence for patient arrivals

# plotting each arrival time in given the i-th patient
plot(arr.times, pat, 
     main = "Arrival time vs. i-th position", 
     xlab = "Arrival time", ylab = "i-th patient", 
     type="l", col = "forestgreen") 
abline(lm(pat~arr.times),col='black')

# Conclusion: that plot shows the similar straight lines 

# Part b
# create a function to split the time interval into chunks
# count the arrivals using 'cut' that divides a range of arrival times 
# breaks aims to giving the number which arrival times to be cut
interval = function(k){
  x = table(cut(arr.times, breaks = k))
  return(x)
}

k.val = c(10,25,50,75,100,150) # k values to repeat the mean and variance
mean.total = vector() # produce a vector that counts mean of the given k values
var.total = vector() # produce a vector that counts variance 
lengths = vector()
lambda.total = vector() # produce a vector that counts lambdas

# loops aims to repeat an operation lots of times over  k values
# For loops have an indicator m which takes the value of a given vector
# count and add elements of mean, variance and lambda total using append

for (m in k.val){
  tmp = interval(m)
  mean.temp =mean(tmp)
  var.temp =var(tmp)
  length.interval = (arr.times[189]-arr.times[1])/m
  lambda.temp = mean.temp/length.interval
  mean.total = append(mean.total, mean.temp)
  var.total = append(var.total, var.temp)
  lambda.total = append(lambda.total, lambda.temp)
}

print(k.val)
print(lambda.total,3)
print(mean.total,3)
print(var.total,3)

plot(mean.total, var.total,
     main = " mean vs. variance",
     xlab = "mean", ylab = "variance",
     type="l", col = "forestgreen")
abline(a=0, b=1)

# Conclusion: The plot shows the similar value for their mean and variance
# That means distributions tend to follow Poisson Process

# Part c
# rexp produces an exponential distribution in rexp(n=size, rate=lambda)
int.times = rexp(n=189, rate=10) # 

# loops aims to repeat an operation lots of times over lambda values
# For loops have an indicator m which takes the value of a given vector
# count and add elements of mean, variance and lambda total using append

int.times.sum <- cumsum(int.times) 

arr.times.c <- subset(int.times.sum, int.times.sum<12)

length(arr.times.c) # count length of arr.times which gives 189 times
pat.length.c=length(arr.times.c) # count the length of patient arrivals
pat.c = seq(1:pat.length.c)

plot(x=arr.times.c, y=pat.c, 
       xlab = "Arrival-time", ylab = "i-th patient", 
       type = "l", col = "forestgreen")
abline(a=1, b=10)

mean.total.c = vector()
var.total.c = vector()

for (n in k.val){
  tmp = table(cut(arr.times.c, breaks=n))
  mean.temp =mean(tmp)
  var.temp =var(tmp)
  mean.total.c = append(mean.total.c, mean.temp)
  var.total.c = append(var.total.c, var.temp)
}

print(k.val)
print(mean.total.c,2)
print(var.total.c,2)

plot(mean.total.c, var.total.c,
     main = " mean vs. variance",
     xlab = "mean", ylab = "variance",
     type="l", col = "forestgreen")

# Conclusion: The plot shows the similar value for their mean and variance
# That means distributions tend to follow Poisson Process

# Part d
# Generate the interval times within arrivals, repeate procedure from Part c
# n=120 is computed using 12 hours multipled by 10 arrivals per hour
int.times.d =runif(n=120, min=0, max=0.1)
int.times.d.total = cumsum(int.times.d)

int.times.d.total <- subset(int.times.d.total, int.times.d.total<12)
# take interval times which are less than 12 hours

plot(int.times.d.total, 
     xlab = "i-th patient", ylab = "Arrival time", 
     type = "l", col = "forestgreen")

mean.total.d = vector()
var.total.d = vector()

for (n in k.val){
  tmp = table(cut(int.times.d.total, breaks=n))
  mean.temp =mean(tmp)
  var.temp =var(tmp)
  mean.total.d = append(mean.total.d, mean.temp)
  var.total.d = append(var.total.d, var.temp)
}

print(mean.total.d,3)
print(var.total.d,3)

plot(mean.total.d, var.total.d,
     ylim=c(0,10),
     main = " mean vs. variance",
     xlab = "mean", ylab = "variance",
     type="l", col = "forestgreen")
abline(a=0, b=1)



# Problem 2
# Part a
# create a simple function to compute log likelihood (log.ll) 
# for each Poisson distribution in the chosen range
ll.poisson <- function(theta, y){
  # log likelihood using sum 
  # dpois that generates the Poisson distribution with parameter lambda
  ll <- sum(dpois(y, theta, log = TRUE))
  # TRUE for probability p given as log(p)
  return(ll)
}

# The function can be applied for a likelihood function with some parameters. 

# Part b
thetas <- seq(0, 10, 0.01)
sample = c(12, 8, 14, 8, 11, 6, 13, 9, 9, 10)

# compute log likelihood for all lambda values
ll <- sapply(thetas, function(x){
  ll.poisson(x, sample)})

# store the lambdas and log likelihood results into a data frame
dflog <- data.frame(ll=ll, theta=thetas)

# plot using ggplot library
library(ggplot2)

# use dplyr to transform the data frame 
library(dplyr)
dflog%>%
  ggplot(aes(x=theta, y=ll))+
  geom_point(size=2, color="darkblue")+
  xlab("Theta")+ylab("Log Likelihood")+
  theme_bw(base_size = 7)+
  geom_vline(xintercept = thetas[which.max(ll)],
             color = "red", size=2)

# The result show that lambda=10 which is the same lambda set up early
# The evidence shows that MLE using Poisson model works

# Part c
# produce data which have Poisson distribution
dat.poisson <- rpois(n=100, theta2c=10) # as if lambda is unknown 
# reasonable range for the lambda
thetas2c <- seq(0, 10, by=0.01)

# create a simple function to compute likelihood 
new.function= function(samplevalues){
  # apply the optimise function to find the value fo theta that maximise 
  # return the value of theta that maximises the log-likelihood
  theta.max = optimise(llp, lower = 0, upper=100, 
                       y = samplevalues,
                       maximum= TRUE, tol=0.01)$maximum
  return(theta.max)
}

new.function(sample)

# produce an empty vector that stores all calculated thetas for each sample value
max.theta = vector()

for (m in dflog){
  print(new.function(m))
  max.theta = append(max.theta, new.function(m))
}

# Compute the average of all generated thetas
mean(max.theta)

# Part d
# A function which takes a sample size (so a single positive number) as input
# returns a vector of the mean and variance of the MLEs from the simulated 1000 samples. 
function.2d = function(n){
  mean.total.2d = vector()
  var.total.2d = vector()
  for (a in 1:1000){
    dat.2d = rpois(n, lambda=10)
    mean.temp =mean(dat.2d)
    var.temp =var(dat.2d)
    mean.total.2d = append(mean.total.2d, mean.temp)
    var.total.2d = append(var.total.2d, var.temp)
  }
  result.2d = c(mean.total.2d,var.total.2d)
  return(result.2d)
}


# Part e
p = c(10,25,50,75,100,150) # k values to repeat the mean and variance
mean.total.2e = vector() # produce a vector that counts mean of the given k values
var.total.2e = vector() # produce a vector that counts variance 

# loops aims to repeat an operation lots of times over  k values
# For loops have an indicator m which takes the value of a given vector
# count and add elements of mean, variance and lambda total using append

for (m in p){
  tmp = function.2d(m)
  mean.temp2 =tmp[1]
  var.temp2 =tmp[2]
  mean.total.2e = append(mean.total.2e, mean.temp2)
  var.total.2e = append(var.total.2e, var.temp2)
}

print(p)
print(mean.total.2e, 2)
print(var.total.2e, 2)

plot(p, mean.total.2e,
     xlab = "sample size",
     ylab = "mean",
     type = "l")

plot(p, var.total.2e,
     xlab = "sample size",
     ylab = "variance",
     type = "l")
  

