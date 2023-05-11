# read data from url
url = "http://www.mas.ncl.ac.uk/~nseg4/teaching/MAS8380/practical2.dat"
arr.times = scan(url)

# ---------- Part a ---------- 
length(arr.times) # count length of arr.times which givens 189
patient.length=length(arr.times)
patient = seq(1:patient.length)

# plotting each arrival time in given the i-th patient
plot(arr.times, patient, main = "Arrival time vs. its position", 
     xlab = "Arrival time", ylab = "i-th patient", 
     type="l", col = "forestgreen") 
      abline(lm(patient~arr.times),col='black')

# ---------- Part b ---------- 
mean(x)
var(x)    


interval = function(k){
    x = table(cut(arr.times, breaks=k))
    return(x)
  }

kvalues=c(10,25,50,75,100,150)
meanTotal =  vector()
varTotal = vector()
lengths = vector()
lambdaTotal = vector()
for (a in kvalues){
  temp =  interval(a)
  meanTemp = mean(temp)
  varTemp = var(temp)
  lengthinterval =(arr.times[188]-arr.times[1])/a
  lambdaTemp = meanTemp/lengthinterval
  meanTotal = append(meanTotal, meanTemp)
  varTotal = append(varTotal, varTemp)
  lambdaTotal = append(lambdaTotal, lambdaTemp)
}

lambdaTotal
meanTotal
varTotal

plot(meanTotal,varTotal, main = "mean vs var", 
     xlab = "mean", ylab = "variance", type="l", col = "forestgreen")

# ---------- Part c ---------- 
# apply rexp for exponential distribution in rexp(n=size,rate=lambda)
# R "rate" is what we call Lambda, n is the sample size and 
# "rexp" stands for random generation from the exponential distribution
inter.times = rexp(n=188, rate=10)

for (lambda in 1:10){
  maxpatient <- 188
  
  # independent exponentially distributed waiting times T_n
  T <- rexp(maxpatient, rate = lambda)
  hist(T, xlab= "i-th patient", ylab="interarrival time T_n",
       main = paste("lambda=", lambda))
  # cumulative sum Y_n=T_1 + ..... + T_n
  Y <- cumsum(T)
  plot(Y, xlab="i-th patient", ylab = "Arrival time Y_n", type="l", col = "forestgreen") 
  
}

# ---------- Part d ---------- 



# generating the interval time between arrivals, n generated from 12 hours*10 arrivals per hour
it_times = c(rep(0,120))
lambda = 10
it_times = rexp(n=120, lambda)
arr.times.sum = cumsum(it_times)
arr.times2 = subset(arr.times.sum, arr.times.sum<12)
head(at)

length(arr.times) # count length of arr.times which givens 189
patient.length2=length(arr.times2)
patient2 = seq(1:patient.length2)

# plotting each arrival time in given the i-th patient
plot(arr.times2, patient2, main = "Arrival time vs. its position", 
     xlab = "Arrival time", ylab = "i-th patient", 
     type="l", col = "forestgreen") 
abline(b=10,col='black')

# the at vector is the comulative sum of the of vector
# meaening the 1st 2 values of the it vector are equal to the 2nd value
# of the at vector and so on

plot(at, 1:120, main = "Arrival time vs. its position", 
     xlab = "Arrival time", ylab = "i-th patient", 
     type="l", col = "forestgreen") 

abline(lm(patient~at),col='black')



# ---------- Part 2 ---------- 
# ---------- Part a ---------- 
log_likelihood <- function(n, theta){
  res = - theta + n*log(theta) - lgamma(n+1)#
  return(res)
}

# Part a(2)
llh_poisson <- function(lambda, y){
  # log(likelihood) by summing
  llh <- sum(dpois(y, lambda, log = TRUE))
  return(llh)
}


#--------- Part b -----------
lambdas <- seq(0, 20, 0.01)
sample = c(12, 8, 14, 8, 11, 6, 13, 9, 9, 10)

# compute log likelihood for all lambda values
ll <- sapply(lambdas, function(x){llh_poisson(x, sample)})
             
  
# save the lambdas and log likelihood in a data frame
df <- data.frame(ll=ll, lambda=lambdas)

library(dplyr)
library(ggplot2)

df %>% 
  ggplot(aes(x=lambda,y=ll))+
  geom_point(size=4,color="dodgerblue")+
  xlab("Lambda") +
  ylab("Log Likelihood")+
  theme_bw(base_size = 16) +
  geom_vline(xintercept = lambdas[which.max(ll)], color="red",size=2)


----
log.likelihood <- function(n, theta){
  theta = seq(0,3,0.01) # the value for maximising the likelihood function 
  L = theta*exp(-2*theta) # the likelihood function
  l = log(theta) - 2*theta # log-likelihood function 
  return(l)
}

log.likelihood=c

kvalues=c(10,25,50,75,100,150)
meanTotal =  vector()
varTotal = vector()
lengths = vector()

par(mfrow=c(1,2)) # par used to set or query graphical parameters
plot(theta,L,type="l") # plot log-likelihood function in second column
plot(theta,l,type="l",ylab="log(L)") # plot likelihood function in second column
par(mfrow=c(1,1))

# ---------- Part b ---------- 
x = c(12, 8, 14, 8, 11, 6, 13, 9, 9, 10)
theta = seq(0,3,0.01) # the value for maximising the likelihood function 
L = theta*exp(-2*theta) # the likelihood function
l = log(theta) - 2*theta # log-likelihood function 
par(mfrow=c(1,2)) # par used to set or query graphical parameters
plot(theta,L,type="l") # plot likelihood function in second column
plot(theta,l,type="l",ylab="log(L)") # plot log-likelihood function in second column
par(mfrow=c(1,1))

plot(theta,l,type="l",ylab="log(L)") # plot log-likelihood function in second column



# differentiate likelihood function to find the MLE dl/dtheta = 1/theta - 2
# then theta is 0.5 

# plot the PMF and CDF
px = rep(1/189,189) # Generates a vector 189s

plot(x, px, type="h", xlab="Outcome", ylab="Probability Mass Function", ylim=c(0,0.006) )
# ylim specifies the lower and upper limits of the y-axis
# type-"h: argument aims to joins the (x,y) coordinate of each point 

Fx = vector() # Create generic empty vector
for(i in 1 : 189){
  Fx[i] = i/189 # for 189 patients
}
Fx

plot(1:189,Fx, type="s", xlab="x", ylab="Cumulative Probability Function", ylim=c(0,1))
# Create plot with type="s" to produce a step-function

sum(x*px) # Expectation (expected value or mean) of a discrete random variable X

sum(x^2*px) # Count variance by estimating E[X^2]
Var_X=sum(x^2*px)-(sum(x*px))^2 # Count variance(X)

k = 10
x = table(cut(arr.times, breaks=k))
x # which contain the observations 
length(x)


# ---------- Part b ----------
n=12; p=0.5; k=10 # compute the probabilities associated with k = 10

k =seq(25,150,25); p = 0.5

plot(k,dgeom(k-1,p),type="h", ylab="Prob(X=k)")


x=seq(0,k)
dbinom(k,n,p) # PMF, which gives 0.04667544
pbinom(k,n,p) # CDF, which gives 0.05796765

# To plot the PMF and CDF
plot(x,dbinom(x,n,p),xlab="k",ylab="Prob(Y=k)",type="h")
plot(x,pbinom(x,n,p),type="s",xlab="y",ylab="F(y)")
plot(x,rbinom(x,n,p),type="s",xlab="y",ylab="F(y)")

# Calculate the mean and variance
Mean_Y=n*p
Mean_Y
Variance_Y=n*p*(1-p)
Variance_Y

Print(Mean=Mean_Y, Variance=Variance_Y)
k = vector() # Create generic empty vector
for(i in 1 : 6){
  k[i] = 25*i # for 189 patients
}
k

x =seq(0,189)


# count the number of arrivals in k = 10 intervals of equal length
count(x)



for(k in c(25, 50, 75, 100, 125, 150)){
  sum(x*px)
  Var_X
  print(mean_poisson,var_poisson)
}


# plot the PMF and CDF
x = seq(1,189) # creates a vector with entries 0, 1, 2, ....188, 189.
plot(x, dbinom(x,n,p), xlab="k", ylab="Prob(Y=k)", type="h", 
     main = "Probability mass function", cex = 1.2, col = "blue")
plot(x, pbinom(x,n,p), type="s", xlab="y", ylab="F(y)", 
     main = "Commulative distribution function", cex = 1.2, col = "blue")


# record the mean and variance


sum(x*px) # Expectation (expected value or mean) of a discrete random variable X

sum(x^2*px) # Count variance by estimating E[X^2]
Var_X=sum(x^2*px)-(sum(x*px))^2 # Count variance(X)




n=length(arr.times); p=0.5 # count length of arrival times and probability
dbinom(k,n,p) # Probability mass function, which gives
pbinom(k,n,p) # Commutative distribution function, which gives 

x = seq(1,6,1)
px = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
sum(x*px)
sum(x^2*px)
x = table(cut(x, breaks=k))


plot(1:189,px,type="s",xlab="x",ylab="Cumulative Probability",ylim=c(0,1))
# The argument type="s" specifies that we want a step-function.

Fx = c(1/6, 2/6, 3/6, 4/6, 5/6, 6/6)
plot(1:6,Fx,type="s",xlab="x",ylab="Cumulative Probability",ylim=c(0,1))

plot(x, dbinom(x,n,p), xlab="k", ylab="Prob(Y=k)", type="h", 
     main = "Probability mass function", cex = 1.2, col = "blue")
plot(x, pbinom(x,n,p), type="s", xlab="y", ylab="F(y)", 
     main = "Commulative distribution function", cex = 1.2, col = "blue")
plot(x, dbinom(x,n,p), xlab="k", ylab="Prob(Y=k)", type="h", 
     main = "Probability mass function", cex = 1.2, col = "blue")
plot(x, pbinom(x,n,p), type="s", xlab="y", ylab="F(y)", 
     main = "Commulative distribution function", cex = 1.2, col = "blue")

# Draw random values from this binomial distribution for a discrete random variable, 
draws <-rbinom(100, 189, p) # draw 100
brks <-(0:(189+1))-0.5
hist(draws, breaks=brks, main="Random draws from Bin(189, 0.5)")

# Applying a Poisson process, mean and variance 
mean_poisson = n*p
mean_poisson

var_poisson = n*p*(1-p)
var_poisson

x = table(cut(arr.times, breaks=k))
x # which contain the observations 
unique(x) # 15, 27, 19, 20, 16, 18, 22, 19, 21, 12

# Part b
for(k in 1 : 5){ # Remember the : shortcut for generating sequences going up in 1s
  print(i)
}

k = c(25,50, 75, 100, 150)
repeat {
  print(x)
  k = k+25
    mean_poisson = n*p
  if (x == 150){
    break
  }
}


simulated.proportions = numeric(length = 5) # Set up a vector to store the results
N = c(25, 50, 75, 100, 150) # Set up the number of repeats we want to use
for(i in 1 : 5)
  {dbinom[i] = dbinom(N[i], n, p) # Simulate the proportion and store it
}
plot(N, simulated.proportions,
     main = "Plot of Number of Repetitions vs Observed Proportion",
     xlab = "Number of Repetitions",
     ylab = "Observed Proportion")
abline(h = 0.117, col = "blue", lty = 2) # Add a horizontal line at the true probability



x = seq(1:189) # Produces a vector with entries 0, 1, 2, ..... 189
plot(x,dbinom(x, 189, 0.5),type="h", xlab="i-th patient", 
     ylab="probability", ylim=c(0,0.2))

ppois(180, lambda = 12)

# count the number of arrivals using interval of length L/k
x = table(cut(arr.times)

