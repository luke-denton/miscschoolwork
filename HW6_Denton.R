######### 2a

rm(list = ls())

library(rjags)
library(coda)

Y <- c(15.5, 13.75, 30, 18.25, 16.5, 22.75, 29.25, 31.5, 42.67, 9.80, 11.25, 13.50, 18, 22, 61)
x <- c(1, 1, 6, 2, 2, 3, 4, 3, 5, 6, 1, 1, 2, 3, 20)

data <- as.data.frame(cbind(Y,x))
N <- length(data$Y)

dat <- list(Y=data$Y, x=data$x, N=N) 

params <- c("alpha", "beta", "tau")
inits <- list(alpha=5000, beta=-1000, tau=1000)

jags_model = "
model {
for (i in 1:N){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- alpha + beta * x[i]
}
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
sigma <- 1/sqrt(tau)
tau ~ dgamma(0.001, 0.001)
}" 

jags.m <- jags.model(textConnection(jags_model), data=dat, inits=inits, n.chains=5)
update(jags.m, n.iter=250)
samp <- coda.samples(jags.m, variable.names=params, n.iter=100)

summary(samp)

# Change the figure margins (too large)
# par(mar=c(2.5,2.5,2.5,2.5))
# plot(samp)

gelman.diag(samp, confidence = 0.95, transform=FALSE, autoburnin=TRUE,multivariate=FALSE)
gelman.plot(samp)

# a burn in of 250 draws helps us keep our gelman-rubin statistics routinely stay 
# under 1.10

######### 2b
rm(list = ls())

library(rjags)
library(coda)

x <- runif(1000,min=0,max=40)
Y <- 10 + 3.75*x + rnorm(1000,mean=0,sd=sqrt(100))

data <- as.data.frame(cbind(Y,x))
N <- length(data$Y)

dat <- list(Y=data$Y, x=data$x, N=N) 

params <- c("alpha", "beta", "tau")
inits <- list(alpha=31, beta=-18, tau=1/400)

jags_model = "
model {
for (i in 1:N){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- alpha + beta * x[i]
}
alpha ~ dnorm(0, 0.0001)
beta ~ dnorm(0, 0.0001)
sigma <- 1/sqrt(tau)
tau ~ dgamma(0.001, 0.001)
}" 

jags.m <- jags.model(textConnection(jags_model), data=dat, inits=inits, n.chains=5)
update(jags.m, n.iter=1000)
samp <- coda.samples(jags.m, variable.names=params, n.iter=100)

summary(samp)

# our median for alpha is 9.7848, median of beta is 3.72599, median of tau is 0.00965
# these median values are very close to the true values of our parameters, besides tau
# convergence is more difficult because of the random draws from the uniform dist

# trace plot and density plot; Change the figure margins (too large)
par(mar=c(2.5,2.5,2.5,2.5))
plot(samp)

gelman.diag(samp, confidence = 0.95, transform=FALSE, autoburnin=TRUE,multivariate=FALSE)
gelman.plot(samp)

######### 3 

rm(list=ls())
library(rjags)
library(coda)

n =1000
y=327
alpha =1
beta = 1

# Data Specification
dat = list(n=n,y=y,alpha=alpha,beta=beta)
params <- "theta"
inits <-list (theta=0.5)
bb_model = "model{
y~ dbinom(theta,n)
theta~dbeta(alpha,beta)
}"

bb_model <-jags.model(textConnection(bb_model), data=dat, inits=inits, n.chains =5)
update(bb_model,n.iter=300)
samp <-coda.samples(bb_model, variable.name=params, n.iter=200)
summary(samp)

# The median for the posterior of theta is 0.3275.

gelman.plot(samp)

# We have convergence based on our gelman-rubin statistic


######### 4

rm(list=ls())
library(rjags)
library(coda)

n=10
y= c(1, 138, 17, 12, 0, 289, 62, 89, 110, 34)
alpha = 1
beta =0.1

# Data Specification
dat2= list(n=n, y=y, alpha=alpha, beta=beta)
params <- "lambda"
inits <- list(lambda=6)

pg_model = "model{
for (i in 1:n){
y[i] ~ dpois(lambda)
}
lambda ~dgamma(alpha,beta)
}"

pg_model <-jags.model(textConnection(pg_model), data=dat2, inits=inits, n.chains =5)
update(pg_model,n.iter=400)
samp <- coda.samples(pg_model,variable.names = params,n.iter=100)

summary(samp)
# the median value of our posterior lambda is 74.58 with a burn in of 400

gelman.plot(samp)
# we have convergence

