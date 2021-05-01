rm(list = ls())

library(rjags)
library(coda)

# Load in data
# Data provide sales prices and characteristics of houses sold in Ames, Iowa during 2006-2010
setwd("C:/Users/luked/Downloads")

data<-read.csv("housing.csv",header=T)

######################## Q1

dat<-na.omit(as.data.frame(cbind(data$SalePrice, data$Lot.Area, data$Garage.Area, data$Full.Bath, data$Fireplaces)))
N <- dim(dat)[1]

dat <- list(Y=dat$V1,x1=dat$V2,x2=dat$V3,x3=dat$V4,x4=dat$V5, N=N) 

params <- c("beta0", "beta1", "beta2", "beta3", "beta4", "tau", "taub0", "taub1", "taub2", "taub3", "taub4")

jags_model = "
model {
for (i in 1:N){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
}
beta0 ~ dnorm(0, taub0)
beta1 ~ dnorm(0, taub1)
beta2 ~ dnorm(0, taub2)
beta3 ~ dnorm(0, taub3)
beta4 ~ dnorm(0, taub4)
tau ~ dgamma(0.001, 0.001)
taub0 ~ dgamma(0.001, 0.001)
taub1 ~ dgamma(0.001, 0.001)
taub2 ~ dgamma(0.001, 0.001)
taub3 ~ dgamma(0.001, 0.001)
taub4 ~ dgamma(0.001, 0.001)
}" 

jags.m <- jags.model(textConnection(jags_model), data=dat, n.chains=5)
update(jags.m, n.iter=100000)
samp <- coda.samples(jags.m, variable.names=params, n.iter=5000)

summary(samp)

ols <- lm(Y ~ x1 + x2 + x3 + x4, data = dat)
summary(ols)

#### percent difference between bayesian and ols estimates

# b1(bayes) = 7.087e-01 , b1(ols) = 7.111e-01, % difference = -0.3375053
# b2(bayes) = 1.589e+02, b2(ols) = 1.579e+02, % difference = 0.6333122
# b3(bayes) = 4.447e+04, b3(ols) = 4.385e+04, % difference = 1.413911
# b4(bayes) = 3.222e+04, b4(ols) = 3.228e+04, % difference = -0.1858736



######################## Q2
rm(list = ls())
setwd("C:/Users/luked/Downloads")
data<-read.csv("housing.csv",header=T)

# add new regressors and tweak code 
dat2<-na.omit(as.data.frame(cbind(data$SalePrice, data$Lot.Area, data$Garage.Area, 
                                  data$Full.Bath, data$Fireplaces, data$Pool.Area,
                                  data$Screen.Porch, data$Bsmt.Full.Bath)))
N <- dim(dat2)[1]

dat2 <- list(Y=dat2$V1,x1=dat2$V2,x2=dat2$V3,x3=dat2$V4,x4=dat2$V5,x5=dat2$V6,
                       x6=dat2$V7, x7=dat2$V8, N=N)

params2 <- c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7",
             "tau", "taub0", "taub1", "taub2", "taub3", "taub4", "taub5", "taub6", "taub7")

jags_model_2 = "
model {
for (i in 1:N){
Y[i] ~ dnorm(mu[i],tau)
mu[i] <- beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i] + 
         beta5*x5[i] + beta6*x6[i] + beta7*x7[i]
}
beta0 ~ dnorm(0, taub0)
beta1 ~ dnorm(0, taub1)
beta2 ~ dnorm(0, taub2)
beta3 ~ dnorm(0, taub3)
beta4 ~ dnorm(0, taub4)
beta5 ~ dnorm(0, taub5)
beta6 ~ dnorm(0, taub6)
beta7 ~ dnorm(0, taub7)
tau ~ dgamma(0.001, 0.001)
taub0 ~ dgamma(0.001, 0.001)
taub1 ~ dgamma(0.001, 0.001)
taub2 ~ dgamma(0.001, 0.001)
taub3 ~ dgamma(0.001, 0.001)
taub4 ~ dgamma(0.001, 0.001)
taub5 ~ dgamma(0.001, 0.001)
taub6 ~ dgamma(0.001, 0.001)
taub7 ~ dgamma(0.001, 0.001)
}" 

jags.m2 <- jags.model(textConnection(jags_model_2), data=dat2, n.chains=5)
update(jags.m2, n.iter=10000)
samp <- coda.samples(jags.m2, variable.names=params2, n.iter=5000)

#### 2a
# burn in = 10,000 ; 
# check gelman rubin statistics
gelman.diag(samp, confidence = 0.95, transform=FALSE, autoburnin=TRUE,multivariate=FALSE)
par(mar=c(2.5,2.5,2.5,2.5))
gelman.plot(samp)
# the point estimate for all the betas is 1.00 so they are all fully converged

#### 2b

#posterior medians of coefficients
summary(samp)
# b1=6.014e-01, b2=1.470e+02, b3=4.846e+04, b4=2.887e+04
# b5=3.371e-03, b6=5.393e+01, b7=2.463e+04

# from previous example in lecture from 4/22: 
# b1=7.077e-01, b2=1.588e+02, b3=4.443e+04, b4=3.221e+04

# from new model compared to old one, changes in the betas are as follows:
# b1=-15.02049%, b2=-7.43073%, b3=9.070448%, b4=-10.36945%

#### 2c

# I would use the more expanded model to try to model house price. The pricing of houses
# is definitely more than a function of 4 variables, so taking into account 7 is better
# than 4.

######################## Q3

Y = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
X1 <- c(0.25, 3, 0.5, 0.4, 4, 3.5, 5.1, 0.15, 0.64, 0.32, 0.89, 1.2, 10, 8, 4.1, 1.1, 0.98, 2.3, 0.97, 0.09, 2.8, 3.9, 0.99, 0.45, 1.8, 6.2, 3.2, 0.88, 1.9, 0.29, 3.8, 2.8, 1.9, 0.23, 0.89, 0.09, 0.13, 1.9, 1.1, 4.2)
X2 <- c(1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1)

data <- as.data.frame(cbind(Y,X1,X2))
N <- dim(data)[1]

dat <- list(Y=data$Y, X1=data$X1, X2=data$X2, N=N) 

params <- c("beta0", "beta1", "beta2", "mu")

jags_model = "
model {
  for (i in 1:N) {
    Y[i] ~ dbern(mu[i])
    mu[i] <- ilogit(beta0 + beta1*X1[i] + beta2*X2[i])
  }
  beta0 ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
  beta2 ~ dnorm(0, 0.001)
}"

jags.m <- jags.model(textConnection(jags_model), data=dat, n.chains=5)
update(jags.m, n.iter=50000)
samp <- coda.samples(jags.m, variable.names=params, n.iter=5000)

# Store posterior medians
post_medians<-summary(samp)$quantiles[,3]

# Very loosely, we may wonder how accurate are the predicted probabilities?
# Assign predicted probabilities < 0.5 as as 0 and > 0.5 as a 1
accmat<-as.data.frame(cbind(dat$Y,post_medians[4:length(post_medians)],rep(0,N)))
accmat[which(accmat[,2]>0.5),3]<-1
colnames(accmat)<-c("Y","Predicted Prob","Predictions")

#### 3a & 3b
# we didn't predict a single one correctly, we predicted all zeros so we technically
# predicted all of the zeros correctly

#### 3c
# it's misleading because when one outcome occurs at a high frequency, you can make it
# appear that you are predicting lots of values correctly while simply just predicting
# the same value each and every time, like we did in this example. We had a 0% success 
# rate predicting the ones.

#### 3d
# we could try to increase the sample size of our vectors in order to let the model
# see more observations as a whole.


######################## Q4

rm(list = ls())

Y = c(1,1,3,3,3,2,1,3,3,3,1,1,1,1,2,2,2,3,1,2,2,2,3,1,3,2,2,3,1,2,3,2,1,1,2,3,3,1,2,2,3,3,1,2,3,1,1,1,2,3,2,3,1,2,1,3,3,2,2,1,3,3,1,1,2,3,1,2,3,1)
x1 = c(2.65, 4.25, 4.13, 4.12, 4.09, 3.87, 3.76, 2.72, 2.89, 2.93, 3.01, 3.29, 3.45, 3.47, 4.19, 4.22, 4.05, 3.89, 3.76, 3.50, 3.41, 3.27, 3.20, 3.18, 2.75, 2.78, 2.89, 2.92, 3.01, 3.67, 3.82, 3.41, 3.89, 4.21, 4.20, 3.66, 2.66, 2.89, 2.94, 2.89, 3.02, 3.89, 3.08, 3.24, 3.84, 3.89, 3.91, 4.01, 4.07, 4.16, 2.76, 2.88, 2.92, 3.04, 3.19, 3.09, 3.89, 4.11, 2.92, 3.19, 3.25, 3.82, 3.99, 2.81, 2.79, 4.02, 4.19, 3.29, 3.67, 3.22)
x2 = c(6.25, 6.50, 6.75, 10, 7.17, 8.12, 9.21, 6.23, 9.18, 10.22, 11.27, 12.19, 11.10, 9.92, 6.88, 7.21, 7.89, 8.22, 8.38, 8.90, 9.12, 9.89, 9.34, 10.25, 11.28, 12.01, 11.67, 12.99, 13.01, 7.18, 6.09, 10.87, 8.89, 9.21, 10.09, 10.75, 11.08, 11.09, 10.08, 10.32, 9.57, 7.88, 6.99, 10.08, 11.32, 11.08, 10.04, 7.62, 8.34, 9.19, 10.08, 10.99, 8.79, 7.62, 8.60, 7.99, 8.12, 8.08, 6.99, 7.24, 8.00, 7.76, 6.43, 7.59, 6.92, 7.03, 7.75, 7.89, 8.01, 7.50)
x3 = c(0.20,0.29,0.33,0.19,0.09,0.22,0.38,0.21,0.17,0.18,0.24,0.25,0.14,0.30,0.22,0.27,0.14,0.31,0.19,0.22,0.28,0.16,0.23,0.29,0.12,0.10,0.11,0.29,0.18,0.29,0.33,0.31,0.08,0.13,0.21,0.29,0.24,0.28,0.21,0.19,0.26,0.11,0.26,0.19,0.22,0.15,0.14,0.13,0.08,0.11,0.14,0.12,0.11,0.18,0.17,0.15,0.18,0.12,0.09,0.10,0.13,0.14,0.19,0.11,0.09,0.12,0.12,0.10,0.11,0.09)
x4 =c(1.50,1.55,1.60,1.62,1.78,2.23,2.19,2.50,1.81,1.93,2.01,2,2.47,2.33,1.89,1.98,2.02,
      2.13,2.23,2.17,2.09,2.22,2.53,1.82,1.89,1.58,1.96,1.99,2.54,1.88,2.23,2.24,2.17,2.19,
      1.68,1.64,1.55,2.03,2.42,2.09,2.11,2.30,1.57,1.71,2.34,2.08,1.62,1.54,1.79,2.08,1.91,
      2.19,1.89,1.97,2.22,2.56,1.93,1.98,2.09,2.07,2.46,2.52,1.94,1.50,1.78,2.14,2.13,1.58,
      2.08,2.09)

# N = number of data points
# Ncat = number of categories
N = 70
Ncat = 4

data <- as.data.frame(cbind(Y,x1,x2,x3,x4))
dat <- list(Y=data$Y, x1=data$x1, x2=data$x2, x3=data$x3, x4=data$x4, N=N, Ncat=Ncat) 

params <- c("beta02","beta03","beta21","beta22","beta23","beta24","beta31","beta32","beta33","beta34")

# We will set category 1, heating oil, as the reference category
# The below code is a minimal example; you can shorten it significanly by using loops

jags_model = "
model {
  for (i in 1:N) {
    Y[i] ~ dcat(mu[1:Ncat,i])
    mu[1:Ncat,i] <- expterm[1:Ncat,i]/sum(expterm[1:Ncat,i])
    expterm[1,i] <- exp(beta01 + beta11*x1[i] + beta12*x2[i] + beta13*x3[i] + beta14*x4[i])
    expterm[2,i] <- exp(beta02 + beta21*x1[i] + beta22*x2[i] + beta23*x3[i] + beta24*x4[i])
    expterm[3,i] <- exp(beta03 + beta31*x1[i] + beta32*x2[i] + beta33*x3[i] + beta34*x4[i])
  }
  # Coefficients of zero are assigned to the first category since it's the reference category:
  beta01 <- 0
  beta11 <- 0
  beta12 <- 0
  beta13 <- 0
  beta14 <- 0
  # Priors on all other coefficients in the non-reference category:
  beta02 ~ dnorm(0, 0.001)
  beta03 ~ dnorm(0, 0.001)
  beta21 ~ dnorm(0, 0.001) 
  beta22 ~ dnorm(0, 0.001)
  beta23 ~ dnorm(0, 0.001)
  beta24 ~ dnorm(0, 0.001)
  beta31 ~ dnorm(0, 0.001)
  beta32 ~ dnorm(0, 0.001)
  beta33 ~ dnorm(0, 0.001)
  beta34 ~ dnorm(0, 0.001)
}"

jags.m <- jags.model(textConnection(jags_model), data=dat, n.chains=5)
update(jags.m, n.iter=75000)
samp <- coda.samples(jags.m, variable.names=params, n.iter=10000)

# it's giving me an error when I try to run the jags.model:

# Error in jags.model(textConnection(jags_model), data = dat, n.chains = 5) : 
# RUNTIME ERROR:
#  Compilation error on line 5.
#  Index out of range taking subset of  expterm
