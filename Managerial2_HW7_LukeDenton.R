rm(list = ls())

library(rjags)
library(coda)

# Load in data
# Data provide sales prices and characteristics of houses sold in Ames, Iowa during 2006-2010
setwd("C:/Users/luked/Downloads")

data<-read.csv("housing.csv",header=T)

############ Q1

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



############ Q2
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

# we didn't predict a single one correctly, we predicted all zeros

# Can define a "hit rate" as the percentage of mis-predicted observations
sum(abs(accmat$Y-accmat$Predictions))/N