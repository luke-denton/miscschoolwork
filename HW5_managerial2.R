# HW 5, Q1

rm(list = ls())

library(ggplot2)
library(MASS)
library(tidyverse)
library(mvtnorm)

# Parameters of the bivariate normal distribution
y1 = 0
y2 = 0
rho = 0.8
S = diag(2)
S[1,2] = rho
S[2,1] = rho

# Starting points
theta01 = -200
theta02 = 200

# Number of iterations
M = 5000

# We will have M draws of theta_1 and M draws of theta_2
# Set up placeholder matrix for the draws, with the first row as the starting point
draws = matrix(rep(0,2*M), ncol = 2)
draws[1,] = c(theta01, theta02)

# Loop to perform Gibbs Sampling
# Sample theta_2 first and then sample theta_1
for (t in 2:M){
  draws[t,2] = rnorm(1, mean=y2+rho*(draws[t-1,1]-y1),sd = sqrt(1-rho^2))
  draws[t,1] = rnorm(1, mean=y1+rho*(draws[t,2]-y2), sd = sqrt(1-rho^2))
  }

# First 100 draws
draws[1:100]

# On the 13th draw, theta1 and theta2 are within one sd of the mean of 0

################################################################################

#### HW5, Q2c
# Parameters of the beta posterior distribution
alpha = 2
beta = 2
n = 100

# Starting points
y1 = 1
theta01 = .5

# Number of iterations
M = 2000

# We will have M draws of y and theta
# Set up placeholder vector for the draws, with the first value as the starting point
ydraws = c(y1, rep(0,M-1))
thetadraws = c(theta01, rep(0,M-1))

# Loop to perform Gibbs Sampling
# Sample y first and then sample theta
for (i in 2:M){
  ydraws[i] = rbinom(1, size = n, prob = thetadraws[i-1])
  thetadraws[i] = rbeta(1, alpha + ydraws[i], beta + n - ydraws[i])
}
# examine draws
ydraws[1:100]
thetadraws[1:100]
# definitely need a burn-in

#### HW5, Q2d
burnin = 100

# create dfs for visualization and remove the burnin
ydrawsdf <- as_tibble(ydraws[-(1:burnin)])
thetadrawsdf <- as_tibble(thetadraws[-(1:burnin)])

# histogram of y draws minus the burn in 
ggplot(data = ydrawsdf,
       aes(x = value)) +
       geom_histogram() +
       theme_minimal()

# now theta
ggplot(data = thetadrawsdf,
       aes(x = value)) +
       geom_histogram() +
       theme_minimal()

# medians
median(ydraws[-(1:burnin)])
# 50
median(thetadraws[-(1:burnin)])
# 0.4988145

#### HW5, Q2e

# y trace plot
sb <- M-burnin
tracedatay = cbind.data.frame(rep(1:sb,times=1),c(ydrawsdf))
tracedatay$group=c(rep("y",sb))
colnames(tracedatay)=c("iter","value","group")

traceploty = ggplot(data = tracedatay) +
  geom_line(aes(iter, value, color = group)) +
  labs(title = 'Trace Plot') +
  scale_color_discrete(labels = c('y')) +
  theme(legend.position = 'bottom', legend.title = element_blank())

traceploty 

# theta trace plot
tracedatatheta = cbind.data.frame(rep(1:sb,times=1),c(thetadrawsdf))
tracedatatheta$group=c(rep("theta",sb))
colnames(tracedatatheta)=c("iter","value","group")

traceplottheta = ggplot(data = tracedatatheta) +
  geom_line(aes(iter, value, color = group)) +
  labs(title = 'Trace Plot') +
  scale_color_discrete(labels = c('y')) +
  theme(legend.position = 'bottom', legend.title = element_blank())

traceplottheta

################################################################################

#### HW5 Q3

# parameters
alpha = 2
beta = 2
lam = 5
# Starting points
y1 = 1
theta01 = .5
n1 = 10

# Number of iterations
M = 10000
burnin = 1000

# We will have M draws of y and theta and n
# Set up placeholder vector for the draws, with the first value as the starting point
ydraws = c(y1, rep(0,M-1))
thetadraws = c(theta01, rep(0,M-1))
ndraws = c(n1, rep(0,M-1))

# Loop to perform Gibbs Sampling
# Sample y first and then sample theta then n
for (i in 2:M){
  ydraws[i] = rbinom(1, size = n, prob = thetadraws[i-1])
  thetadraws[i] = rbeta(1, alpha + ydraws[i], beta + n - ydraws[i])
  ndraws[i] = rpois(1, lambda = lam*(1-thetadraws[i]))
}

# medians
median(ydraws[-(1:burnin)])
# 59
median(thetadraws[-(1:burnin)])
# 0.5934336
median(ndraws[-(1:burnin)])
# 2

################################################################################

#### HW5 Q4
# Parameters of the bivariate normal target distribution
y1 = 0
y2 = 0
S = diag(2)

scale = sqrt(0.001)
prop_sigma = (scale)^2*diag(2)

# Starting points
theta01 = -2.5
theta02 = 2.5

# Four chains
theta0 = c(c(-2.5,-2.5),c(-2.5,2.5),c(2.5,-2.5),c(2.5,2.5))

# Number of iterations
M = 5000

# We will have M draws of theta_1 and M draws of theta_2
# Set up placeholder matrix for the draws, with the first row as the starting point across four chains
draws = matrix(rep(0,8*M), ncol = 8)
draws[1,] = theta0

# Simple loop to perform Metropolis algorithm across four chains 
for (t in 2:M){
  prop1 = mvrnorm(1, mu = draws[t-1,1:2], Sigma=prop_sigma)
  u = runif(1, min = 0, max = 1)
  r = dmvnorm(x = prop1, mean = c(0,0), sigma = S)/dmvnorm(x = draws[t-1,1:2], mean = c(0,0), sigma = S)
  
  if(u<min(r,1)){
    draws[t,1:2] = prop1
  } else{ 
    draws[t,1:2] = draws[t-1,1:2]
  }
  
  prop2 = mvrnorm(1, mu = draws[t-1,3:4], Sigma=prop_sigma)
  u = runif(1, min = 0, max = 1)
  r = dmvnorm(x = prop2, mean = c(0,0), sigma = S)/dmvnorm(x = draws[t-1,3:4], mean = c(0,0), sigma = S)
  
  if(u<min(r,1)){
    draws[t,3:4] = prop2
  } else{ 
    draws[t,3:4] = draws[t-1,3:4]
  }
  
  prop3 = mvrnorm(1, mu = draws[t-1,5:6], Sigma=prop_sigma)
  u = runif(1, min = 0, max = 1)
  r = dmvnorm(x = prop3, mean = c(0,0), sigma = S)/dmvnorm(x = draws[t-1,5:6], mean = c(0,0), sigma = S)
  
  if(u<min(r,1)){
    draws[t,5:6] = prop3
  } else{ 
    draws[t,5:6] = draws[t-1,5:6]
  }
  
  prop4 = mvrnorm(1, mu = draws[t-1,7:8], Sigma=prop_sigma)
  u = runif(1, min = 0, max = 1)
  r = dmvnorm(x = prop4, mean = c(0,0), sigma = S)/dmvnorm(x = draws[t-1,7:8], mean = c(0,0), sigma = S)
  
  if(u<min(r,1)){
    draws[t,7:8] = prop4
  } else{ 
    draws[t,7:8] = draws[t-1,7:8]
  }
  
}    

burnin = 500
dfs=data.frame(th1=draws[(burnin+1):M,1],th2=draws[(burnin+1):M,2],th3=draws[(burnin+1):M,3],th4=draws[(burnin+1):M,4],th5=draws[(burnin+1):M,5],th6=draws[(burnin+1):M,6],th7=draws[(burnin+1):M,7],th8=draws[(burnin+1):M,8])

# Plot of M draws after burn-in of 500, Chain 1
labs2 <- c('Draws', '90% HPD')
c1 = ggplot() +
  geom_point(data = dfs,
             aes(th1, th2, color = '1'), alpha = 0.3) +
  stat_ellipse(data = dfs, aes(x = th1, y = th2, color = '2'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('steelblue', 'blue'), labels = labs2) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA), linetype = c(0, 1), alpha = c(1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

c2 = ggplot() +
  geom_point(data = dfs,
             aes(th3, th4, color = '1'), alpha = 0.3) +
  stat_ellipse(data = dfs, aes(x = th3, y = th4, color = '2'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('steelblue', 'blue'), labels = labs2) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA), linetype = c(0, 1), alpha = c(1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

c3 = ggplot() +
  geom_point(data = dfs,
             aes(th5, th6, color = '1'), alpha = 0.3) +
  stat_ellipse(data = dfs, aes(x = th5, y = th6, color = '2'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('steelblue', 'blue'), labels = labs2) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA), linetype = c(0, 1), alpha = c(1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

c4 = ggplot() +
  geom_point(data = dfs,
             aes(th7, th8, color = '1'), alpha = 0.3) +
  stat_ellipse(data = dfs, aes(x = th7, y = th8, color = '2'), level = 0.9) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  labs(x = 'theta1', y = 'theta2') +
  scale_color_manual(values = c('steelblue', 'blue'), labels = labs2) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA), linetype = c(0, 1), alpha = c(1, 1)))) +
  theme(legend.position = 'bottom', legend.title = element_blank())

c1
c2
c3
c4

# Trace plots: 4 chains per coefficients
sb <- M-burnin

tracedata_theta1=cbind.data.frame(rep(1:sb,times=2),c(dfs[,1],dfs[,3],dfs[,5],dfs[,7]))
tracedata_theta1$group=c(rep("th1_1",sb),rep("th1_2",sb),rep("th1_3",sb),rep("th1_4",sb))
colnames(tracedata_theta1)=c("iter","theta1","group")

tracedata_theta2=cbind.data.frame(rep(1:sb,times=2),c(dfs[,2],dfs[,4],dfs[,6],dfs[,8]))
tracedata_theta2$group=c(rep("th2_1",sb),rep("th2_2",sb),rep("th2_3",sb),rep("th2_4",sb))
colnames(tracedata_theta2)=c("iter","theta2","group")

traceplot_theta1 = ggplot(data = tracedata_theta1) +
  geom_line(aes(iter, theta1, color = group)) +
  labs(title = 'Trace Plot') +
  scale_color_discrete(labels = c('Chain1','Chain2','Chain3','Chain4')) +
  theme(legend.position = 'bottom', legend.title = element_blank())

traceplot_theta2 = ggplot(data = tracedata_theta2) +
  geom_line(aes(iter, theta2, color = group)) +
  labs(title = 'Trace Plot') +
  scale_color_discrete(labels = c('Chain1','Chain2','Chain3','Chain4')) +
  theme(legend.position = 'bottom', legend.title = element_blank())

# Show trace plots  
traceplot_theta1
traceplot_theta2
