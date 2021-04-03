# HW 5, Q1

rm(list = ls())

library(ggplot2)
library(MASS)
library(tidyverse)

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
y1 = 1
alpha = 2
beta = 2
n = 100

# Starting point
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
