# HW4 Q2

data<-read.csv("CEX.csv",header=T)

# 2a
summary(data$fruitveg)
sd(data$fruitveg)
# mean = 12.17
# median = 6.17
# sd = 17.12968

summary(data$fruitveg[which(data$week == 1)])
# week 1 median = 12.622

summary(data$fruitveg[which(data$week == 2)])
# week 2 median = 5.834

length(data$fruitveg[which(data$fruitveg == 0)])
# 3414 observations with fruitveg = 0

# 2b
hist(data$fruitveg, breaks = 50)

# 2c 
data$ihsfruitveg <- asinh(data$fruitveg)
hist(data$ihsfruitveg, breaks = 50)

# 2d
# load the geoR package 
library(geoR)

ybar=mean(data$ihsfruitveg)
s_sq = var(data$ihsfruitveg)

# Define sample size, degrees of freedom, and total number of draws
samp=length(data$ihsfruitveg)
df2 = samp-1
total <- 1000

# Create placeholder vectors for the posterior variance, posterior mean, and posterior predictions
post_norm_var<-rep(0,total)
post_norm_mu<-rep(0,total)

# Loop
# Step 1: get a draw for the variance
# Step 2: get a draw for the mean, conditional on what you just drew in (1) for the variance
# Repeat the steps, in this example, 1000 times by setting total = 1000
for (i in 1:total){
  post_norm_var[i]=rinvchisq(n=1, df2, scale=s_sq)
  post_norm_mu[i]=rnorm(n=1,mean=ybar,sd=sqrt(post_norm_var[i]/samp))
}

median(post_norm_mu)
# 2.142314
ybar
# 2.141911

# 95% Credible intervals for mean and variance
quantile(post_norm_mu,c(0.025,0.975))
#   2.5%    97.5% 
# 2.110981 2.173820 
quantile(post_norm_var,c(0.025,0.975))
#   2.5%    97.5% 
# 2.718937 2.867966 

# 2e
drawsback<-sinh(post_norm_mu)
hist(drawsback, breaks = 50)
