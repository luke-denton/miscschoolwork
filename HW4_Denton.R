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


# Q3

# 3a
library(extraDistr)
genstudentt<-rlst(n=500000,df=100,mu=0,sigma=1) 
studentt<-rt(n=500000,df=100)

# histograms
hist(genstudentt, breaks = 50)
hist(studentt, breaks = 50)

# 3b
samp =length(data$ihsfruitveg)
df2 = samp-1
ybar = mean(data$ihsfruitveg)
s_sq = var(data$ihsfruitveg)

# find location and scale parameters
loc_paramter = ybar
scale_paramter = s_sq*sqrt(1 + (1/samp))

# now draw from gen student t dist with new loc and scale parameters
fruitgenstudentt <- rlst(n=1000, df = df2, mu = loc_paramter, sigma = scale_paramter) 
median(fruitgenstudentt)
# 2.145394 , very similar to median from draws from posterior


# Q4

# input data from hw sheet
y1 = 51
y2 = 84
y3 = 101
y4 = 203
y5 = 32 
y6 = 20
y7 = 9

# set hyperparamters
alpha1 = .001
alpha2 = .001
alpha3 = .001
alpha4 = .001
alpha5 = .001
alpha6 = .001
alpha7 = .001

sumalpha=(alpha1+alpha2+alpha3+alpha4+alpha5+alpha6+alpha7)

# Get 1,000 draws from the posterior for each of the probabilities 
# Save them in a 10000 x 8 matrix posterior matrix

total<-1000

post1<-alpha1+y1
post2<-alpha2+y2
post3<-alpha3+y3
post4<-alpha4+y4
post5<-alpha5+y5
post6<-alpha6+y6
post7<-alpha7+y7

postmat <- rdirichlet(total,c(post1,post2,post3,post4,post5,post6,post7))
# verify rows add to 1
rowSums(postmat)

# compare mean of draws to relative frequency

# means of posterior draws
colMeans(postmat)
# 0.10224305 0.16809625 0.20175025 0.40590235 0.06406072 0.04003159 0.01791580

# relative frequencies
n = 500
c(y1/n,y2/n,y3/n,y4/n,y5/n,y6/n,y7/n)
# 0.102 0.168 0.202 0.406 0.064 0.040 0.018

# now with means right above relative frequencies for ease of comparison
# 0.10224305 0.16809625 0.20175025 0.40590235 0.06406072 0.04003159 0.01791580
# 0.102      0.168      0.202      0.406      0.064      0.040      0.018

# the means of the posterior draws are very similar to the relative frequencies 
# within the data provided on the hw





