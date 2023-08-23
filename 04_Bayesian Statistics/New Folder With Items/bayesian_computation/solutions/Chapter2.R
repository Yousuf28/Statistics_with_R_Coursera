#Commands to run in R command line
library('LearnBayes')

# Exercise 1
p=seq(0,1, by=0.125)
prior=c(.001,.001,.950,.008,0.008,.008,0.008,0.008,0.008)
post=pdisc(p,prior,c(60,40))
plot(p,post,type='h', ylab='Posterior Probability')
cbind(p,prior,post)
print(post[3])

# Exercise 2
midp=seq(0.05,0.95, by=0.1)
prior=c(0,0,1,2,7,7,2,1,0,0)/20
s=13
f=7
p=seq(0,1,length=500)
prior=histprior(p,midp,prior)
like=dbeta(p,s+1,f+1)
post=like*prior
post=post/sum(post)
Simulated=sample(p,replace=TRUE, prob=post)
hist(Simulated, xlab='p')

# Exercise 5
#a)
mu=c(20,30,40,50,60,70)
prior=c(.1,.15,.25,.25,.15,.1)
#b)
y=c(38.6,42.4,57.5,40.5,51.7,67.1,33.4,60.9,64.1,40.1,40.7,6.4)
n=length(y)
ybar=mean(y)
#c)
sigma=10
like=exp(-n*(mu-ybar)^2/(2*sigma^2))
#d)
post=prior*like/sum(prior*like)

#e)
print(cbind(mu,post))
print(discint(cbind(mu,post),0.8))
#Note: asnwer is 40 50 with p = 0.9999959. Any smaller interval yields p<0.8