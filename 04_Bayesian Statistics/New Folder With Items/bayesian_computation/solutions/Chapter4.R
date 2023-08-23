# Exercise 1
#a)
y=c(9, 8.5, 7, 8.5, 6, 12.5, 6, 9, 8.5, 7.5, 8, 6, 9, 8, 7, 10, 9, 7.5, 5, 6.5)
S=sum((y-mean(y))^2)
n=length(y)
sigma2=S/rchisq(1000,n-1)
mu=rnorm(1000, mean = mean(y), sd=sqrt(sigma2/n))
plot(mu,sigma2)
#b)
quantile(mu,c(0.05,0.95))
quantile(sqrt(sigma2),c(0.05,0.95))
#c)
p75=mu+0.674*sqrt(sigma2)
cbind(mean(p75),sqrt(var(p75)))

# Exercise 2
#a) 
#Since the samples are independent, the likelyhood L(x1,x2,..xm, y1,y2,..yn) 
#is the product of [f1(x1)*..f1(xm)]*[f2(y1)*..f2(ym)], where f1 and f2 are normal densities
#with means mu1 and mu2 and standard deviation sigma1 and sigma2 accordingly.
#The prior is a product of individual uninformative priors 1/sigma1^2 and 1/sigma2^2.
#The posterior density is then a product of the individual posterior densities 
# 1/(sigma1^2)^(m/2+1)*exp(-1/(2*sigma1^2)*(S1+m*(mu1-xbar)^2)) and
# 1/(sigma2^2)^(n/2+1)*exp(-1/(2*sigma2^2)*(S2+n*(mu2-ybar)^2)), where xbar and ybar are sample means
# of samples x and y respectively, S1 and S2 are given by sum(xi-xbar)^2 sum(yi-ybar)^2 and .
# The vectors (mu1,sigma1) and (mu2,sigma2) are independent since their joint posterior density
# is a product of their individual posterior density functions.
# P.S. There is no such notion as "independent distributions".
#b)
#Simulate (mu1,sigma1) as in exercise 1, using sample x. In the very same way, simulate (mu2,sigma2)
#using sample y. Combine the values into (mu1,sigma1,mu2,sigma2).
#c)
x=c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
S=sum((x-mean(x))^2)
n=length(x)
sigma2=S/rchisq(1000,n-1)
mux=rnorm(1000, mean = mean(x), sd=sqrt(sigma2/n))

y=c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)
S=sum((y-mean(y))^2)
m=length(y)
sigma2=S/rchisq(1000,m-1)
muy=rnorm(1000, mean = mean(y), sd=sqrt(sigma2/m))

d=mux-muy
hist(d)
CI=quantile(d,c(0.025,0.975))
print(CI)
print(mean(d))
# Since the 95% confidence interval does not contain zero, we reject the hypothesis that male and female
# means are equal at 5% significance level.
#    2.5%    97.5% 
#1.467182 7.853980 