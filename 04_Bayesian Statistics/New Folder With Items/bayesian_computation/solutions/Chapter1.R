#Commands to run in R command line
#library('LearnBayes')
#data(studentdata)
#print(studentdata[1:10,])
#attach(studentdata)

# Exercise 1
#a)
hist(Dvds)
#b)
print(summary(Dvds))
#c)
print(table(Dvds))
barplot(table(Dvds))
#People tend to collect "whole" number of DVDs, i.e. 10, 15, 30...

# Exercise 2
#a)
boxplot(Height~Gender)
#b)
output=boxplot(Height~Gender)
print(output)
#c)
print(output$stats[3,2]-output$stats[3,1])

# Exercise 3
#a)
plot(ToSleep,WakeUp)
#b)
fit=lm(WakeUp~ToSleep)    
print(fit)
#Note: treat y~x as y=ax+b 
#c)
abline(fit$coefficients[[1]],fit$coefficients[[2]])
#d)
time=fit$coefficients[[1]]+fit$coefficients[[2]]*0
print(time)

# Exercise 4
#a)
binomial.conf.interval=function(y,n)
{
	z=qnorm(.95)
	phat=y/n
	se=sqrt(phat*(1-phat)/n)
	return(c(phat-z*se,phat+z*se))
}
#b)
n=20
p=.5
SimulateN=2000           		   #Use 20 to match the task
y=rbinom(SimulateN,n,p)
Covered=0
for (i in 1:SimulateN)
{
	ci=binomial.conf.interval(y[i],n)
	Covered[i]=(ci[1]<p)&(p<ci[2])
}
print(sum(Covered)/SimulateN)
#c)
n=20
p=.05
SimulateN=2000				   #Use 20 to match the task
y=rbinom(SimulateN,n,p)
Covered=0
for (i in 1:SimulateN)
{
	ci=binomial.conf.interval(y[i],n)
	Covered[i]=(ci[1]<p)&(p<ci[2])
}
print(sum(Covered)/SimulateN)
#Note: in c) the coverage probability deviates from theoretical 
#      because of poor normal approximation of phat for small values p.
#      Compare hist(y) in c) and d)