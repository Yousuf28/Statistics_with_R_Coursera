library(tidyverse)
library(LearnBayes)

data("studentdata")
names(studentdata)

glimpse(studentdata)
attach(studentdata)
dim(studentdata)

hours_sleep <- WakeUp - ToSleep

hist(hours_sleep)
boxplot(hours_sleep ~ Gender)

plot(jitter(ToSleep), jitter(hours_sleep))

fit <- lm(hours_sleep ~ ToSleep)   
abline(fit)        

p <- seq(from= 0.05, to=0.95, by= 0.1)
prior <- c(1,5.2,8,7.2,4.6,2.1,0.7,0.1,0,0)
prior
prior <- prior/sum(prior)
prior
plot(p,prior, type = 'h', ylab = 'prior probability')


# posterior probability

data <- c(11,16) # 11 success and 16 failure
post <- pdisc(p, prior, data)
round(cbind(p, prior, post), 2)


## 

library(lattice)

PRIOR <- data.frame('prior', p, prior)
POST <- data.frame('posterior',p,post)
names(PRIOR) <- c('Type','P','Probability')
names(POST) <- c('Type','P','Probability')
data <- rbind(PRIOR,POST)

xyplot(Probability ~ P| Type, data = data, layout=c(1,2), type='h', lwd =3, col='black')
