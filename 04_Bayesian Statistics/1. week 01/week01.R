theta <-  seq(from=0, to=1, by=0.01)
plot(theta, dbinom(3,8,theta))
dbeta(theta,shape1 = 4,shape2 = 8)

plot(theta, dbeta(theta,shape1 = 90, shape2 = 10), col='red')
plot(theta, dbeta(theta,shape1 = 9, shape2 = 1), col= 'green')

plot(theta, dbeta(theta,shape1 = 10, shape2 = 90), col='blue')
plot(theta, dbeta(theta,shape1 = 50, shape2 = 50), col='darkred')

plot(theta, dbeta(theta,shape1 = 1, shape2 = 9), col='darkred')
###################################################################################
d <- dpois(10,3)+dpois(10,2)+dpois(10,1)
n <- 1/3*(dpois(10,3))
n/d


plot(theta, dgamma(theta,shape = 50, rate = 50), col='darkred')
