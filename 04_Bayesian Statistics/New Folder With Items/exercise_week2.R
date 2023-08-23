# week 02 , maximum likelyhood plotting 

likelihood <- function(n,y,theta){
    return(theta^y*(1-theta)^(n-y))
}

theta <- seq(from = 0.01, to = 0.99, by = 0.01)

plot(theta, likelihood(400, 72, theta))

# 72/400 = 0.18

abline(v=0.18)

# log function for maximum likelihood
loglike <- function(n,y,theta){
    return(y*log(theta)+(n-y)*log(1-theta))
}
plot(theta,loglike(400,72,theta), col= 'skyblue')
abline(v = .18)
max_like <- likelihood(400,72,theta)

# plot in ggplot

library(tidyverse)
library(ggthemes)

likelihood_data <- cbind(theta,max_like)
likelihood_data <- as.data.frame(likelihood_data)
glimpse(likelihood_data)

ggplot(data = likelihood_data)+
    geom_point(aes(x=theta, y= max_like))+
    geom_line(mapping = aes(x=theta, y=max_like))+
    geom_vline(xintercept = 0.18, color = 'red')+
    theme_fivethirtyeight()

