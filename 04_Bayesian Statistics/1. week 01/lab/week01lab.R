library(statsr)
library(tidyverse)
bandit_posterior(data = data.frame(machine=c(1L,1L), outcome=c('W', 'L')))

data1 = data.frame(machine=c(1L), outcome = c('W'))
data2 = data.frame(machine=c(1L), outcome= c('L'))

bandit_posterior(data1)
bandit_posterior(data2)

bandit_posterior(data1) %>% 
    bandit_posterior(data2, prior = .)

data3 = data.frame(machine=c(1L,1L), outcome = c('W','W'))
data4 = data.frame(machine=c(2L,2L,2L), outcome= c('W','W','L'))

bandit_posterior(data3)
bandit_posterior(data4)

# question 04

bandit_posterior(data3) %>% 
    bandit_posterior(data4, prior = .)
# 05

bandit_posterior(data4) %>% 
    bandit_posterior(data3, prior = .)
