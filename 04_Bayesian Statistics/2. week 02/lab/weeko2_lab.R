library(tidyverse)
library(statsr)
data("brfss")

#from q-04
#frequntist approch

count_sex <- table(brfss$sex)
count_sex <- as.tibble(count_sex )

total_population <- sum(count_sex$n)
male <- count_sex[1,2]
female <- count_sex[2,2]
male
female
male_prop <- male/total_population
female_prop <- female/total_population
male_prop
female_prop

#confidence interval for female propportion
# female_prop = 0.5172

p_hat <- 0.5172
se <- sqrt((p_hat*(1-p_hat))/5000)
up <- p_hat+(1.96*se)
low <- p_hat-(1.96*se)
up
low

# interpretation is different
# if we repeat experiment in an  infinit number and build confident interval then  95% of all confident interval will contain true parameter

# bayesian
#The probability that the true proportion of females lies in this interval is 0.95.

n <- length(brfss$sex)
x <- sum(brfss$sex=='Female')
n
x
# since prior beta(1,1), posterior beta(alpha+success, beta+failure)
qbeta(p=c(0.025,0.975), shape1 = 2587, shape2 = 2415)

qbeta(p=c(0.025,0.975), shape1 = 3086, shape2 = 2914)

qbeta(p=c(0.025,0.975), shape1 = 2591, shape2 = 2614)


