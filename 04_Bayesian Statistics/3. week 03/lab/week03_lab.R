library(statsr)
library(dplyr)
library(ggplot2)
data(nc)
glimpse(nc)

hist(nc$weight)

ggplot(data=nc, aes(nc$weight))+
    geom_histogram(bins = 20)


bayes_inference(y = weight, data = nc, statistic = "mean", type = 'ci', cred_level = .99)

inference(y = weight, data = nc, statistic = "mean", type = "ci", method = 'theoretical')

### larson stat book 379 page
bn <- rnorm(30,mean = 66900, sd = 5500)
t.test(x = bn, mu = 68000,alternative = 'less', conf.level = .95)


### hypothesis test frequentist

nc_weight <- nc$weight
summary(nc_weight)
sd(nc$weight)
t.test(nc_weight, mu = 7, sd=1.509, alternative = 'two.sided', conf.level = .95)



#bayes

bayes_inference(y = weight, data = nc, statistic = "mean", type = "ht", null = 7, alternative = "twosided")


ggplot(nc)+
    geom_boxplot(mapping = aes(x=habit, y= weight))


bayes_inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, alternative = "twosided", hypothesis_prior = c(H1 = 0.2, H2 = 0.8))


bayes_inference(y = lowbirthweight, data = nc, success = "low", statistic = "proportion", type = "ht", null = 0.075, alternative = "twosided", beta_prior = c(75,925))

bayes_inference(y = lowbirthweight, data = nc, success = "low", statistic = "proportion", type = "ht", null = 0.075, alternative = "twosided", hypothesis_prior = c(H1 = 0.2, H2 = 0.8))
