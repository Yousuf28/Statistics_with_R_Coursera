library(statsr)
library(MASS)
library(dplyr)
library(ggplot2)
library(BAS)


data(wage)

hist(wage$wage)
boxplot(wage$wage)
summary(wage$wage)
table(wage$wage < 300)

m_wage_iq = lm(wage ~ iq, data = wage)

plot(m_wage_iq, which = 2)
plot(wage$iq, wage$wage)
hist(wage$iq)
hist(m_wage_iq$residuals)

summary(m_wage_iq)


# Q 4

m_lwage_iq <- lm(lwage ~ iq, data = wage)
plot(m_lwage_iq, which = 2)
(m_lwage_iq$coefficients)
confint(m_lwage_iq,level = .95)


# 5

m_lwage_full <- lm(lwage ~ . -wage, data= wage)
m_lwage_full
summary(m_lwage_full)

BIC(m_lwage_full)
m_lwage_nobrthord = lm(lwage ~ . -wage -brthord, data = na.omit(wage))
BIC(m_lwage_nobrthord)

m_lwage_sibs = lm(lwage ~ . -wage -sibs, data = na.omit(wage))
BIC(m_lwage_sibs)


m_lwage_feduc= lm(lwage ~ . -wage -feduc, data = na.omit(wage))
BIC(m_lwage_feduc)

m_lwage_meduc = lm(lwage ~ . -wage -meduc, data = na.omit(wage))
BIC(m_lwage_meduc)


#####

wage_no_na = na.omit(wage)
bma_lwage = bas.lm(lwage ~ . -wage, data = wage_no_na,
                   prior = "BIC", 
                   modelprior = uniform())
bma_lwage
summary(bma_lwage)
######


wage_red = wage %>%
    select(-sibs, -brthord, -meduc, -feduc)
wage_no_na_red = na.omit(wage_red)
bma_lwage1 = bas.lm(lwage ~ . -wage, data = wage_no_na_red,
                   prior = "BIC", 
                   modelprior = uniform())

bma_lwage1
summary(bma_lwage)
######