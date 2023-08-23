
library(tidyverse)
dat <- read.table('pgalpga2008.dat')
glimpse(dat)
dat %>% as.tibble()

glimpse(dat)

ggplot(data = dat)+
    geom_boxplot(aes(x=V3, y= V1))

ggplot(data = dat, aes(V1,V2))+
    geom_point(aes(colour=factor(V3)))

datF <- subset(dat, V3==1, select=1:2)

model <- lm(V2~V1, datF)
summary(model)

