set.seed(20170703L)
DF_1 <- data.frame(id = sample(1:2, 9L, TRUE),
                 code = sample(letters[1:3],
                  9, TRUE), valA = 1:9,
                   valB = 10:18,
                    stringsAsFactors = FALSE)
DF_2 <- data.frame(id = c(3L, 1L, 1L, 2L, 3L),
 code = c("b", "a", "c", "c", "d"),
                 mul = 5:1,
                 stringsAsFactors = FALSE)


plot(df$mpg)

library(ISLR)
library(caret)
library(arm)
library(Ecdat)
library(gridExtra)


data('Hedonic')
inTrain <- createDataPartition(y = Hedonic$tax, p = 0.7, list = FALSE)

trainingset <- Hedonic[inTrain,]
# 356 * 15



testingset <- Hedonic[-inTrain,]

# 150 * 15


ols.reg <- lm(tax~. , trainingset)
summary(ols.reg)

# predict
ols.regTest <- predict.lm(ols.reg, testingset, interval = 'prediction', se.fit = T)

# correlation (r) test value vs predic value

cor(testingset$tax, ols.regTest$fit[,1])

summary(ols.regTest$fit[,1])

summary(trainingset$tax)


# MAE  mean absolute error

MAE <- function(actuals, predicted){
    mean(abs(actuals-predicted))
}

MAE(ols.regTest$fit[,1], testingset$tax)


yout.ols <- as.data.frame(cbind(testingset$tax, ols.regTest$fit))
yout.ols


ols.upr <- yout.ols$upr
ols.lwr <- yout.ols$lwr


p.ols <- ggplot(data = yout.ols, aes(x = testingset$tax, y = ols.regTest$fit[,1]))+
    geom_point()+
    ggtitle('ordianry regression')+
    labs(x = 'actual', y ='predicted')
p.ols+geom_errorbar(ymin = ols.lwr, ymax= ols.upr)


### bayes

bayes.reg<-bayesglm(tax~.,family=gaussian(link=identity),trainingset,prior.df = Inf)


bayes.regTest<-predict.glm(bayes.reg,newdata = testingset,se.fit = T)

cor(testingset$tax,bayes.regTest$fit)

summary(bayes.regTest$fit)

summary(trainingset$tax)

MAE(bayes.regTest$fit, testingset$tax)

yout.bayes <- as.data.frame(cbind(testingset$tax,bayes.regTest$fit))

names(yout.bayes) <- c("tax", "fit")

critval <- 1.96 #approx for 95% CI

bayes.upr <- bayes.regTest$fit + critval * bayes.regTest$se.fit

bayes.lwr <- bayes.regTest$fit - critval * bayes.regTest$se.fit

bayes_con <- as.data.frame(cbind(bayes.regTest$fit, bayes.lwr,bayes.upr))

# plot

p.bayes <- ggplot(data = yout.bayes, aes(x = yout.bayes$tax,
                                         y = yout.bayes$fit)) + 
    geom_point() + 
    ggtitle("Bayesian Regression Prediction") + 
    labs(x = "Actual", y = "Predicted")



ols.plot <-  p.ols + geom_errorbar(ymin = ols.lwr, ymax = ols.upr)

bayes.plot <-  p.bayes + geom_errorbar(ymin = bayes.lwr, ymax = bayes.upr)
grid.arrange(ols.plot,bayes.plot,ncol=2)


# comparison
head(ols.regTest$fit)
head(bayes_con)
head(testingset$tax)

# (https://educationalresearchtechniques.com/2017/10/18/linear-regression-vs-bayesian-regression/)
