library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(ggthemes)
library(moments)
library(sjPlot)
library(RColorBrewer)
library(data.table)
library(formattable)
library(corrplot)
library(Hmisc)
library(car)
library(EnvStats)
library(tidyverse)
library(caret)
library(leaps)
library(DescTools)
library(RcmdrMisc)
library(ISLR)
library(caret)
library(InformationValue)
library(ROCR)
library(grid)
library(devtools)
library(ggthemes)
library(mlbench)
library(pROC)
library(glmnet)

options(scipen = 999)

College = ISLR::College

summary(College)
str(College)

College$Private <- ifelse(College$Private=="Yes",1,0)

set.seed(999)

hist(College$Grad.Rate,main = "Graduation Rate", xlab="",col = "coral1")
mean(College$Grad.Rate)
median(College$Grad.Rate)

trainindex <- sample(x=nrow(College),size=nrow(College)*.7)
train <- College[trainindex,]
test <- College[-trainindex,]

trainx <- model.matrix(Grad.Rate~.,train)[,-1]
testx <- model.matrix(Grad.Rate~.,test)[,-1]

trainy <- train$Grad.Rate
testy <- test$Grad.Rate


cv.ridge <- cv.glmnet(trainx,trainy,nfolds = 10,alpha = 0)
plot(cv.ridge)

log(cv.ridge$lambda.min)
log(cv.ridge$lambda.1se)

ridgemodelmin <- glmnet(trainx,trainy,alpha=0,lambda = cv.ridge$lambda.min)
ridgemodelmin
coef(ridgemodelmin)

ridgemodel1se <- glmnet(trainx,trainy,alpha=0,lambda = cv.ridge$lambda.1se)
ridgemodel1se
coef(ridgemodel1se)

preds_train2 <- predict(ridgemodelmin,newx = trainx)
train_rmse2 <- RMSE(trainy,preds_train2)
train_rmse2

preds_test2 <- predict(ridgemodelmin,newx = testx)
test_rmse2 <- RMSE(testy,preds_test2)
test_rmse2

preds_train <- predict(ridgemodel1se,newx = trainx)
train_rmse <- RMSE(trainy,preds_train)
train_rmse

preds_test <- predict(ridgemodel1se,newx = testx)
test_rmse <- RMSE(testy,preds_test)
test_rmse

#LASSO#
set.seed(999)

cv.lasso <- cv.glmnet(trainx,trainy,nfolds = 10)
plot(cv.lasso)

log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

lassomodelmin <- glmnet(trainx,trainy,alpha=1,lambda = cv.lasso$lambda.min)
lassomodelmin
coef(lassomodelmin)

lassomodel1se <- glmnet(trainx,trainy,alpha=1,lambda = cv.lasso$lambda.1se)
lassomodel1se
coef(lassomodel1se)

preds.train3 <- predict(lassomodelmin,newx = trainx)
train.rmse3 <- RMSE(trainy,preds.train3)
train.rmse3

preds.test4 <- predict(lassomodelmin,newx = testx)
test.rmse4 <- RMSE(testy,preds.test4)
test.rmse4

preds.train5 <- predict(lassomodel1se,newx = trainx)
train.rmse5 <- RMSE(trainy,preds.train5)
train.rmse5

preds.test6 <- predict(lassomodel1se,newx = testx)
test.rmse6 <- RMSE(testy,preds.test6)
test.rmse6

modelstep <- step(lm(Grad.Rate~.,data = College),direction = "both")
summary(modelstep)
