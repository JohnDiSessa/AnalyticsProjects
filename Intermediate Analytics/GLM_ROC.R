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

options(scipen = 999)

College = ISLR::College

summary(College)
str(College)

College$Private <- ifelse(College$Private=="Yes",1,0)



plyr::count(College$Private)
count <- as.data.frame(plyr::count(College$Private))
colnames(count) <- c("Private","Count")
count

#EDA#
pie <- ggplot(count,aes(x="",y=Count,fill=Private))+geom_bar(stat="identity",width=1)
pie

barplot(count$Count, main = "Public vs Private",xlab = "School Type",
        ylab = "Count",names.arg = c("Public","Private"),col = "coral1")
text(.7,100,"212")
text(1.92,290,"565")

hist(College$Grad.Rate,main = "Graduation Rate", xlab="",col = "coral1")
mean(College$Grad.Rate)
mean(College$F.Undergrad)
mean(College$Expend)

#Machine Learning#
input_ones <- College[which(College$Private==1),]
input_zeros <- College[which(College$Private==0),]

set.seed(3456)

no_bias_row_number <- nrow(input_zeros) # smallest amount
input_ones_no_bias <- head(input_ones, no_bias_row_number) # from all the O's 24720 Select only 7841
input_zeros_no_bias <- input_zeros
# Split the balanced datasets itno the training and test
rnd_row_indices <- sample(1:nrow(input_ones_no_bias), .7*no_bias_row_number)

ones_training <- input_ones_no_bias[rnd_row_indices,]  # 1's for training - randomly extract
zeros_training <- input_zeros_no_bias[rnd_row_indices,] # 0's for training. Pick as many 0's as 1's to eliminate bias, i.e. ensure equal proportions

trainingData <- rbind(ones_training, zeros_training)
trainingData # row bind the 1's and 0's
table(trainingData$Private)

# Create Test Data same logic as train just `-rnd_row_indices`
test_ones <- input_ones_no_bias[-rnd_row_indices, ] # 1's for testing, - randomly extract
test_zeros <- input_zeros_no_bias[-rnd_row_indices, ] # 0's for testing, Pick as many 0's as 1's to eliminate bias, i.e. ensure equal proportions

testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 
cat("Training dataset size is = [", dim(trainingData), "] testing is = 
    [", dim(testData), "]\n")

table(testData$Private)


#model building#
model <- glm(Private~., family = "poisson", data=trainingData)
model
summary(model)

#have all variables predicting graduation rate#
#but want to find the optimal amount of variables#

submodels <- regsubsets(Private~., data = trainingData,nvmax=4)
summary(submodels)
res.sum <- summary(submodels)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic))

#now we know the best model has 4 predictors#


model2 <- glm(Private~F.Undergrad+Outstate+Terminal+perc.alumni, 
              family = "poisson", data=trainingData)
model2
summary(model2)

#confusion matrix#
predicted <- predict(model2,testData,type = "response")
optimal <- optimalCutoff(testData$Private,predicted)[1]
optimal
cm <- confusionMatrix(testData$Private,predicted,threshold = optimal)
cm

n_distinct(caret_train$Private)
plyr::count(caret_train$Private)

#public = 0, private = 1#


cm2 <- matrix(c(cm[1,1],cm[2,1],cm[1,2],cm[2,2]),ncol = 2)
rownames(cm2) <- c("Predicted:Public","Predicted:Private")
colnames(cm2) <- c("Actual:Public","Actual:Private")
cm2
fourfoldplot(cm2,color = c("skyblue","coral1"),conf.level = .95)


accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,1]+cm[1,2]+cm[2,2])
accuracy
precision = 58/(58+6)
precision
recall = cm[1,1]/(cm[1,1]+cm[1,2])
recall
specificity = 62/(62+6)
specificity

cm[1,1]
cm[2,1]
cm[1,2]
cm[2,2]

#ROC#
pROC_obj <- roc(trainingData$Private,trainingData$Enroll,
                smoothed = TRUE,# arguments for ci ci=TRUE,
                ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)
sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
