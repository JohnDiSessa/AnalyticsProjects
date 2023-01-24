#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('readxl')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(readxl)

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
library(sjPlot)
library(RColorBrewer)
library(data.table)
library(formattable)

options(scipen = 999)

#Reading the data set as a data frame
protest <- read_excel("GlobalProtestTracker2.xlsx")

# structure of the data
str(protest)

# number of rows with missing values
nrow(protest) - sum(complete.cases(protest))

#changing variables to factors
protest$Country <- as.factor(protest$Country)
protest$`Start_Date` <- as.Date(protest$`Start_Date`)
protest$Freedom_Rating <- as.factor(protest$Freedom_Rating)
protest$`Peak_Size` <- as.factor(protest$Peak_Size)
protest$Duration <- as.factor(protest$Duration)
protest$Key_Participants <- as.factor(protest$Key_Participants)
protest$Active <- as.factor(protest$Active)
protest$Economic_Motivation <- as.factor(protest$Economic_Motivation)
protest$Political_Motivation <- as.factor(protest$Political_Motivation)
protest$Corruption_Motivation <- as.factor(protest$Corruption_Motivation)
protest$Size_Category <- as.factor(protest$Size_Category)
protest$Significant_Outcome <- as.factor(protest$Significant_Outcome)
protest$Large_Protest <- as.factor(protest$Large_Protest)
protest$Violent_Gov_Response <- as.factor(protest$Violent_Gov_Response)
protest$Long_Protest <- as.factor(protest$Long_Protest)
protest$Coronavirus_Related <- as.factor(protest$Coronavirus_Related)



str(protest)

#analyzing the odor variable
table(protest$Significant_Outcome,protest$Size_Category)
table(protest$Significant_Outcome,protest$Duration)

number.perfect.splits <- apply(X=protest[-1], MARGIN = 2, FUN = function(col){
  t <- table(protest$Significant_Outcome,col)
  sum(t == 0)
})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,5,2,2))
barplot(number.perfect.splits,
        main="Number of Perfect Splits",
        xlab="",ylab="Feature",las=2,col="light blue")
#duration has the most perfect splits becaues there are many categories with 1 or 2 protests#



#data splicing
set.seed(12345)
train <- sample(1:nrow(protest),size = ceiling(0.80*nrow(protest)),replace = FALSE)
# training set
protest_train <- protest[train,]
# test set
protest_test <- protest[-train,]

# penalty matrix
penalty.matrix <- matrix(c(0,1,1,0), byrow=TRUE, nrow=2)

# building the classification tree with rpart
tree <- rpart(Significant_Outcome~.,
              data= protest_train,
              parms = list(loss = penalty.matrix),
              method = "class")

# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)

# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# tree pruning using the best complexity parameter
tree <- prune(tree, cp=cp.optim)
tree

#Testing the model
pred <- predict(object=tree,protest_test[-1], type = 'class')

#Calculating accuracy
t <- table(protest_test$Significant_Outcome,pred) 
confusionMatrix(t) 





