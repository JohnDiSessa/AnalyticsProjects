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
mushrooms <- read_excel("mushrooms.xlsx")

# structure of the data
str(mushrooms)

# number of rows with missing values
nrow(mushrooms) - sum(complete.cases(mushrooms))

# deleting redundant variable `veil.type`
mushrooms$`veil-type` <- NULL

#changing variables to factors
mushrooms$class <- as.factor(mushrooms$class)
mushrooms$`cap-shape` <- as.factor(mushrooms$`cap-shape`)
mushrooms$`cap-surface` <- as.factor(mushrooms$`cap-surface`)
mushrooms$`cap-color` <- as.factor(mushrooms$`cap-color`)
mushrooms$bruises <- as.factor(mushrooms$bruises)
mushrooms$odor <- as.factor(mushrooms$odor)
mushrooms$`gill-attachment` <- as.factor(mushrooms$`gill-attachment`)
mushrooms$`gill-spacing` <- as.factor(mushrooms$`gill-spacing`)
mushrooms$`gill-size` <- as.factor(mushrooms$`gill-size`)
mushrooms$`gill-color` <- as.factor(mushrooms$`gill-color`)
mushrooms$`stalk-shape` <- as.factor(mushrooms$`stalk-shape`)
mushrooms$`stalk-root` <- as.factor(mushrooms$`stalk-root`)
mushrooms$`stalk-surface-above-ring` <- as.factor(mushrooms$`stalk-surface-above-ring`)
mushrooms$`stalk-surface-below-ring` <- as.factor(mushrooms$`stalk-surface-below-ring`)
mushrooms$`stalk-color-above-ring` <- as.factor(mushrooms$`stalk-color-above-ring`)
mushrooms$`stalk-color-below-ring` <- as.factor(mushrooms$`stalk-color-below-ring`)
mushrooms$`veil-type` <- as.factor(mushrooms$`veil-type`)
mushrooms$`veil-color` <- as.factor(mushrooms$`veil-color`)
mushrooms$`ring-number` <- as.factor(mushrooms$`ring-number`)
mushrooms$`ring-type` <- as.factor(mushrooms$`ring-type`)
mushrooms$`spore-print-color` <- as.factor(mushrooms$`spore-print-color`)
mushrooms$population <- as.factor(mushrooms$population)
mushrooms$habitat <- as.factor(mushrooms$habitat)

str(mushrooms)

#analyzing the odor variable
table(mushrooms$class,mushrooms$odor)

number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of Perfect Splits by Feature",
        xlab="",ylab="Feature",las=2,col="light blue")

#data splicing
set.seed(12345)
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]

# penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)

# building the classification tree with rpart
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")

# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)

# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]

# tree pruning using the best complexity parameter
tree <- prune(tree, cp=cp.optim)

#Testing the model
pred <- predict(object=tree,mushrooms_test[-1],type="class")

#Calculating accuracy
t <- table(mushrooms_test$class,pred) 
confusionMatrix(t) 





