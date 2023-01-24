install.packages("e1071")
install.packages("caret")
install.packages("ggplot2")
install.packages("GGally")
install.packages("caret")
library("e1071")
library("caret")
library("ggplot2")
library("GGally")
library("caret")

options(scipen = 999)

#use sepal length, sepal width, pedal length, pedal width to rpedict flower type#

data(iris)
str(iris)

svm_model <- svm(Species ~ ., data=iris,
                 kernel="radial")
summary(svm_model)

#EDA#
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))
#petal length and petal width do a great job of identifying setosa#


#SVM Plot#
plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4),
     colour = Species)

#confusion matrix#
x <- iris[,-5]
y <- iris[5]
pred <- predict(svm_model,x)
confusionMatrix(pred,y$Species)


#parameter tuning to improve the model# #darkest blue finds best balance of over and under fitting#
set.seed(123)
tmodel=tune(svm,Species~., data=iris,
            ranges=list(epsilon= seq(0,1,0.1), cost = 2^(2:7)))
plot(tmodel)
summary(tmodel)

#best model#
mymodel=tmodel$best.model
summary(mymodel)
mymodel

plot(mymodel, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) )
#optimal hyperplanes##maximize the margin##support vectors#


#confusion matrix#
pred1 <- predict(mymodel,x)
confusionMatrix(pred1,y$Species)
