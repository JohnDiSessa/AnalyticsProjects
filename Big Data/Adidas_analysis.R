
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
library(corrplot)
library(Hmisc)
library(stringr)

options(scipen = 999)

#clear environment
rm(list=ls())

#load file
adidas <- read.csv("adidas.csv", 
                  header = TRUE)
adidas

#subbrands
unique(adidas$Brand)

#create new subbrand column
#remove all "adidas " strings
adidas$subbrand <- gsub("Adidas ","", adidas$Brand)
adidas$subbrand

#check subbrand
unique(adidas$subbrand)

#subrands as factor
adidas$subbrand <- as.factor(adidas$subbrand)
str(adidas)

#average sales of each brand
price_average <- by(adidas$Sale.Price,adidas$subbrand,mean)
price_average

#average sales of each brand as proper $$
price_in_money <- price_average/100
round(price_in_money,2)

#barplot of subbrand frequency
subbrandbar <- barplot(summary(adidas$subbrand),
                main = "Sub Brand Frequency",
                xlab = "Sub Brand",
                ylab = "Frequency",
                col = "lightblue",
                ylim = c(0,1200))
text(x = subbrandbar, y = summary(adidas$subbrand) -100,labels = summary(adidas$subbrand))

#average rating of each brand
review_average_all <- by(adidas$Rating[adidas$Reviews>0],adidas$subbrand[adidas$Reviews>0],mean)
round(review_average_all,2)

#exclude ratings with less than 11 reviews
review_average <- by(adidas$Rating[adidas$Reviews>10],adidas$subbrand[adidas$Reviews>10],mean)
round(review_average,2)