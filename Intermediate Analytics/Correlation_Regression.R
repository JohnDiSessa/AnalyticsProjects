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

options(scipen = 999)

# Exploratory Data Analysis #
ames1 <- read.csv("AmesHousing1.csv", 
                  header = TRUE, sep = ",")
ames1
summary(ames1)
str(ames1)

ames1df <- data.frame(ames1)
ames1df
m <- mean(ames1df$SalePrice)
m
mean(ames1df$Year.Built)
median(ames1df$Yr.Sold)
mean(ames1df$Yr.Sold)
cvsale <- sd(ames1$SalePrice)/m
cvsale
mean(ames1df$Year.Built)

ggplot(ames1df,aes(x=Year.Built))+ 
  geom_histogram()+
  geom_vline(xintercept = 1971,linetype="dashed",color="red",size=2)+
  geom_vline(xintercept = 1973,linetype="solid",color="blue",size=1.2)+
  xlab("Year Built") + ylab("Count")+
  theme_bw()

ggplot(ames1df,aes(y=SalePrice,x=Year.Built))+ 
  geom_point()+
  geom_hline(yintercept = m,linetype="dashed",color="red",size=2)+
  xlab("Year Built") + ylab("Sale Price ($)")+
  theme_bw()

ggplot(ames1df,aes(x=Yr.Sold))+ 
  geom_histogram()+
  geom_vline(xintercept = 2007.79,linetype="dashed",color="red",size=2)+
  geom_vline(xintercept = 2008,linetype="solid",color="blue",size=1.2)+
  xlab("Year Sold") + ylab("Count")+
  theme_bw()

# Correlation Plots #
amescor <- cor(ames1df,use = "complete.obs")
amescor <- (round(amescor,2))
amescor

corplotames <- corrplot(amescor, method = "number", type = "lower",
                        col=brewer.pal(n=8, name="RdBu"),
                        tl.col= "black")
corplotamesp <- rcorr(as.matrix(ames1df))
corplotamesp

# Scatter Plot SP and Liv Room Area #

#checked p values (.0000)#
ggplot(ames1df,aes(y=SalePrice,x=Gr.Liv.Area))+ 
  geom_point()+
  xlab("Living Room Area (Sq.Ft.)") + ylab("Sale Price ($)")+ 
  ggtitle("Living Room vs Sales Price")+
  geom_smooth(method=lm,color="red")+
  annotate("text",x=500,y=550000,label="r = .75",col="red")+
  theme_bw()

ggplot(ames1df,aes(y=SalePrice,x=Lot.Frontage))+ 
  geom_point()+
  xlab("Lot Frontage") + ylab("Sale Price ($)")+ 
  ggtitle("Lot Area vs Sales Price")+
  geom_smooth(method=lm,color="red")+
  annotate("text",x=250,y=550000,label="r = .37",col="red")+
  theme_bw()

ggplot(ames1df,aes(y=SalePrice,x=Mas.Vnr.Area))+ 
  geom_point()+
  xlab("Vaneer Area (Sq.Ft.)") + ylab("Sale Price ($)")+ 
  ggtitle("Vaneer Area vs Sales Price")+
  geom_smooth(method=lm,color="red")+
  annotate("text",x=100,y=600000,label="r = .46",col="red")+
  theme_bw()

# Multiple Regression Model #



# Plot Regression Model #
avPlots(mlrames)
plot(mlrames)

# Multicollinearity #
vif(mlrames)
mlrames2 <- lm(SalePrice ~ Overall.Qual + Garage.Area + 
                Total.Bsmt.SF + X1st.Flr.SF + Lot.Area + TotRms.AbvGrd + 
                Year.Built + Fireplaces, data = ames1df)
mlrames2
summary(mlrames2)
vif(mlrames2)
plot(mlrames2)

errate <- (sigma(mlrames2)/mean(ames1df$SalePrice))
errate
# Outliers #
# Already removed before analysis due to a few unique houses skewing values
#not representative of the housing market

# Subset Regression #
submodels <- regsubsets(SalePrice~., data = ames1df,nvmax=6)
summary(submodels)
res.sum <- summary(submodels)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic))

#8 model was the highest amount of factors that all 3 indicators agreed on
#but 8 and 7 included garage year built plus too many factors to make it presentable
# 6 was the highest amount of factors with garage year built
mlrames3 <- lm(SalePrice ~ Overall.Qual + Garage.Area + Total.Bsmt.SF + 
                 Lot.Area + Gr.Liv.Area + Mas.Vnr.Area, data = ames1df)
mlrames3
summary(mlrames3)
vif(mlrames3)
plot(mlrames3)
#this has a higher adj r squared than my original one had#