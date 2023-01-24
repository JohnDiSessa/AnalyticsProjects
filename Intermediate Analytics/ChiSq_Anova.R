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

options(scipen = 999)

#Section 11-1#

#Blood Types#
bloodtype <- c("A","B","O","AB")
expected <- c(.2,.28,.36,.16)
observed <- c(12,8,24,6)
bt <- data.frame(bloodtype,expected)
bt
chisq.test(observed,p=expected)

#Airlines#
airexpected <- c(.708,.082,.09,.12)
airobserved <- c(125,10,25,40)
chisq.test(airobserved,p=airexpected)

#Section 11.2#

#Ethnicity & Movies#
race <- c("Caucasian","Hispanic","AA","Other")
cauc <- c(724,370)
hisp <- c(335,292)
AA <- c(174,152)
Other <- c(107,140)
eth <- data.frame(cauc,hisp,AA,Other)
eth
chisq.test(eth)

#Women in military#
officers <- c(10791,7816,932,11819)
enlisted <- c(62491,42750,9525,54344)
wim <- data.frame(officers,enlisted)
wim
chisq.test(wim)

#Section 12-1#

#sodium#
cond <- c(270,130,230,180,80,70,200)
cer <- c(260,220,290,290,200,320,140)
dess <- c(100,180,250,250,300,360,300,160)
condiments <- data.frame("sodium"=cond,"type"=rep("condiments",7))
cereal <- data.frame("sodium"=cer,"type"=rep("cereal",7))
desserts <- data.frame("sodium"=dess,"type"=rep("desserts",8))
sodium <- rbind(condiments,cereal,desserts)
sodium
sodiumvar <- aov(sodium~type,data = sodium)
sodiumvar
summary(sodiumvar)
ScheffeTest(sodiumvar)

#Section 12-2#

#sales#
cersales <- c(578,320,264,249,237)
chocsales <- c(311,106,109,125,173)
cofsales <- c(261,185,302,689)
cerealsales <- data.frame("Sales"=cersales,"Company"=rep("Cereal",5))
chocolatesales <- data.frame("Sales"=chocsales,"Company"=rep("Chocolate",5))
coffeesales <- data.frame("Sales"=cofsales,"Company"=rep("Coffee",4))
sales <- rbind(cerealsales,chocolatesales,coffeesales)
sales
salesvar <- aov(Sales~Company,data=sales)
salesvar
summary(salesvar)
ScheffeTest(salesvar)

#expenditures#
east <- c(4946,5953,6202,7243,6113)
mid <- c(6149,7451,6000,6479)
west <- c(5282,8605,6528,6911)
eastthird <- data.frame("Exp"=east,"Region"=rep("East Third",5))
midthird <- data.frame("Exp"=mid,"Region"=rep("Middle Third",4))
westthird <- data.frame("Exp"=west,"Region"=rep("West Third",4))
expenditures <- rbind(eastthird,midthird,westthird)
expenditures
expvar <- aov(Exp~Region,data=expenditures)
summary(expvar)

#Section 12-3#
fal1 <- c(9.2,9.4,8.9)
fal2 <- c(8.5,9.2,8.9)
fbl1 <- c(7.1,7.2,8.5)
fbl2 <- c(5.5,5.8,7.6)
x1 <- data.frame("Growth"=fal1,"Light"=rep("L1",3),"Food"=rep("FA",3))
x2 <- data.frame("Growth"=fal2,"Light"=rep("L2",3),"Food"=rep("FA",3))
x3 <- data.frame("Growth"=fbl1,"Light"=rep("L1",3),"Food"=rep("FB",3))
x4 <- data.frame("Growth"=fbl2,"Light"=rep("L2",3),"Food"=rep("FB",3))
plants <- rbind(x1,x2,x3,x4)
plants
plantsvar <- aov(Growth~Light+Food+Light:Food,data=plants)
summary(plantsvar)
interaction.plot(plants$Light,plants$Food,plants$Growth,
                 xlab="Light",ylab = "Mean Growth",main="Light vs Food Interaction",
                 trace.label = "Food")

#Baseball#

baseball <- read.csv("baseball.csv", 
                  header = TRUE, sep = ",")
baseball

#EDA#
summary(baseball)
baseball$Decade <- as.factor(baseball$Decade)
str(baseball)
baseball2 <- read.csv("baseballcordata.csv", 
                     header = TRUE, sep = ",")
baseball2
str(baseball2)
baseball2df <- data.frame(baseball2)
baseball2df

baseballcor <- cor(baseball2df,use = "complete.obs")
baseballcor <- (round(baseballcor,2))
baseballcor

corplotbaseball <- corrplot(baseballcor, method = "number", type = "lower",
                        col=brewer.pal(n=8, name="RdBu"),
                        tl.col= "black")
hist(baseball$W,xlab = "Wins",ylab = "Frequency",main = "Wins Histogram")
boxplot(baseball$W~baseball$League, xlab = "League",ylab = "Wins",main="Wins by League")

mean(baseball2$W)

count(baseball,vars=Decade)
wintable <- aggregate(baseball$W,by=list(Decade=baseball$Decade),FUN=sum)
wintable
expectedwin <- rep(1/6,6)
expectedwin
chisq.test(wintable$x,p=expectedwin)

#Crops#
crops <- read.csv("crop_data.csv", 
                     header = TRUE, sep = ",")
crops
str(crops)
crops$density <- as.factor(crops$density)
crops$block <- as.factor(crops$block)
crops$fertilizer <- as.factor(crops$fertilizer)
str(crops)
summary(crops)

cropsaov <- aov(yield~fertilizer+density+fertilizer:density,data=crops)
cropsaov
summary(cropsaov)

TukeyHSD(cropsaov)

interaction.plot(crops$fertilizer,crops$density,crops$yield, 
                 main = "Fertilizer vs Density Interaction", xlab = "Fertilizer",
                 ylab = "Yield",trace.label = "Density")
