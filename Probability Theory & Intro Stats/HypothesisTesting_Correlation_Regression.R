#Module 1#

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
library(sjPlot)
library(RColorBrewer)
library(ggpubr)
library(MASS)
library(forcats)
library(corrplot)
library(Hmisc)
library(formattable)


rae <- read.csv("RAExperiment.csv", 
         header = TRUE, sep = ",")
rae

str(rae)

rae2 <- subset(rae,select = -c(Gender,TreatmentA))
rae2
str(rae2)
smk <- as.factor(rae2$Smoke)
asb <- as.factor(rae2$Asbestos)

rae3 <- subset(rae2,select = -c(Smoke,Asbestos))
rae3
rae4 <- cbind(rae3,Smoke = smk)
rae5 <- cbind(rae4,Asbestos = asb)
rae5
str(rae5)
summary(rae5)

#Analysis#

table1 <- tab_xtab(var.row = rae5$Smoke, var.col = rae5$Asbestos, 
                   show.row.prc = T)
table1

plot1 <- plot(rae5$Size,rae5$Days, xlab = "Tumor Size (cm)", 
              ylab = "Days in Hospital", main = "Tumor Size vs Hospital Stay",
              pch = 10, col = "orange", abline(lm(rae5$Days~rae5$Size)))
plot2 <- plot(rae5$Age,rae5$Days, xlab = "Age", 
              ylab = "Days in Hospital", main = "Age vs Hospital Stay",
              pch = 10, col = "orange", abline(lm(rae5$Days~rae5$Age)))
plot3 <- plot(rae5$Age,rae5$Size, xlab = "Age", 
              ylab = "Tumor Size (cm)", main = "Age vs Tumor Size",
              pch = 10, col = "orange", abline(lm(rae5$Size~rae5$Age)))

hist1 <- hist(rae5$Age, breaks = 18, xlab = "Age",
              main = "Patient Age Frequency",
              col=brewer.pal(n=18, name = "Oranges"))
hist2 <- hist(rae5$Size, breaks = 10, xlab = "Tumor Size (cm)",
              main = "Tumor Size Frequency",
              col=brewer.pal(n=18, name = "Oranges"))
hist3 <- hist(rae5$Days, breaks = 20, xlab = "Days",
              main = "Hospital Stay Duration Frequency",
              col=brewer.pal(n=18, name = "Oranges"))

cor1 <- cor(rae5$Days,rae5$Size, use = "complete.obs")
cor1
rsq1 <- cor1^2
rsq1
cor2 <- cor(rae5$Days,rae5$Age, use = "complete.obs")
cor2
rsq2 <- cor2^2
rsq2
cor3 <- cor(rae5$Size,rae5$Age, use = "complete.obs")
cor3
rsq3 <- cor3^2
rsq3

#Module 2#

sds <- sd(rae5$Size)
sdd <- sd(rae5$Days)
sda <- sd(rae5$Age)
sds
sdd
sda

library(data.table)
library(formattable)

tab1 <- read.csv("table1.csv", 
                header = TRUE, sep = ",")
tab1
names(tab1)[1] <- " "
tab1f <- formattable(tab1)
tab1f

agelabs <- c("41-50","51-60","61-70","71-80")
agelabs
rae6 <- read.csv("RAExperiment2.csv", 
                 header = TRUE, sep = ",")
rae6
sd(rae6$Days[rae6$AgeGroup=="41-50"])
sd(rae6$Days[rae6$AgeGroup=="51-60"])
sd(rae6$Days[rae6$AgeGroup=="61-70"])
sd(rae6$Days[rae6$AgeGroup=="71-80"])

tab2 <- read.csv("table2.csv", 
                 header = TRUE, sep = ",")
tab2
names(tab2)[1] <- "Days in Hosp by Age"
tab2f <- formattable(tab2)
tab2f

sd(rae6$Size[rae6$AgeGroup=="41-50"])
sd(rae6$Size[rae6$AgeGroup=="51-60"])
sd(rae6$Size[rae6$AgeGroup=="61-70"])
sd(rae6$Size[rae6$AgeGroup=="71-80"])

tab3 <- read.csv("table3.csv", 
                 header = TRUE, sep = ",")
names(tab3)[1] <- "Tumor Size by Age"
tab3f <- formattable(tab3)
tab3f


# Basic stripchart
ggplot(rae6, aes(x=AgeGroup, y=Days)) + 
  geom_jitter()
# Change the position
# 0.2 : degree of jitter in x direction

jit1 <- ggplot(rae6, aes(x=AgeGroup, y=Days, color = AgeGroup)) + 
  geom_jitter(position=position_jitter(0.2)) +
  stat_summary(fun.y = mean,geom="point",shape = 17, size = 2,color = "black") +
  theme_bw() +
  labs(title="Days in Hospital")
jit1

jit2 <- ggplot(rae6, aes(x=AgeGroup, y=Size, color = AgeGroup)) + 
  geom_jitter(position=position_jitter(0.2)) +
  stat_summary(fun.y = mean,geom="point",shape = 17, size = 2,color = "black") +
  theme_bw() +
  labs(title="Tumor Size")
jit2

box1 <- ggplot(rae6, aes(x=AgeGroup, y=Days, color = AgeGroup)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8)+
  theme_bw()+
  labs(title="Days in Hospital")
box1

box2 <- ggplot(rae6, aes(x=AgeGroup, y=Size, color = AgeGroup)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8) +
  theme_bw() +
  labs(title="Tumor Size")
box2

#Module 3#

options(scipen = 999)

t.test(rae6$Size, alternative = "greater",mu=5, conf.level = .95)

t.test(rae6$Size[rae6$AgeGroup=="41-50"],rae6$Size[rae6$AgeGroup=="51-60"],
       alternative = "less",var.equal = F)
t.test(rae6$Size[rae6$AgeGroup=="41-50"],rae6$Size[rae6$AgeGroup=="61-70"],
       alternative = "less",var.equal = F)
t.test(rae6$Size[rae6$AgeGroup=="41-50"],rae6$Size[rae6$AgeGroup=="71-80"],
       alternative = "less",var.equal = F)
t.test(rae6$Size[rae6$AgeGroup=="51-60"],rae6$Size[rae6$AgeGroup=="61-70"],
       alternative = "less",var.equal = F)
t.test(rae6$Size[rae6$AgeGroup=="51-60"],rae6$Size[rae6$AgeGroup=="71-80"],
       alternative = "less",var.equal = F)
t.test(rae6$Size[rae6$AgeGroup=="61-70"],rae6$Size[rae6$AgeGroup=="71-80"],
       alternative = "less",var.equal = F)

t.test(rae6$Days[rae6$AgeGroup=="41-50"],rae6$Days[rae6$AgeGroup=="51-60"],
       alternative = "less",var.equal = F)
t.test(rae6$Days[rae6$AgeGroup=="41-50"],rae6$Days[rae6$AgeGroup=="61-70"],
       alternative = "less",var.equal = F)
t.test(rae6$Days[rae6$AgeGroup=="41-50"],rae6$Days[rae6$AgeGroup=="71-80"],
       alternative = "less",var.equal = F)
t.test(rae6$Days[rae6$AgeGroup=="51-60"],rae6$Days[rae6$AgeGroup=="61-70"],
       alternative = "less",var.equal = F)
t.test(rae6$Days[rae6$AgeGroup=="51-60"],rae6$Days[rae6$AgeGroup=="71-80"],
       alternative = "less",var.equal = F)
t.test(rae6$Days[rae6$AgeGroup=="61-70"],rae6$Days[rae6$AgeGroup=="71-80"],
       alternative = "less",var.equal = F)

tab4 <- read.csv("table4.csv", 
                 header = TRUE, sep = ",")
names(tab4)[1] <- "Age Groups (X vs Y)"
names(tab4)[2] <- "Mean Tumor Size (X)"
names(tab4)[3] <- "Mean Tumor Size (Y)"
names(tab4)[4] <- "P Value"
names(tab4)[5] <- "T Stat"
tab4f <- formattable(tab4)
tab4f
tab5 <- read.csv("table5.csv", 
                 header = TRUE, sep = ",")
names(tab5)[1] <- "Age Groups (X vs Y)"
names(tab5)[2] <- "Mean Days in Hosp (X)"
names(tab5)[3] <- "Mean Days in Hosp (Y)"
names(tab5)[4] <- "P Value"
names(tab5)[5] <- "T Stat"
tab5f <- formattable(tab5)
tab5f

#Module 4#
cats
male <- subset(cats,subset=cats$Sex=="M")
female <- subset(cats,subset=cats$Sex=="F")

mavgbwt <- mean(male$Bwt)
mavghwt <- mean(male$Hwt)
mavgbwt
mavghwt
favgbwt <- mean(female$Bwt)
favghwt <- mean(female$Hwt)
favgbwt
favghwt

tab6 <- read.csv("table6.csv", 
                 header = TRUE, sep = ",")
names(tab6)[1] <- "Summary"
tab6f <- formattable(tab6)
tab6f

box4 <- ggplot(cats, aes(x=Sex, y=Bwt, color = Sex)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8)+
  theme_bw()+
  labs(title="Bodyweight by Gender",y="Bodyweight")
box4

box6 <- ggplot(cats, aes(x=Sex, y=Hwt, color = Sex)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8)+
  theme_bw()+
  labs(title="Height by Gender",y="Height")
box6

median(male$Bwt)
median(male$Hwt)
median(female$Bwt)
median(female$Hwt)

t.test(male$Bwt,female$Bwt,alternative="two.sided",var.equal = F)
t.test(male$Bwt,female$Bwt,alternative="greater",var.equal = F)
t.test(male$Hwt,female$Hwt,alternative="two.sided",var.equal = F)
t.test(male$Hwt,female$Hwt,alternative="greater",var.equal = F)

#Sleep#
sleepbef <- c(4.6,7.8,9.1,5.6,6.9,8.5,5.3,7.1,3.2,4.4)
sleepaft <- c(6.6,7.7,9.0,6.2,7.8,8.3,5.9,6.5,5.8,4.9)
id <- c(1,2,3,4,5,6,7,8,9,10)
befaft <- c("Before","Before","Before","Before","Before","Before","Before",
            "Before","Before","Before","After","After","After","After","After",
            "After","After","After","After","After")
scores <- c(4.6,7.8,9.1,5.6,6.9,8.5,5.3,7.1,3.2,4.4,6.6,7.7,9.0,6.2,7.8,8.3,
            5.9,6.5,5.8,4.9)
sleep <- data.frame(id,scores,befaft)
sleep
headtail(sleep)

tab7 <- read.csv("table7.csv", 
                 header = TRUE, sep = ",")
names(tab7)[1] <- "Summary"
tab7f <- formattable(tab7)
tab7f

box5 <- ggplot(sleep, aes(x=fct_reorder(befaft,scores), 
                          y=scores, color = befaft)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8)+
  theme_bw()+
  labs(title="Sleep Scores",x=" ",y="Scores")+
  theme(legend.title = element_blank())
box5

t.test(sleepbef,sleepaft,paired = T,alternative="two.sided",var.equal = F)
t.test(sleepbef,sleepaft,paired = T,alternative="less",var.equal = F)

ggplot(sleep,aes(x=id,y=scores,fill=befaft))+
  geom_bar(position="dodge",stat = "identity")+
  theme_bw()+
  labs(title="Sleep Scores",x="Participant",y="Scores")+
  theme(legend.title = element_blank())

#Module 5#

mtcars2 <- subset(mtcars,select = c(mpg,disp,hp,drat,wt))
mtcars2
summary(mtcars2)
sd(mtcars2$mpg)
sd(mtcars2$disp)
sd(mtcars2$hp)
sd(mtcars2$drat)
sd(mtcars2$wt)

tab8 <- read.csv("table8.csv", 
                 header = TRUE, sep = ",")
names(tab8)[1] <- "Summary"
tab8f <- formattable(tab8)
tab8f


carscor <- cor(mtcars2)
carscor <- (round(carscor,2))
carscor
corrplot(carscor, method = "circle")
corplotcars <- corrplot(carscor, method = "number", type = "lower",
                        col=brewer.pal(n=8, name="RdBu"),
                        tl.col= "black")

corplotcarsp <- rcorr(as.matrix(mtcars2))
corplotcarsp

mlrcars <- lm(mpg ~ disp+hp+drat+wt, data = mtcars2)
mlrcars
summary(mlrcars)
errate <- (sigma(mlrcars)/mean(mtcars2$mpg))
errate #residual standard error#

mlrcars2 <- lm(mpg ~ hp+wt, data = mtcars2)
mlrcars2
summary(mlrcars2)

mtcars2
#remove categorical variables, only numeric,removed qsec n
#model looked good everywhere except individual pvalues - multicolinearity#
#try to remove predictor variables that are highly correlated to each other#

#Module 6#
super <- read.csv("supermarket_sales.csv", 
                header = TRUE, sep = ",")
super$Branch <- as.factor(super$Branch)
super
str(super)

mean(super$Rating[super$Branch=="A"])
mean(super$Rating[super$Branch=="B"])
mean(super$Rating[super$Branch=="C"])
max(super$Rating[super$Branch=="A"])
max(super$Rating[super$Branch=="B"])
max(super$Rating[super$Branch=="C"])
min(super$Rating[super$Branch=="A"])
min(super$Rating[super$Branch=="B"])
min(super$Rating[super$Branch=="C"])
sd(super$Rating[super$Branch=="A"])
sd(super$Rating[super$Branch=="B"])
sd(super$Rating[super$Branch=="C"])
tab9 <- read.csv("table9.csv", 
                 header = TRUE, sep = ",")
names(tab9)[1] <- "Branch"
tab9f <- formattable(tab9)
tab9f

gg1 <- ggplot(super,aes(y=Total,x=Quantity))+
  geom_jitter(position=position_jitter(0.5))+
  stat_summary(fun.y = mean,geom="point",shape = 17, size = 2,color = "black")+
  theme_bw()+
  geom_smooth(method="lm")+
  labs(y="Total Sales",x="Quantity",title="Total Sales by Quantity")
gg1

gg2 <- ggplot(super,aes(y=Total,x=Quantity,color=Branch))+
  geom_jitter(position=position_jitter(0.5))+
  stat_summary(fun.y = mean,geom="point",shape = 17, size = 2,color = "black")+
  theme_bw()+
  geom_smooth(method="lm")+
  labs(y="Total Sales",x="Quantity",title="Total Sales by Quantity")
gg2

cor(super$Quantity,super$Total)

reg <- lm(Total~Quantity,data = super)
summary(reg)

box7 <- ggplot(super, aes(x=Branch, y=Total, color = Branch)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8)+
  theme_bw()+
  labs(title="Sales by Branch")
box7

box8 <- ggplot(super, aes(x=Gender, y=Total, color = Gender)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8)+
  theme_bw()+
  labs(title="Sales by Gender")
box8

a <- subset(super,subset=super$Branch=="A")
b <- subset(super,subset=super$Branch=="B")
c <- subset(super,subset=super$Branch=="C")
t.test(a$Total,b$Total,alternative="two.sided",var.equal = F)
t.test(b$Total,c$Total,alternative="two.sided",var.equal = F)
t.test(a$Total,c$Total,alternative="two.sided",var.equal = F)
cor(a$Quantity,a$Total)
cor(b$Quantity,b$Total)
cor(c$Quantity,c$Total)
gg3 <- ggplot(a,aes(y=Total,x=Quantity,color=Branch))+
  geom_jitter(position=position_jitter(0.5))+
  stat_summary(fun.y = mean,geom="point",shape = 17, size = 2,color = "black")+
  theme_bw()+
  geom_smooth(method="lm")+
  labs(y="Total Sales",x="Item Quantity",title="Total Sales by Quantity")
gg3
gg4 <- ggplot(b,aes(y=Total,x=Quantity,color=Branch))+
  geom_jitter(position=position_jitter(0.5))+
  scale_color_manual(values=c("forestgreen"))+
  stat_summary(fun.y = mean,geom="point",shape = 17, size = 2,color = "black")+
  theme_bw()+
  geom_smooth(method="lm")+
  labs(y="Total Sales",x="Item Quantity",title="Total Sales by Quantity")
gg4
gg5 <- ggplot(c,aes(y=Total,x=Quantity,color=Branch))+
  geom_jitter(position=position_jitter(0.5))+
  scale_color_manual(values=c("dodgerblue2"))+
  stat_summary(fun.y = mean,geom="point",shape = 17, size = 2,color = "black")+
  theme_bw()+
  geom_smooth(method="lm")+
  labs(y="Total Sales",x="Item Quantity",title="Total Sales by Quantity")
gg5

