#John DiSessa Final Project#

#Milestone 1#

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

options(scipen = 999)

fifa1 <- read.csv("fifa19_players.csv", 
                 header = TRUE, sep = ",")
fifa1$Preferred.Foot <- as.factor(fifa1$Preferred.Foot)
fifa1

str(fifa1)
summary(fifa1)


ggplot(fifa1,aes(x=Value,y=Wage))+
  geom_point(color="darkblue") +
  geom_smooth(method=lm, se=FALSE, color="black")+
  ggtitle("Wage vs Value")+ xlab("Value")+ ylab("Weekly Wage")+
  theme_bw()

cor1 <- cor(fifa1$Wage,fifa1$Value,use="complete.obs")
cor1
rsq1 <- cor1^2
rsq1

plot(fifa1$Age,fifa1$Growth)

tab11 <- read.csv("table11.csv", 
                 header = TRUE, sep = ",")
names(tab11)[1] <- "Desc Stats"
tab11f <- formattable(tab11)
tab11f

tab12 <- read.csv("table12.csv", 
                  header = TRUE, sep = ",")
names(tab12)[1] <- "L vs R"
tab12f <- formattable(tab12)
tab12f

ggplot(fifa1, aes(x=Overall))+
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept = mean(Overall)),linetype = "dashed") +
  ylab("Count") + ggtitle("Overall Rating Histogram")+
  theme_bw()

mean(fifa1$Overall)
median(fifa1$Overall)

ggplot(fifa1, aes(x=Age))+
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept = mean(Age)),linetype = "dashed") +
  ylab("Count") + ggtitle("Age Histogram") +
  theme_bw()

mean(fifa1$Age)
median(fifa1$Age)

ggplot(fifa1, aes(x=Weak.Foot))+
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept = median(Weak.Foot)),linetype = "dashed") +
  ylab("Count") + ggtitle("Weak Foot Ratings")+
  theme_bw()+
  xlab("Weak Foot")

jit13 <- ggplot(fifa1, aes(x=Weak.Foot, y=Value, color = Weak.Foot)) + 
  geom_jitter(position=position_jitter(0.5)) +
  theme_bw() +
  labs(title="Value by Weak Foot")+xlab("Weak Foot")+
  geom_smooth(method=lm, se=FALSE, color="black")
jit13

cor3 <- cor(fifa1$Weak.Foot,fifa1$Value,use="complete.obs")
cor3
rsq3 <- cor3^2
rsq3

ggplot(fifa1, aes(x=Skill.Moves))+
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept = median(Skill.Moves)),linetype = "dashed") +
  ylab("Count") + ggtitle("Skill Moves Ratings")

jit11 <- ggplot(fifa1, aes(x=Age, y=Value, color = Age)) + 
  geom_jitter(position=position_jitter(0.2)) +
  theme_bw() +
  labs(title="Value by Age")
jit11
cor2 <- cor(fifa1$Age,fifa1$Value,use="complete.obs")
cor2
rsq2 <- cor2^2
rsq2

mean(fifa1$Value,na.rm=T)
median(fifa1$Value,na.rm=T)

box1 <- ggplot(fifa1, aes(x=Preferred.Foot, y=Value, color = Preferred.Foot)) + 
  geom_boxplot(outlier.color = "black",outlier.shape = 8) +
  theme_bw() +
  labs(title="Player Value")
box1

mean(fifa1$Wage,na.rm=T)
median(fifa1$Wage,na.rm=T)

tab13 <- read.csv("table13.csv", 
                  header = TRUE, sep = ",")
names(tab13)[1] <- "Weak Foot"
tab13f <- formattable(tab13)
tab13f

mean(fifa1$Weak.Foot,na.rm=T)
median(fifa1$Weak.Foot,na.rm=T)

#Milestone 2#

lefty <- subset(fifa1,subset=fifa1$Preferred.Foot=="Left")
righty <- subset(fifa1,subset=fifa1$Preferred.Foot=="Right")
mean(lefty$Value,na.rm=T)
mean(righty$Value,na.rm=T)

t.test(lefty$Value,righty$Value,alternative="two.sided",var.equal = F)
t.test(lefty$Value,righty$Value,alternative="greater",var.equal = F)

fivewf <- subset(fifa1,subset=fifa1$Weak.Foot==5)
fourwf <- subset(fifa1,subset=fifa1$Weak.Foot==4)
threewf <- subset(fifa1,subset=fifa1$Weak.Foot==3)
twowf <- subset(fifa1,subset=fifa1$Weak.Foot==2)
onewf <- subset(fifa1,subset=fifa1$Weak.Foot==1)

t.test(fivewf$Value,fourwf$Value,alternative = "greater", var.equal = F)
t.test(fourwf$Value,threewf$Value,alternative = "greater", var.equal = F)
t.test(threewf$Value,twowf$Value,alternative = "greater", var.equal = F)
t.test(twowf$Value,onewf$Value,alternative = "greater", var.equal = F)

tab14 <- read.csv("table14.csv", 
                  header = TRUE, sep = ",")
names(tab14)[1] <- "u1 vs u2"
tab14f <- formattable(tab14)
tab14f

epl <- read.csv("eplvalues.csv",
                header = TRUE, sep = ",")
mean(epl$Value)
epl
t.test(epl$Value,mu = 18930832,alternative = "less")

#Milestone 3#
fifa2 <- read.csv("fifa19_players2.csv", 
                  header = TRUE, sep = ",")
fifa2$Preferred.Foot <- as.factor(fifa2$Preferred.Foot)
fifa2

fifa2 <- subset(fifa2,select = c(Overall,Potential,Growth,Value,Wage,
                                 Weak.Foot))
fifa2

cor(fifa2$Wage,fifa2$Overall,use="complete.obs") #.58

fifcor <- cor(fifa2,use = "complete.obs")
fifcor <- (round(fifcor,2))
fifcor

corplotfifa <- corrplot(fifcor, method = "number", type = "lower",
                        col=brewer.pal(n=8, name="RdBu"),
                        tl.col= "black")
corplotfifap <- rcorr(as.matrix(fifa2))
corplotfifap

mlrfifa <- lm(Value ~ Overall+Wage+Potential+Weak.Foot, data = fifa2)
mlrfifa
summary(mlrfifa)
errate <- (sigma(mlrfifa)/mean(fifa2$Value))
errate

model <- lm(Value ~ Pref.Foot,data=fifa2)
summary(model)

newlr <- data.frame(Pref.Foot = c(1,0))
newlr
predict(model,newdata = newlr,interval = "confidence")

model2 <- lm(Overall~Wage,data=fifa2)
summary(model2)