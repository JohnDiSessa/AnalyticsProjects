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
library(BSDA)

options(scipen = 999)

#Question 6#
#sign test single sample#
attendance <- c(6210,3150,2700,3012,4875,3540,6127,2581,2642,2573,2792,
               2800,2500,3700,6030,5437,2758,3490,2851,2720)

qqnorm(attendance)
qqline(attendance) #on straight line means normal distribution#
text(-1,5000,"Game Attendance")

#H0: median = 3000#
#Ha: median notequal to 3000#

mediandiff <- attendance-3000
mediandiff

positive <- length(mediandiff[mediandiff>0])
negative <- length(mediandiff[mediandiff<0])
positive
negative
#n=20,alpha=.05, twotailed test so CV (from table) = 5
#test value = 10#

signtestss <- binom.test(x=c(positive,negative),alternative="two.sided")
signtestss
signtestss$statistic #test value#
signtestss$p.value #p-value#

#test value > cv AND p-value > alpha#
#fail to reject the null#


#Question 10#
#sign test single sample#

#alpha = .05#
#n=40#
#one tailed#
#median=200#
#H0: median = 200#
#Ha: median <200 (claim)#
#15 days less than 200, 25 days =>200#
#since n>25, use z formula to calculate test value = -1.64#
n <- 40
X <- 15

#Test Value#
z <- ((X+.5)-(.5*n))/((sqrt(n)/2))
z
#test value < critical value, fail to reject null#


#Question 4#
#wilcox rank sum#

males <- c(8,12,6,14,22,27,32,24,26,19,15,13)
females <- c(7,5,2,3,21,26,30,9,4,17,23,12,11,16)

qqnorm(males)
qqline(males)
text(-1,25,"Prison Sentences")

qqnorm(females)
qqline(females)

#H0: no difference in prison sentence length by gender (claim)#
#Ha: difference in prison length by gender#

#alpha = .05, two-tailed test, cv = 1.96 & -1.96#

wilctest <- wilcox.test(x=males,y=females,alternative="two.sided",correct = FALSE)
wilctest
#teststat = 113, pvalue =.13#
#fail to reject null#

#Question 8#
AL <- c(108,86,91,97,100,102,95,104,95,89,88,101)
NL <- c(89,96,88,101,90,91,92,96,108,100,95)

qqnorm(AL)
qqline(AL)
text(-1,102,"Winning Baseball Games")

qqnorm(NL)
qqline(NL)

#H0: no difference in wins by league#
#Ha: difference in wins by league (claim)#

#alpha = .05, two-tailed test, cv = 1.96 & -1.96#

wilctestbb <- wilcox.test(x=AL,y=NL,alternative="two.sided",correct = FALSE)
wilctestbb
#teststat = 73, pvalue =.67#
#fail to reject null#

#Question 2#
WestHem <- data.frame(LiteracyScore=c(527,406,474,381,411),Region=rep("Western Hemisphere",5))
Europe <- data.frame (LiteracyScore=c(520,510,513,548,496),Region=rep("Europe",5))
EastAsia <- data.frame(LiteracyScore=c(523,547,547,391,549),Region=rep("Eastern Asia",5))
table <- rbind(WestHem,Europe,EastAsia)
table

#H0: no difference in mean mathematics literacy scores among the 3 different regions
#Ha: difference in mean mathematics literacy scores among the 3 different regions (Claim)

#alpha = .05#
#df =Number of groups-1 = k-1 = 3-1 =2#
#Chi-Square table CV = 5.991

TestV <- kruskal.test(LiteracyScore ~ Region ,data=table)
TestV
#test value = 4.17#
#pvalue=.12#
#fail to reject null#


#question 6#
city <- c(1,2,3,4,5,6)
subway <- c(845,494,425,313,108,41)
rail <- c(39,291,142,103,33,38)
dataframe <- data.frame(City=city,Subway=subway,Rail=rail)
dataframe

#The most common null hypothesis is H0: ?? = 0 which indicates there is no linear relationship between x and y in the population#
#H0: p=0#
#Ha: p not equal to 0#

#n=6, alpha=.05, twosided#
#CV = .89 from rank correlation coefficient cv table#
TestV2 <- cor.test(dataframe$Subway,dataframe$Rail,method = "spearman")
TestV2

#testvalue=.6, pvalue=.24#
#fail to reject null#