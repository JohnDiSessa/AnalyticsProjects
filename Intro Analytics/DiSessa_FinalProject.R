##### John DiSessa

#Import Libraries#
library(FSA)
library(FSAdata)
library(magrittr)
library(plyr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(anytime)
library(RColorBrewer)

#Import Data#

read.csv("Video_Games_sales_as_at_22_Dec_2016.csv", 
               header = TRUE, sep = ",")
vg <- read.csv("Video_Games_sales_as_at_22_Dec_2016.csv", 
         header = TRUE, sep = ",")
vg
consolesales <- read.csv("salesbyconsole3.csv", 
               header = TRUE, sep = ",")
consolesales
salesbypub <- read.csv("salesbypub.csv", 
                   header = TRUE, sep = ",")
salesbypub
sales = read_csv("salesbyear.csv")
salesconsole = read_csv("salesbyconsole2.csv")
avgsalesgenre = read_csv("avgsalesbygenre.csv")

#Data Clean-Up#

nrow(vg)
str(vg)
summary(vg)

vg$User_Score <- as.numeric(as.character(vg$User_Score))

vg$Total_Sales <- vg$NA_Sales + vg$EU_Sales +vg$JP_Sales + vg$Other_Sales
mutate(vg,vg$Total_Sales)

vg$Cum_Score <- ((vg$User_Score*10) + vg$Critic_Score)/2
mutate(vg,vg$Cum_Score)
vg

mean(vg$Total_Sales)
median(vg$Total_Sales)
mean(vg$Cum_Score, na.rm=T)
median(vg$Cum_Score, na.rm=T)

#Summary#

countsc <- count(vg, vars = Platform)
countsc
countscd <- arrange(countsc,desc(n))
countscd
countsg <- count(vg, vars = Genre)
countsg
countsp <- count(vg, vars = Publisher)
countsp
countspd <- arrange(countsp,desc(n))
countspd
countsy <- count(vg, vars = Year_of_Release)
countsy

cols <- c("red4","red3","plum", "rosybrown1", "lightblue","lightgoldenrod",
          "yellow2","turquoise","springgreen2","grey49","grey88","skyblue3")
percentlabels <- round(100*countsg$n/sum(countsg$n),1)
pielabels <- paste(percentlabels, "%", sep="")
pie(countsg$n, labels = pielabels, main = "Video Game Genres", 
    col = cols, cex=.8, radius = .6)
legend("right", countsg$vars, cex=.6,fill = cols)

plot(countsy$vars, countsy$n, main = "Video Games Releases by Year",
     xlab = "Year", ylab = "Number of Releases", 
     lwd = 4, type = "l", col = "red3")

barplot(countscd$n, names = countscd$vars, main = "Video Games per Console",
        xlab = "Consoles", ylab = "Number of Releases", cex.names = 1,
        col=brewer.pal(n = 24, name = "Reds"), las = 2)

genre <- c("Action","Adventure","Fighting","Misc","Platform","Puzzle","Racing",
           "Role-Playing","Shooter","Simulation","Sports","Strategy")
salesg <- c(1716.5,233.13,441.24,790.2,824.01,239.87,723.42,929.77,1041.34,
            387.94,1309.65,172.23)
avgsalesg <- c(923.4182,388.8286,537.0305295,550.3027352,1845.416808,898.780747,
               804.7528565,765.7325484,1088.037741,451.3239949,
               834.6294806,205.6726957)

salesgenre <- data.frame(genre,salesg,avgsalesg)
salesgenre <- salesgenre[order(salesg,decreasing = T),]
salesgenre

barplot(salesgenre$salesg, names = salesgenre$genre, main = "Sales by Genre",
         ylab = "Sales in Millions", las = 2, cex.names = .8,
        col=brewer.pal(n=12, name = "Reds"))

barplot(salesgenre$avgsalesg, names = salesgenre$genre, 
        main = "Average Sales per Game by Genre",
        ylab = "Sales in 100,000s", las = 2, cex.names = .8,
        col=brewer.pal(n=12, name = "Reds"))

#Analysis#

#video games releases by console over time

sales %>%
  ggplot(aes(x = Year, y = Sales, color = Region)) +
  geom_line(size = 1.2) +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())+
  labs(title = "Sales by Region", y = "Sales in Millions")

#Score / Sales correlation

plot(vg$Cum_Score, vg$Total_Sales,
     main = "Game Review to Sales", xlab = "Average Review Score",
     ylab = "Total Sales", col = "red3",
     abline(lm(vg$Total_Sales~vg$Cum_Score)), lwd = .5)
correlation <- cor(vg$Cum_Score,vg$Total_Sales, use = "complete.obs")
correlation
rsq <- correlation^2
rsq

#video game sales by console
colors <- c("aquamarine","azure4","bisque2","black","blue","blueviolet","brown",
            "burlywood4","cadetblue","grey69","chocolate","chocolate4",
            "green","darksalmon","darkslateblue","darkslategray","deeppink",
            "darkgreen","gold","lightpink1","peru","red","red4","seagreen1")
salesconsole %>%
  ggplot(aes(x = Year_of_Release, y = Total_Sales, color = Platform)) +
  geom_line(size = .6) +
  labs(title = "Sales by Console", x = "Year", y = "Sales in Millions") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  scale_color_manual(values = colors)

avgsalesgenre %>%
  ggplot(aes(x = Year_of_Release, y = Total_Sales, color = Genre)) +
  geom_line(size = .6) +
  labs(title = "Sales by Genre", x = "Year", y = "Sales") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

avgsalesgenre %>%
  ggplot(aes(x = Year_of_Release, y = Avg_Sales, color = Genre)) +
  geom_line(size = .6) +
  labs(title = "Avg Sales per Release", x = "Year", y = "Sales in Millions") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

#Sales by Platform#
barplot(consolesales$Sales, names = consolesales$ï..Platform, 
        main = "Sales by Console",
        ylab = "Sales in Millions", las = 2, cex.names = .8,
        col=brewer.pal(n=18, name = "Reds"))

barplot(consolesales$Average.Sales.per.Release, 
        names = consolesales$ï..Platform, 
        main = "Average Sales per Game by Console",
        ylab = "Sales in 100,000s", las = 2, cex.names = .8,
        col=brewer.pal(n=18, name = "Reds"))

#Sales by Publisher#
barplot(salesbypub$Total_Sales, names = salesbypub$ï..Publisher, 
        main = "Sales by Publisher",
        ylab = "Sales in Millions", las = 2, cex.names = .7,
        col=brewer.pal(n=11, name = "Reds"))

barplot(salesbypub$Avg_Sales, 
        names = salesbypub$ï..Publisher , 
        main = "Average Sales per Game by Publisher",
        ylab = "Sales in 100,000s", las = 2, cex.names = .7,
        col=brewer.pal(n=11, name = "Reds"))