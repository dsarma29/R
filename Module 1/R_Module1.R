#Debjani Sarma
#Preprocessing:1
rm(list = ls())
install.packages("rio")
install.packages("moments")
library(rio)
library(moments)
getwd()

#Load the file “6304 Module 1 Assignment Data.xlsx” into R
crime.info=import("6304 Module 1 Assignment Data.xlsx")

#2.	Using the numerical portion of your U number as a random number seed, take a random sample of 30 counties using the method presented in class.
set.seed(24173877)
my.crime= crime.info[sample(1:nrow(crime.info),30),]

colnames(my.crime)=tolower(make.names(colnames(my.crime)))


#Analysis
#1
str(my.crime)
attach(my.crime)
#2
mean(population)
median(population)
sd(population)
skewness(population)
kurtosis(population)

#3
boxplot(total.crimes)

mean(total.crimes)
median(total.crimes)

boxplot(total.crimes,col="red",pch=19,ylim=c(0,50000),main="Boxplot of Total Crimes",ylab="Total Crimes")

#4
quantile(aggravated.assault,probs = seq(0,1,.2))

#5
hist(larceny,col="green",main="Histogram of Larceny",xlim = c(0,80000))


#6

boxplot(robbery,aggravated.assault,burglary,col="red",main="Comparative boxplots of Robbery,Aggravated Assault,Burglary",names=c("Robbery","Aggravated Assault","Burglary"),pch=19)

#7

#max(total.crimes)
county.max.totalcrime = subset(my.crime,total.crimes==max(total.crimes))
county.max.totalcrime$county

my.crime[which.max(total.crimes),1]

