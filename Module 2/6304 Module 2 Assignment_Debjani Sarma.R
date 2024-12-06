#Debjani Sarma
rm(list=ls())
library(rio)
library(moments)
getwd()


#Preprocessing
marxbrothers=import("6304 Module 2 Assignment Data.xlsx")
colnames(marxbrothers)=tolower(make.names(colnames(marxbrothers)))
attach(marxbrothers)

#Analysis
#Skewness
skewness(harpo)
skewness(zeppo)
skewness(groucho)
skewness(chico)

kurtosis(harpo)
kurtosis(zeppo)
kurtosis(groucho)
kurtosis(chico)

hist(harpo,col="pink",main="Histogram of harpo")
hist(zeppo,col="pink",main="Histogram of zeppo")
hist(groucho,col="pink",main="Histogram of groucho")
hist(chico,col="pink",main="Histogram of chico")

plot(density(harpo),lwd=3,col="red",xlab="harpo",main="Density plot of harpo")
plot(density(zeppo),lwd=3,col="red",xlab="zeppo",main="Density plot of zeppo")
plot(density(groucho),lwd=3,col="red",xlab="groucho",main="Density plot of groucho")
plot(density(chico),lwd=3,col="red",xlab="chico",main="Density plot of chico")

qqnorm(harpo,pch=19,main="QQ plot of harpo")
qqline(harpo,col="red",lwd=3)

qqnorm(zeppo,pch=19,main="QQ plot of zeppo")
qqline(zeppo,col="red",lwd=3)

qqnorm(groucho,pch=19,main="QQ plot of groucho")
qqline(groucho,col="red",lwd=3)

qqnorm(chico,pch=19,main="QQ plot of chico")
qqline(chico,col="red",lwd=3)


mean.zeppo=mean(zeppo)
mean.zeppo
sd.zeppo=sd(zeppo)
sd.zeppo
hist(zeppo,col="pink",main="My zeppo vs Theoretical zeppo",probability=TRUE)
curve(dnorm(x,mean.zeppo, sd.zeppo), from=min(zeppo), to = max(zeppo), col="red",lwd=3,add=TRUE)
plot(density(zeppo),lwd=3,col="blue",xlab="zeppo",main="My Density zeppo vs Theoretical")
curve(dnorm(x,mean.zeppo, sd.zeppo), from=min(zeppo), to = max(zeppo), col="red",lwd=3,add=TRUE)


#Chico
sample_mean=c()
for (i in 1:1000) {
  chico.sample=marxbrothers[sample(1:nrow(marxbrothers),40),]
  mean.chico=mean(chico.sample$chico)
  sample_mean=c(sample_mean,mean.chico)
}
sample_mean

sample.mean= mean(sample_mean)
sd.mean = sd(sample_mean)
curve(dnorm(x,sample.mean, sd.mean), from=min(sample_mean), to = max(sample_mean), col="red",lwd=3,add=TRUE)

plot(density(sample_mean),main="Plot of sample mean of chico",lwd=3,col="blue")
hist(sample_mean,col="yellow",main="Histogram of sample mean of chico")
skewness(sample_mean)
kurtosis(sample_mean)
qqnorm(sample_mean,main="QQPlot of sample mean of chico",pch=19)
qqline(sample_mean,lwd=3,col="red")

sample.mean= mean(sample_mean)
sample.mean
sd.mean = sd(sample_mean)
sd.mean

plot(density(sample_mean),main="My Plot vs Theoritical curve",lwd=3,col="blue")
curve(dnorm(x,sample.mean, sd.mean), from=min(sample_mean), to = max(sample_mean), col="red",lwd=3,add=TRUE)
hist(sample_mean,col="pink",main="My sample histogram vs Theoretical",probability=TRUE, ylim=c(0,9))
curve(dnorm(x,sample.mean, sd.mean), from=min(sample_mean), to = max(sample_mean), col="red",lwd=3,add=TRUE)
