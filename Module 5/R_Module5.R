#pre-processing
rm(list=ls())
library("rio")
library("moments")
getwd()

#1
master.data=import("6304 Module 5 Assignment Data.xlsx")

#2
nashville.data = master.data[master.data$property.city == "NASHVILLE", ]
nashville.data$bedrooms=as.factor(nashville.data$bedrooms)
nashville.data$full.bath=as.factor(nashville.data$full.bath)
set.seed(24173877)
my.nashdata=nashville.data[sample(1:nrow(nashville.data),4000),]

#Analysis 
#1
str(my.nashdata)
attach(my.nashdata)

#2
multi.reg = lm(sale.price ~ land.value + building.value + 
                 total.value + finished.area + bedrooms + full.bath, 
               data = my.nashdata)

#2a
summary(multi.reg)
par(mfrow=c(2,2))
#Linearity
plot(my.nashdata$sale.price, multi.reg$fitted.values,
     xlab = "Sale Price", ylab="Fitted values",
 pch=19,main ="Actual vs Fitted Values, Linearity")
abline(0,1,col="red",lwd=3)

#Independence
plot(scale(multi.reg$residuals), pch=19, main="Independence plot",
     ylab="Scaled residuals")
abline(0,0, col="red", lwd=3)

#Normality
qqnorm(multi.reg$residuals, pch=19, main = "Normality Plot")
qqline(multi.reg$residuals, col="red", lwd=3)
residuals = multi.reg$residuals
#histogram
hist(multi.reg$residuals, col="red", main = "Histogram of Residuals", 
     probability = TRUE, ylim = c(0, 250e-08),
     xlim = c(-2e+06, 2e+06), xlab= "Residuals")
max.res=max(multi.reg$residuals)
curve(dnorm(x,mean(multi.reg$residuals),sd(multi.reg$residuals)),
      from=c(-2e+06, 2e+06),
      to=max.res,lwd=3,col="blue", add=TRUE)

skewness(multi.reg$residuals)
kurtosis(multi.reg$residuals)

#Equality of variances
plot(sale.price,scale(multi.reg$residuals), 
     pch=19, main="Equality of variances plot", 
     xlab = "Sale Price", ylab="Scaled Residuals")
abline(0,0,col="red",lwd=3)

mean(my.nashdata$sale.price)
