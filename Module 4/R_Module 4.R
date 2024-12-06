#Debjani Sarma
rm(list=ls())
library("rio")
library("moments")
getwd()

#Preprocessing
my.cars = import("6304 Module 4 Assignment Data.xlsx")
set.seed(24173877)
my.sample=my.cars[sample(1:nrow(my.cars),30),]
attach(my.sample)
#Analysis
#1
str(my.sample)
#2
my.simpleLR = lm(price ~ mileage , data=my.sample)
#3
summary(my.simpleLR)
confint(my.simpleLR)
#4
#Mileage vs price:
plot(my.sample$mileage, my.sample$price, main="Mileage vs price", pch=19, 
     xlab="Mileage", ylab="Price", ylim=c(0,30000))
abline(my.simpleLR, col="red",lwd=3)
#linearity
plot(my.sample$price, my.simpleLR$fitted.values, pch=19, 
     xlab="Car Price", ylab="Fitted Value",
     xlim=c(0,25000),ylim=c(0,25000), 
     main="Linearity Plot")
abline(0,1,col="red",lwd=3)
#independence
plot(my.sample$price, scale(my.simpleLR$fitted.values), pch=19, 
     xlab="Car Price", ylab = "Scaled Fitted Values",
     main="Independence Plot")
abline(0,0,col="red",lwd=3)
#noramlity 
qqnorm(my.simpleLR$residuals, main="Normality Plot", pch=19)
qqline(my.simpleLR$residuals, col="red", lwd=3)
hist(my.simpleLR$residuals, col="pink",main="Histogram of residuals")
skewness(my.simpleLR$residuals)
kurtosis(my.simpleLR$residuals)
#equality of variances
plot(my.simpleLR$fitted.values, scale(my.simpleLR$residuals), pch=19, 
     xlab="Fitted values", ylab="Residuals", main="Equality of Variances",
     ylim=c(-4,4))
abline(0,0, col="red",lwd=3)

#5
attach(my.sample)
newdata=data.frame(mileage=25000)
predicted_price = predict(my.simpleLR, newdata, interval = "none")
predicted_price

#6
rolls.royce=data.frame(mileage=1275)
predicted.price1 = predict(my.simpleLR, rolls.royce, interval = "none")
predicted.price1

