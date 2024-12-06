#Debjani Sarma
rm(list = ls())

#Preprocessing
hospital.data=rio::import("6304 Module 6 Assignment Data.xlsx")

set.seed(24173877)
my.data=hospital.data[sample(1:nrow(hospital.data),150),]
my.data$smoker = as.factor(my.data$smoker)

#Analysis
str(my.data)
attach(my.data)
my.regmodel = lm(charges ~ age + bmi + smoker, data = my.data)
summary(my.regmodel)

confint(my.regmodel)


#Assumptions of LINE
#Linearity:
plot(charges, my.regmodel$fitted.values, pch=19, main = "Linearity Plot", 
     xlab="Charges", ylab="Fitted values")
abline(0,1,lwd=3, col="red")

#Independence:
plot(scale(my.regmodel$residuals),pch=19, main = "Independence Plot", ylab="Residuals")
abline(0,0, lwd=3, col="red")

#Normality:
qqnorm(my.regmodel$residuals, pch=19, main ="Normality plot")
qqline(my.regmodel$residuals, lwd=3, col="red")

hist(my.regmodel$residuals, main="Histogram of residuals", col="pink", probability = TRUE, 
     xlab = "Residuals")
curve(dnorm(x,mean(my.regmodel$residuals),sd(my.regmodel$residuals)),
      from=-20000, to = max(my.regmodel$residuals), lwd=3, col="blue",add=TRUE)
moments::skewness(my.regmodel$residuals)
moments::kurtosis(my.regmodel$residuals)

#Equality of variances:
plot(my.regmodel$fitted.values, rstandard(my.regmodel), pch=19, main="Equality of variances plot",
     xlab="Fitted values", ylab="Scaled residuals", ylim=c(-4,4))
abline(0,0,lwd=3, col="red")

#Determining the leverage points
leverages = hat(model.matrix(my.regmodel))
plot(leverages, pch = 19)
abline(3 * mean(leverages), 0, col="red", lwd=3)
my.data[leverages>(3*mean(leverages)),]
