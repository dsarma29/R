rm(list=ls())
library(car) #Companion to Applied regression
#preprocessing
#1
commonwealth=rio::import("6304 Module 7 Assignment Data.xlsx")
colnames(commonwealth) =  tolower(make.names(colnames(commonwealth)))

#2
commonwealth$index = 1:nrow(commonwealth)
attach(commonwealth)
#Analysis
#1
plot(index,australia.visitors, pch=19, xlab = "Index",
     ylab = "Number of Visitors")
#2 
commonwealth.lm = lm(australia.visitors ~ index, data = commonwealth)
summary(commonwealth.lm)

#3

plot(index,australia.visitors, pch=19 , xlab = "Index",
     ylab = "Number of Visitors", main="Time series data with simple regression")
abline(commonwealth.lm,col="red",lwd=3)

#4 Durbin-Watson test
durbin.out=durbinWatsonTest(commonwealth.lm)
durbin.out

#5 
#Making Seasonal Indices

indices=data.frame(quarter=1:4,average=0,index=0)
for(i in 1:4) {
  count=0
  for(j in 1:nrow(commonwealth)) { #loops over all row which contains visitors
    if(i==commonwealth$quarter[j]) {
      indices$average[i]=indices$average[i]+australia.visitors[j]
      count=count+1
    }
  }
  indices$average[i]=indices$average[i]/count
  indices$index[i]=indices$average[i]/mean(australia.visitors)
}

#Deseasonalizing the original data

for(i in 1:4){
  for(j in 1:nrow(commonwealth)){
    if(i==commonwealth$quarter[j]){
      commonwealth$deseason.visitors[j]=australia.visitors[j]/indices$index[i]
    }
  }
}


#6
#Conducting the deseasonalized regression

desreg.out=lm(commonwealth$deseason.visitors ~ index,data=commonwealth)
summary(desreg.out)

index2 = index^2
desreg.out2=lm(commonwealth$deseason.visitors ~ index + index2,data=commonwealth)
summary(desreg.out2)


index3 = index^3
desreg.out3=lm(commonwealth$deseason.visitors ~ index + index2 + index3,data=commonwealth)
summary(desreg.out3)


index4 = index^4
desreg.out4=lm(commonwealth$deseason.visitors ~ index + index2 + index3 + index4,data=commonwealth)
summary(desreg.out4)


#7
#Reasonalizing fitted values
deseason.fitted1=desreg.out$fitted.values
commonwealth$reseason.fitted = 0
for(i in 1:4) { 
  for(j in 1:nrow(commonwealth)) {
    if(i == commonwealth$quarter[j]) {
      commonwealth$reseason.fitted1[j]=deseason.fitted1[j] * indices$index[i]
    }
  }
}

deseason.fitted2=desreg.out2$fitted.values
commonwealth$reseason.fitted2 = 0
for(i in 1:4) {
  for(j in 1:nrow(commonwealth)) {
    if(i == commonwealth$quarter[j]) {
      commonwealth$reseason.fitted2[j]=deseason.fitted2[j] * indices$index[i]
    }
  }
}

deseason.fitted3=desreg.out3$fitted.values
commonwealth$reseason.fitted3 = 0
for(i in 1:4) {
  for(j in 1:nrow(commonwealth)) {
    if(i == commonwealth$quarter[j]) {
      commonwealth$reseason.fitted3[j]=deseason.fitted3[j] * indices$index[i]
    }
  }
}

deseason.fitted4=desreg.out4$fitted.values
commonwealth$reseason.fitted4 = 0
for(i in 1:4) {
  for(j in 1:nrow(commonwealth)) {
    if(i == commonwealth$quarter[j]) {
      commonwealth$reseason.fitted4[j]=deseason.fitted4[j] * indices$index[i]
    }
  }
}


##Plotting reseasonalized graph with original one

par(mfrow=c(1,1))

plot(index, australia.visitors, type="o",pch=19,
     main="Original Regression Model 1",
     xlab="Index", ylab="No. of Visitors")

lines(index, commonwealth$reseason.fitted, col="yellow", lwd=3)
lines(index, commonwealth$reseason.fitted2, col="green", lwd=3)
lines(index, commonwealth$reseason.fitted3, col="darkgreen", lwd=3)
lines(index, commonwealth$reseason.fitted4, col="purple", lwd=3)


#8
correlation1=cor(commonwealth$deseason.visitors, deseason.fitted1)
correlation1
correlation2=cor(commonwealth$deseason.visitors, deseason.fitted2)
correlation2
correlation3=cor(commonwealth$deseason.visitors, deseason.fitted3)
correlation3
correlation4=cor(commonwealth$deseason.visitors, deseason.fitted4)
correlation4

#Comparison of Reseanlized and Deseasonalized
par(mfrow=c(2,2))

plot(index, australia.visitors, type="o",pch=19,
     main="Original vs Fitted, Regression Model 1",
     xlab="Index", ylab="No. of Visitors")

lines(index, commonwealth$reseason.fitted, col="yellow", lwd=3)

plot(index, australia.visitors, type="o",pch=19,
     main="Original vs Fitted, Regression Model 2",
     xlab="Index", ylab="Number of Visitors")

lines(index, commonwealth$reseason.fitted2, col="red", lwd=3)

plot(index, australia.visitors, type="o",pch=19,
     main="Original vs Fitted, Regression Model 3",
     xlab="Index", ylab="Number of Visitors")
lines(index, commonwealth$reseason.fitted3, col="green", lwd=3)

plot(index, australia.visitors, type="o",pch=19,
     main="Original vs Fitted, Regression Model 4",
     xlab="Index", ylab="Number of Visitors")
lines(index, commonwealth$reseason.fitted4, col="purple", lwd=3)