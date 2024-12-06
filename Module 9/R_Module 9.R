#Debjani Sarma.
rm(list = ls())
library(rio)
library(car)

#1
master.states = import("6304 Module 9 Assignment Data.xlsx")

set.seed(24173877)

#2a
mydata = master.states[, c("state", "popdensity", "density.category", "percollege", "inmetro")]

#2b
mydata.il = mydata[mydata$state == "IL", ]
mydata.il = mydata.il[sample(1:nrow(mydata.il),50), ]

mydata.in = mydata[mydata$state == "IN", ]
mydata.in = mydata.in[sample(1:nrow(mydata.il),50), ]

mydata.mi = mydata[mydata$state == "MI", ]
mydata.mi = mydata.mi[sample(1:nrow(mydata.il),50), ]

mydata.oh = mydata[mydata$state == "OH", ]
mydata.oh = mydata.oh[sample(1:nrow(mydata.il),50), ]

mydata.wi = mydata[mydata$state == "WI", ]
mydata.wi = mydata.wi[sample(1:nrow(mydata.il),50), ]

mydata = rbind(mydata.il, mydata.in, mydata.mi, mydata.oh, mydata.wi)

rm(mydata.il)
rm(mydata.in)
rm(mydata.mi)
rm(mydata.oh)
rm(mydata.wi)

#2c
mydata$state = as.factor(mydata$state)
mydata$density.category = as.factor(mydata$density.category)
mydata$inmetro = as.factor(mydata$inmetro)

str(mydata)
table(mydata$state)

#3
percollege.levenetest = leveneTest(percollege ~ state, data = mydata)
percollege.levenetest

aggregate(percollege ~ state, data = mydata, var)

boxplot(percollege ~ state, data = mydata,main="Percollege, Five States", col="red")

#4 One way ANOVA

anovatest = aov(percollege ~ state, data = mydata)
summary(anovatest)
anovatest$coefficients

anovatukey = TukeyHSD(anovatest)
anovatukey

plot(anovatukey)
par(mar = c(5.1,8,4.1,2.1))
plot(anovatukey, las = 2)
par(mar = c(5.1,4.1,4.1,2.1))

#5
#3
percollege.density.category = leveneTest(percollege ~ density.category, data = mydata)
percollege.density.category

aggregate(percollege ~ density.category, data = mydata, var)

boxplot(percollege ~ density.category, data = mydata,main="Percollege, Density Category", col="red", cex.axis=0.8)

#4 One way ANOVA

anovatest.density.category = aov(percollege ~ density.category, data = mydata)
summary(anovatest.density.category)
anovatest.density.category$coefficients

anovatukey.density.category = TukeyHSD(anovatest.density.category)
anovatukey.density.category

plot(anovatukey.density.category)
par(mar = c(5.1,8,4.1,2.1))
plot(anovatukey.density.category, las = 2, cex.axis=0.65)
par(mar = c(5.1,4.1,4.1,2.1))

#6 two way anova

anovatest.inmetro = aov(percollege ~ state + inmetro, data = mydata)
summary(anovatest.inmetro)

anovatukey.inmetro = TukeyHSD(anovatest.inmetro)
anovatukey.inmetro

par(mar = c(5.1,8,4.1,2.1))
plot(anovatukey.inmetro, las = 2, cex.axis=0.8)
par(mar = c(5.1,4.1,4.1,2.1))

percollege.inmetro = leveneTest(percollege ~ inmetro, data = mydata)
percollege.inmetro
percollege.state = leveneTest(percollege ~ state, data = mydata)
percollege.state

aggregate(percollege ~ inmetro, data = mydata, var)
aggregate(percollege ~ state, data = mydata, var)

par(mfrow = c(1,2))
boxplot(percollege ~ inmetro, data = mydata,main="Percollege, 2 Inmetro", col="red")
boxplot(percollege ~ state, data = mydata,main="Percollege, Five States", col="red", cex.axis = 0.8)
