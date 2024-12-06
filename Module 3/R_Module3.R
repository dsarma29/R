#Debjani Sarma
rm(list=ls())
library("rio")
library("moments")
getwd()

crime=import("6304 Module 3 Assignment Data.xlsx")
colnames(crime)=tolower(make.names(colnames(crime)))
attach(crime)

population75=quantile(population,0.75)
inter.population=subset(crime,population<population75)

set.seed(24173877)
my.county=inter.population[sample(1:nrow(inter.population),15),]

#Analysis
str(my.county)
attach(my.county)

ci.totalcrime99=t.test(total.crimes,conf.level = 0.99)
ci.totalcrime99


ci.total=ci.totalcrime99$conf.int
ci.total
mean(inter.population$total.crimes)

ci.totalcrime95=t.test(total.crimes, conf.level = 0.95)
width95=ci.totalcrime95$conf.int[2] - ci.totalcrime95$conf.int[1]
width99=ci.totalcrime99$conf.int[2] - ci.totalcrime99$conf.int[1]
difference = width99 - width95
difference

pop.mean=t.test(crime.rate.per.100k.popln, mu=1700, alternative = "less")
pop.mean

mean.pop=mean(crime.rate.per.100k.popln)
max.pop=max(crime.rate.per.100k.popln)
for(i in seq(from = mean.pop, to =max.pop))
{
  my.test=t.test(crime.rate.per.100k.popln, mu= i, alternative = "two.sided")
  if (my.test$p.value <= 0.05)
  {
    i=print(i)
    break
  }
}
t.test(crime.rate.per.100k.popln, mu= i, alternative = "two.sided")

for(i in seq(from = max.pop , to =mean.pop, by=-1))
{
  my.test=t.test(crime.rate.per.100k.popln, mu= i, alternative = "two.sided")
  if (my.test$p.value > 0.05)
  {
    i=print(i)
    break
  }
}

t.test(crime.rate.per.100k.popln, mu= 2046.4, alternative = "two.sided")
max.offence=which.max(my.county$clearance.rate.per.100.offenses)
my.county$county[max.offence]

mean.difference=t.test(aggravated.assault,burglary, paired = TRUE)
mean.difference
