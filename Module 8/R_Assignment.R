rm(list = ls())
library(rio)
#Preprocessing
master.emp=import("6304 Module 8 Assignment Data.xlsx")
set.seed(24173877)
mydata = master.emp[sample(1:nrow(master.emp), 4500),]

#Analysis
#1
str(mydata)
#2
my.logreg = glm(over5000 ~ department.name + nummos, data = mydata, family = binomial)
summary(my.logreg)
#3
#The Null Model represents model with just intercept (no independent variables)
#Since the Residual Deviance is much lower than Null deviance, it indicates that the model with independent variables (department.name and nummos) explains better variation in outcome(over5000) than the Null model. 
#Yes, I believe the Residual Deviance of your model is markedly different from the Null Deviance.

#4
#The variable department.nameFire and nummos have positive coefficients of 1.05781 and 0.68509 respectively.
#These variables influence in increasing the modeled probability that an employee earned  $5000 or more. 
#Having nummos as positive coefficient suggests that each additional months employee receives overtime , the prob of earning over $5000 increases.
#Having department.nameFire as as positive coefficient suggests that employees in fire department have the prob of earning over $5000 increases compared to other departments.

#5
#department.namePolice, department.nameStreets and Sanitation, department.nameWater Management all have negative coefficients suggesting influence in decreasing the modeled probability that an employee earned  $5000 or more.
#However,department.nameStreets and Sanitation has a negative coefficient of -1.60539, which is the most negative of all variables.
#Therefore, department.nameStreets and Sanitation has the greatest influence in decreasing the modeled probability that an employee earned  $5000 or more compared to others.

#6
prediction = expand.grid(department.name = unique(mydata$department.name),nummos= unique(mydata$nummos))
prediction
prediction$pred_prob = round(predict(my.logreg, newdata = prediction, type = "response"), 4) 
head(prediction,10)

#7
prediction.max = prediction[which.max(prediction$pred_prob),]
prediction.max$department.name
prediction.max$nummos

prediction.min = prediction[which.min(prediction$pred_prob),]
prediction.min$department.name
prediction.min$nummos
