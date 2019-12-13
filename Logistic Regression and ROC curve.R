setwd("C:/Users/fpsto/Documents/School/Fall_2019/Stat_ML")
data <- read.csv("wcgs.csv", header = T)

library(ROCR)

# age. Age in years
#s bp. Systolic blood pressure in mm Hg
# dbp. Diastolic blood pressure in mm Hg
# ncigs. Number of cigarettes smoked per day, on average.
#The response variable is chd69,

boxplot(data$age, data$chd69)
boxplot(data$sbp, data$chd69)
boxplot(data$dbp, data$chd69)
boxplot(data$ncigs, data$chd69)

# all the variables seem to differentiate 

log_model <- glm(data = data, chd69 ~ dbp + age + sbp + ncigs, family = binomial)
summary(log_model)

#log(odds) = -9.119 + .0078 dbp .....


set.seed(111)
sample.data<-sample.int(nrow(data), floor(.50*nrow(data)), replace = F)
train<-data[sample.data, ]
test<-data[-sample.data, ]

result_train<-glm(chd69 ~ age + sbp + ncigs, family=binomial, data=train)

preds<-predict(result_train,newdata=test, type="response")
rates<-prediction(preds, test$chd69)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")
