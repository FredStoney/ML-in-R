setwd("C:/Users/fpsto/Documents/School/Fall_2019/Stat_ML")
data <- read.csv("wcgs.csv",header = T)

library(MASS)
library(klaR)
library(ROCR)

set.seed(199)
index_sample <- sample(1:nrow(data), floor(nrow(data)/2) )
train <- data[index_sample,]
test <- data[-index_sample,]
data$chd69 <- as.factor(data$chd69)
lda_model <- lda(chd69 ~ age + sbp + ncigs, data = train)
summary(lda_model)



parti <- partimat(chd69 ~ age + sbp + ncigs,
         nplots.vert=1, nplots.hor=3, data = train, method="lda")

lda_predictions <- predict(lda_model, test)


preds<-lda_predictions$posterior[,2]
rates<-prediction(preds, test$chd69)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Curve for Iris Data")
lines(x = c(0,1), y = c(0,1), col="red")
auc<-performance(rates, measure = "auc")
auc

table(preds > .1, test$chd69)


parti_qda <- partimat(chd69 ~ age + sbp + ncigs,
                      nplots.vert=1, nplots.hor=3, data = train, method="qda")
qda_model <- qda ( chd69 ~ age + sbp + ncigs, data = train)
qda_predictions <- predict(qda_model, test)
preds_qda <-qda_predictions$posterior[,2]

table(preds_qda > .1, test$chd69)



