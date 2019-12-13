library(ISLR)
library(tree)
library(dplyr)
library(randomForest)

data <- College

# subset predictors
dataset <- data
dataset$Private <- factor(dataset$Private)

# train test
set.seed(1)
train_perc = .5
row_ind <- sample(1:nrow(dataset), floor(train_perc * nrow(dataset)))
train = dataset[row_ind,]
test = dataset[-row_ind,]

tree.train <- tree(Private ~ . , data = train )
test.response <- test$Private

summary(tree.train)
plot(tree.train)
text(tree.train, cex=0.75, pretty=0)

pred.response <- predict(tree.train, newdata = test, type = "class")
table(test.response, pred.response )
accuracy <- mean(test.response == pred.response )
accuracy

set.seed(2)
cv.class <- cv.tree(tree.train, K=10, FUN=prune.misclass)
cv.class
# 4 is optimal size
trees.num.class = 4

prune.class<-prune.misclass(tree.train, best=trees.num.class)
prune.class
plot(prune.class)
text(prune.class, cex=0.75, pretty=0)

tree.prune.predict <- predict(prune.class, newdata = test, type = "class")
table(test.response, tree.prune.predict)
mean(test.response == tree.prune.predict)

set.seed(3)
baggin <- randomForest(Private ~ ., data = train, mtry = 17, importance = T )
baggin

importance(baggin)
