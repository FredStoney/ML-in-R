dataset <- swiss
library(glmnet)

x<-model.matrix(Fertility ~ .,dataset)[,-1] 
y<-dataset$Fertility


set.seed(1888)
train_perc <- .5
row_ind <- sample(1:nrow(dataset), floor(train_perc * nrow(dataset)))
x.train = x[row_ind,]
x.test = x[-row_ind,]
y.train = y[row_ind]
y.test = y[-row_ind]

# check threshold is low enough by observing the coefficients for 
# ridge regression with lambda = 0 are equal to OLS regression
ridge.r<-glmnet(x,y, alpha=0, lambda=0, thresh = 1e-14)
coefficients(ridge.r)
coefficients(lm(data = dataset, formula = Fertility ~ .))

# ridge regression
set.seed(2019) 
cv.out.ridge <-cv.glmnet(x.train, y.train,alpha=0)
plot(cv.out.ridge, main = "ridge")
# find the best lambda for ridge regression
bestlam<-cv.out.ridge$lambda.min
bestlam


grid<-10^seq(10,-2,length=100)
ridge.mod<-glmnet(x.train,y.train,alpha=0,lambda=grid, thresh = 1e-14)

# mean squared error
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x.test)
mean((ridge.pred-y.test)^2)
# [1] 50.9831 


# 4 (f) # lasso regression

set.seed(2019) 
cv.out.lasso<-cv.glmnet(x.train, y.train,alpha=1)
plot(cv.out.lasso, main = "lasso")
# find best lambda for lasso regression
bestlam<-cv.out$lambda.min
bestlam
# [1] 0.168852

grid<-10^seq(10,-2,length=100)
lasso.mod<-glmnet(x.train,y.train,alpha=1,lambda=grid, thresh = 1e-14)


lasso.pred<-predict(lasso.mod,s=bestlam,newx=x.test)
mean((lasso.pred-y.test)^2)
# 1] 71.9308

# mean squared error of ols
train <- dataset[row_ind,]
test <- dataset[row_ind,]
ols <- lm(data = train, formula = Fertility ~ .)
ols.predict <- predict(ols, newdata = test)
mean((ols.predict - test$Fertility) ^ 2)
# [1] 44.42285

