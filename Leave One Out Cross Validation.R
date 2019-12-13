


xi <-  c(70, 75, 80, 80, 85, 90)
yi <- c(75, 82, 80, 86, 90, 91)

data <- as.data.frame(cbind(xi, yi))
# estimate the test mse
LOOCV <- function(data){
  sum_mse = 0
  for (index in 1:length(yi)){
    test <- data[index,]
    train <- data[-index,]
    model <- lm (data = train, formula = yi ~ xi)
    predicted <- predict.lm(model, test)
    mse <- (predicted - test[,2])^2
    sum_mse <- sum_mse + mse
    
  }
  return(sum_mse / length(yi))
}

sum_mse <- LOOCV(data)
sum_mse
# 9.054487 