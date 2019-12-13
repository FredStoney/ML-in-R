library(MASS)
dataset = Boston


# create a model
linear_model <- lm(data = dataset, age ~ .)

par(mfrow = c(1,1))
plot(linear_model)

# can improve the model assumptions with a boxcox transformation of the response variable
par(mfrow = c(1,1))
boxcox(linear_model)

new_linear_model <- lm(data = dataset, age^(1.5) ~ .)
boxcox(new_linear_model)
plot(new_linear_model)



