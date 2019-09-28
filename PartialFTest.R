library(MASS)
dataset = iris


# full model without predictors
# 
full_model <- lm(data = dataset, formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species)
summary(full_model)
# partial model without Petal.Width
partial_model <- lm(data = dataset, formula = Sepal.Length ~ Sepal.Width + Petal.Length + Species)

# carry out anova test
# Null Hypothesis is that the coefficient of Petal.Width is equal to zero.
# Alternative Hypothesis is that the coefficient of Petal.Width is not equal to zero
anova(partial_model,full_model)

# The F test is significant at significance level of .05, so we reject the null hypothesis that the coeficient is equal to zero, in favor of the full model.