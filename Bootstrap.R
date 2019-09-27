# Using bootstrapping to estimate the error of an estimator

library(MASS)

dataset = iris
# Example
# Estimator: for population median of Petal.width for setosa species
# Goal : estimate the  standard error of the estimator


setosa_subset <- dataset[dataset$Species == "setosa", ]
sample_original <- setosa_subset$Petal.Width

estimate <- median(sample_original)
estimate

# Begin bootstrap by taking samples WITH replacement from our original sample
# the size of our samples must be the same as our original sample size.
sample_size = length(sample_original)

samples <- replicate(1000, sample(sample_original, sample_size, replace = T))
medians <- apply(samples, MARGIN = 2, FUN = median)