library(ISLR)
datset <- Wage

# calculate the acutal mean and standard deviations
sample_mean <- mean(datset$wage)
original_sd <- sd(datset$wage)/sqrt(nrow(datset))
# bounds for a 95% confidence interval
lower_bound <- sample_mean - 1.96 * sd_mean
upper_bound <- sample_mean + 1.96 * sd_mean

set.seed(115)
bootstrap_means <- replicate(10, mean(sample(datset$wage, nrow(datset), replace = T)))
bootstrap_sd_error <- sd(bootstrap_means)
# the bootstrap standard error and mean
bootstrap_sd_error
original_sd


# can improve estimates with more samples

bootstrap_means <- replicate(100, mean(sample(datset$wage, nrow(datset), replace = T)))
bootstrap_sd_error100 <- sd(bootstrap_means)

bootstrap_means <- replicate(1000, mean(sample(datset$wage, nrow(datset), replace = T)))
bootstrap_sd_error1000 <- sd(bootstrap_means)

bootstrap_means <- replicate(10000, mean(sample(datset$wage, nrow(datset), replace = T)))
bootstrap_sd_error10000 <- sd(bootstrap_means)

sd_mean
bootstrap_sd_error
bootstrap_sd_error100
bootstrap_sd_error1000
bootstrap_sd_error10000

# converges to the actual standard error
library(boot)
my.mean <- function(data, index){
  return(mean(data[index]))
}


my_boot <- boot(datset$wage, my.mean, R=10000)
boot.ci(my_boot, conf = .95, type = "basic")
