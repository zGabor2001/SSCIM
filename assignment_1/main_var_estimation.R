library("microbenchmark")
library("knitr")


one_pass <- function(data, n) {
  n <- length(data)
  data_sum <- sum(data)
  return((sum(data^2) - data_sum^2 / n) / n)
}


two_pass <- function(data, n) {
  dataset_mean <- mean(data)
  deviate_var <- sum((data - dataset_mean)^2)
  return(deviate_var / (n - 1))
}


shifted_one_pass <- function(data, n) {
  shift_value <- data[1]
  shifted_data <- sum(data - shift_value)
  return((sum((data - shift_value)^2) - shifted_data / n) / n) 
}


online <- function(data) {
  mean_value <- 0
  m2 <- 0
  n <- 0
  for (x in data) {
    n <- n + 1
    delta_x <- x - mean_value
    mean_value <- mean_value + delta_x / n
    second_delta_x <- x - mean_value
    m2 <- m2 + delta_x * second_delta_x
  }
  return(m2 / n)
}


var_calc_wrapper <- function(data, var_method) {
  n <- length(data)
  switch(var_method,
    "one_pass_var" = one_pass(data, n),
    "two_pass_var" = two_pass(data, n),
    "shifted_one_pass_var" = shifted_one_pass(data, n),
    "online_alg_var" = online(data),
    "default_var" = var(data)
 )
}


set.seed(12427550)
# simulated dataset
x <- rnorm(1000, mean = 5, sd = 2)

results <- data.frame(
  Method = c("One-Pass", "Two-Pass", "Shifted One-Pass", "Online", "Default var"),
  Variance = c(var_calc_wrapper(x, "one_pass_var"),
               var_calc_wrapper(x, "two_pass_var"),
               var_calc_wrapper(x, "shifted_one_pass_var"),
               var_calc_wrapper(x, "online_alg_var"),
               var_calc_wrapper(x, "default_var")
               )
)

kable(results, caption = "Variance Estimates by Method")


# microbenchmark
benchmark_results <- microbenchmark(
  one_pass = var_calc_wrapper(x, "one_pass_var"),
  two_pass = var_calc_wrapper(x, "two_pass_var"),
  shifted_one_pass = var_calc_wrapper(x, "shifted_one_pass_var"),
  online = var_calc_wrapper(x, "online_alg_var"),
  r_var = var_calc_wrapper(x, "default_var"),
  times = 1000
)

kable(summary(benchmark_results), caption = "Performance Benchmarking (in microseconds)")


#scale invariance
scaled_x <- x * 10

scaled_results <- data.frame(
  Method = c("One-Pass", "Two-Pass", "Shifted One-Pass", "Online", "R's var"),
  Scaled_Variance = c(var_calc_wrapper(scaled_x, "one_pass_var"),
                      var_calc_wrapper(scaled_x, "two_pass_var"),
                      var_calc_wrapper(scaled_x, "shifted_one_pass_var"),
                      var_calc_wrapper(scaled_x, "online_alg_var"),
                      var_calc_wrapper(scaled_x, "default_var"))
)

kable(scaled_results, caption = "Variance Estimates for Scaled Data")


# condition numbers
library(pracma)

x1 <- rnorm(1000, mean = 0, sd = 1)
x2 <- rnorm(1000, mean = 100, sd = 1)
x3 <- c(rep(1, 999), 10000)

cond_num <- function(x) {
  sd(x) / abs(mean(x))
}

condition_numbers <- data.frame(
  Dataset = c("Centered Data", "Shifted Data", "Extreme Outlier Data"),
  Condition_Number = c(cond_num(x1), cond_num(x2), cond_num(x3))
)

kable(condition_numbers, caption = "Condition Numbers for Different Datasets")