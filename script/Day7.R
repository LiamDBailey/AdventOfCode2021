# DAY 7
library(dplyr)

#Load data
day7_data <- scan(file = here::here("data/Day7.txt"), sep = ",")

possible_positions <- range(day7_data)

## PUZZLE 1

fuel <- NULL
for (i in possible_positions[1]:possible_positions[2]) {

  fuel <- append(fuel, sum(abs(day7_data - i)))

}

#Try doing it using an optimization algorithm
optim_func1 <- function(i){

  sum(abs(day7_data - as.integer(i)))

}

optim(par = 0, fn = optim_func1, lower = 0, upper = possible_positions[2], method = "Brent")

## PUZZLE 2

#Sum of values in a sequence
sum_seq <- function(start, end, n){

  (n*(start + end))/2

}

fuel2 <- NULL
for (i in possible_positions[1]:possible_positions[2]) {

  diffs <- abs(day7_data - i)

  fuel2 <- append(fuel2, sum(sum_seq(start = 1, end = diffs, n = diffs)))

}

min(fuel2)

#Try doing it using an optimization algorithm
optim_func2 <- function(i){

  diffs <- abs(day7_data - as.integer(i))

  sum(sum_seq(start = 1, end = diffs, n = diffs))

}

answer2 <- optim(par = 0, fn = optim_func2, lower = 0, upper = possible_positions[2], method = "Brent")

