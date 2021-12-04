# DAY 3
library(dplyr)

#Load data
day3_data <- readr::read_delim(file = here::here("data/Day3.txt"), delim = "/t",
                               col_names = "binary", show_col_types = FALSE)

binary_length <- nchar(day3_data$binary[1])

## PUZZLE 1

#Separate number into columns
separated_binary <- day3_data %>%
  #We use regex lookbehind to only separate the col when there is a number in front
  #otherwise, it will also create an additional empty column
  tidyr::separate(col = binary, into = as.character(1:binary_length), sep = "(?<=[0-9])", convert = TRUE)

most_common <- separated_binary %>%
  #Return sum of each col
  #If it's greater than half nrow() then 1 is most common (and the inverse is true)
  summarise(across(.cols = everything(), .fns = ~sum(.) > (n()/2)))

gamma_binary   <- paste(as.integer(most_common), collapse = "")
epsilon_binary <- paste(as.integer(!most_common), collapse = "")

gamma_decimal    <- strtoi(gamma_binary, base = 2)
epsilon_decimal  <- strtoi(epsilon_binary, base = 2)

answer <- gamma_decimal * epsilon_decimal

## PUZZLE 2

#Write a function to find and filter by row
#Can make it recursive!
filter_binary <- function(binary_columns, colnumber = 1, output = "O2"){

  filter_value <- switch(output,
                         O2  = as.integer(sum(binary_columns[, colnumber]) >= nrow(binary_columns)/2),
                         CO2 = as.integer(!sum(binary_columns[, colnumber]) >= nrow(binary_columns)/2))

  filtered_binary <- binary_columns %>%
    filter(across(.cols = !!colnumber, .fns = ~. == !!filter_value))

  if (nrow(filtered_binary) > 1){

    Recall(binary_columns = filtered_binary, colnumber = colnumber + 1, output = output)

  } else {

    binary_outcome  <- paste(filtered_binary, collapse = "")
    decimal_outcome <- strtoi(binary_outcome, base = 2)

    return(decimal_outcome)

  }

}

filter_binary(binary_columns = separated_binary, output = "O2") * filter_binary(binary_columns = separated_binary, output = "CO2")
