# DAY 8
library(dplyr)

#Load data
day8_data <- readr::read_delim(file = here::here("data/Day8.txt"), delim = " | ",
                               col_names = c("inputs", "outputs"), show_col_types = FALSE)

#Actual letter number combos
correct_codes <- 0:9
names(correct_codes) <- c("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")

## PUZZLE 1

sum(nchar(stringr::str_split(day8_data$outputs, pattern = " ", simplify = TRUE)) %in% c(2, 3, 4, 7))

## PUZZLE 2

numbers <- NULL
for (i in 1:nrow(day8_data)) {

  #Split output
  split_output <- stringr::str_extract_all(stringr::str_split(day8_data$inputs[i], pattern = " ", simplify = TRUE), pattern = "[a-z]{1}")

  #If there are two values, it must correspond to c and f
  output1 <- split_output[unlist(lapply(split_output, \(letters) length(letters) == 2))][[1]]
  split_output[unlist(lapply(split_output, \(letters) length(letters) == 2))] <- NULL

  #If there are three values, it must correspond to a, c and f
  output7 <- split_output[unlist(lapply(split_output, \(letters) length(letters) == 3))][[1]]
  split_output[unlist(lapply(split_output, \(letters) length(letters) == 3))] <- NULL

  #If there are four values, it must correspond to b, c, d and f
  output4 <- split_output[unlist(lapply(split_output, \(letters) length(letters) == 4))][[1]]
  split_output[unlist(lapply(split_output, \(letters) length(letters) == 4))] <- NULL

  #If there are seven values, it must correspond to a-g
  output8 <- split_output[unlist(lapply(split_output, \(letters) length(letters) == 7))][[1]]
  split_output[unlist(lapply(split_output, \(letters) length(letters) == 7))] <- NULL

  #Letter that's not shared between 1 and 7 must be a
  a <- setdiff(output7, output1)

  #Value of 6 characters that contains the letters in 4 must be 9.
  #The missing value here is e
  output9 <- split_output[unlist(lapply(split_output, \(letters) length(letters) == 6 & all(output4 %in% letters)))][[1]]
  split_output[unlist(lapply(split_output, \(letters) length(letters) == 6 & all(output4 %in% letters)))] <- NULL
  e <- setdiff(output8, output9)

  #Remaining 6 letter character that includes the same letters as output1 must be 0.
  #The missing value here is d
  output0 <- split_output[unlist(lapply(split_output, \(letters) length(letters) == 6 & all(output1 %in% letters)))][[1]]
  split_output[unlist(lapply(split_output, \(letters) length(letters) == 6 & all(output1 %in% letters)))] <- NULL
  d <- setdiff(output8, output0)

  #Last remaining length 6 is 6
  #Missing value is c
  output6 <- split_output[unlist(lapply(split_output, \(letters) length(letters) == 6))][[1]]
  split_output[unlist(lapply(split_output, \(letters) length(letters) == 6))] <- NULL
  c <- setdiff(output8, output6)

  #The letter in 7 that wasn't just extracted is f
  f <- setdiff(output1, c)

  #Letter in 4 that hasn't yet been deciphered in b
  b <- setdiff(output4, c(c, d, f))

  #Remaining letter in 0 after removing all known ones is g
  g <- setdiff(output0, c(a, b, c, e, f))

  #Dictionary
  dict <- letters[1:7]
  names(dict) <- c(a, b, c, d, e, f, g)

  #Now we can translate our output
  split_codes <- stringr::str_extract_all(stringr::str_split(day8_data$outputs[i], pattern = " ", simplify = TRUE), pattern = "[a-z]{1}")

  output <- correct_codes[unlist(lapply(split_codes, \(letters) paste(dict[letters][order(dict[letters])], collapse = "")))]

  numbers <- append(numbers, as.integer(paste(output, collapse = "")))

}
