# DAY 4

library(dplyr)

#Read first line of data that includes the announced numbers
number_calls_chr <- readLines(here::here("data/Day4.txt"), n = 1)

#Covert this to a vector of integers
number_calls_int <- as.integer(stringr::str_split(number_calls_chr, pattern = ",", simplify = TRUE))

#Read in the bingo boards
allboards <- as.integer(scan(here::here("data/Day4.txt"), what = "list", skip = 2))

#Convert into 3D matrix
board_array  <- array(allboards, dim = c(5, 5, length(allboards)/25))

## PUZZLE 1

result_array <- array(rep(FALSE, n = length(allboards)), dim = c(5, 5, length(allboards)/25))

#Another recursive function
result <- NULL
for (number in number_calls_int){

  result_array[which(board_array == number, arr.ind = TRUE)] <- TRUE

  board_status <- apply(result_array, MARGIN = 3, FUN = function(x){

    any(rowSums(x) == 5) | any(colSums(x) == 5)

  })

  has_winner <- any(board_status)

  if (has_winner) {

    #Find the winning board!
    winner <- board_array[,,board_status]
    unmarked <- winner[!result_array[,,board_status]]

    result <- sum(unmarked) * number

    break()

  }

}

## PUZZLE 2

board_array  <- array(allboards, dim = c(5, 5, length(allboards)/25))
result_array <- array(rep(FALSE, n = length(allboards)), dim = c(5, 5, length(allboards)/25))

#Recursive func is better here!!
play_bingo <- function(bingo_boards, numbers, i, current_results){

  number <- numbers[i]

  current_results[which(bingo_boards == number, arr.ind = TRUE)] <- TRUE

  board_status <- apply(current_results, MARGIN = 3, FUN = function(x){

    any(rowSums(x) == 5) | any(colSums(x) == 5)

  })

  if (length(board_status) == 1 & sum(board_status) == 1) {

    #Find the winning board!
    last_winner <- bingo_boards[,,1]
    unmarked <- last_winner[!current_results[,,1]]

    return(sum(unmarked) * number)

  } else {

    #Only losing boards
    losing_boards  <- bingo_boards[,,!board_status, drop = FALSE]
    losing_results <- current_results[,,!board_status, drop = FALSE]

    Recall(bingo_board = losing_boards, numbers = numbers, i = i + 1, current_results = losing_results)

  }

}

play_bingo(bingo_boards = board_array, numbers = number_calls_int, i = 1, current_results = result_array)
