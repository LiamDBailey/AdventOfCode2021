# DAY 2
library(dplyr)

#Load data
day2_data <- readr::read_delim(file = here::here("data/Day2.txt"), delim = "/t",
                               col_names = "moves", show_col_types = FALSE) %>%
  #Split column so that we have 'direction' and 'distance'
  tidyr::separate(col = moves, sep = " ", into = c("direction", "distance"), convert = TRUE)

## PUZZLE 1

#Write funcs to do each process
forward <- function(start, distance){

  start[1] <- start[1] + distance

  return(start)

}

up <- function(start, distance){

  start[2] <- start[2] - distance

  return(start)

}

down <- function(start, distance){

  start[2] <- start[2] + distance

  return(start)

}

move <- function(data, start,
                 forward_func,
                 up_func,
                 down_func){

  input  <- data[1, ]
  remain <- data[-1, ]

  new_coord <- switch(input$direction,
                      forward = forward_func(start = start, distance = input$distance),
                      down = down_func(start = start, distance = input$distance),
                      up = up_func(start = start, distance = input$distance))

  if (nrow(remain) > 0) {

    Recall(data = remain, start = new_coord,
           forward_func = forward_func,
           up_func = up_func,
           down_func = down_func)

  } else {

    return(new_coord)

  }

}

final_position <- move(data = day2_data, start = c(0, 0),
                       forward_func = forward,
                       up_func = up,
                       down_func = down)

prod(final_position[1], final_position[2])

## PUZZLE 2

#Write new funcs that can be input to the move function
forward_new <- function(start, distance){

  #Add an error to make sure we have 'aim'
  if (length(start) < 3) {

    stop("Coordinates should include X, Y, and 'aim'")

  }

  start[1] <- start[1] + distance
  start[2] <- start[2] + distance*start[3]

  return(start)

}

up_new <- function(start, distance){

  #Add an error to make sure we have 'aim'
  if (length(start) < 3) {

    stop("Coordinates should include X, Y, and 'aim'")

  }

  start[3] <- start[3] - distance

  return(start)

}

down_new <- function(start, distance){

  #Add an error to make sure we have 'aim'
  if (length(start) < 3) {

    stop("Coordinates should include X, Y, and 'aim'")

  }

  start[3] <- start[3] + distance

  return(start)

}

final_position <- move(data = day2_data, start = c(0, 0, 0),
                       forward_func = forward_new, up_func = up_new, down_func = down_new)

prod(final_position[1], final_position[2])
