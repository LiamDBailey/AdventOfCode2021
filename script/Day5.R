# DAY 5
library(readr)
library(dplyr)

#Load data. We now have our x and y coordinates as separate comma separated character strings
#Use readr to start because it allows for separator that is not 1 bit (unlike e.g. scan)
raw_data <- readr::read_delim(here::here("./data/Day5.txt"),
                              delim = " -> ", col_names = c("start", "end"),
                              show_col_types = FALSE,
                              col_types = list(start = readr::col_character(),
                                               end = readr::col_character()))

#Convert the characters into a 4col numeric matrix
start_point <- stringr::str_split(raw_data$start, pattern = ",", simplify = TRUE)
end_point   <- stringr::str_split(raw_data$end, pattern = ",", simplify = TRUE)
all_points  <- cbind(start_point, end_point)
all_points  <- as.numeric(all_points)
dim(all_points) <- c(nrow(raw_data), 4)

#Identify only horizontal or vertical lines
nondiag <- all_points[apply(all_points, MARGIN = 1, FUN = function(x){

  x[1] == x[3] | x[2] == x[4]

}), ]

#Identify max x and max y. Matrix goes to 1000
max_y <- max(c(all_points[,2], all_points[,4]))
max_x <- max(c(all_points[,1], all_points[,3]))

dim <- max(c(max_y, max_x))

#Create a matrix of all 0s.
zero_mat <- matrix(0, nrow = dim + 1, ncol = dim + 1)

#Loop through and add 1 every time in a line
for (i in 1:nrow(nondiag)) {

  line <- nondiag[i, ]
  ys   <- (line[1]:line[3]) + 1
  xs   <- (line[2]:line[4]) + 1

  zero_mat[xs, ys] <- zero_mat[xs, ys] + 1

}

sum(zero_mat > 1)

####
diag <- all_points[apply(all_points, MARGIN = 1, FUN = function(x){

  x[1] != x[3] & x[2] != x[4]

}), ]

for (i in 1:nrow(diag)) {

  line <- diag[i, ]

  down  <- line[1] < line[3]
  right <- line[2] < line[4]

  start_value <- line[1]*ncol(zero_mat) + line[2]+1
  diff_y      <- abs(line[3] - line[1]) + 1

  direction_funcs <- case_when(down & right ~ c(`+`, `+`),
                               down & !right ~ c(`+`, `-`),
                               !down & right ~ c(`-`, `-`),
                               !down & !right ~ c(`-`, `+`))

  zero_mat[direction_funcs[[1]](start_value, 0L:((diff_y) - 1L) * (direction_funcs[[2]](dim(zero_mat)[1L], 1)))] <- zero_mat[direction_funcs[[1]](start_value, 0L:((diff_y) - 1L) * (direction_funcs[[2]](dim(zero_mat)[1L], 1)))] + 1

  #1 + dim(zero_mat[1L]*start_y) will start the diagonal in the correct column
  #0L:((end_y + 1) - 1L) counts how many rows it should move along the diagonal (equal to the distance in y)
  #* (dim(zero_mat)[1L] + 1) ensures that every step moves 1 more than the number of cols (i.e. moves on the diagonal)

}

sum(zero_mat > 1)

