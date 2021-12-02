# DAY 1
library(dplyr)

#Load data
day1_data <- readr::read_delim(file = here::here("data/Day1.txt"), delim = "/t",
                               col_names = "depth", show_col_types = FALSE)

## PUZZLE 1

#tidyverse way
day1_data %>%
  dplyr::mutate(deeper = depth < dplyr::lead(depth)) %>%
  dplyr::pull(deeper) %>%
  sum(na.rm = TRUE)

#base R way
sum(day1_data$depth[1:nrow(day1_data) - 1] < day1_data$depth[2:nrow(day1_data)])

## PUZZLE 2

#tidyverse way
day1_data %>%
  #Create 3 day sum
  dplyr::mutate(threeday_sum = depth + lead(depth, n = 1) + lead(depth, n = 2)) %>%
  #Remove NAs
  dplyr::filter(!is.na(threeday_sum)) %>%
  #Apply same lead check
  dplyr::mutate(deeper = threeday_sum < dplyr::lead(threeday_sum)) %>%
  dplyr::pull(deeper) %>%
  sum(na.rm = TRUE)

#base R way
new_vector <- day1_data$depth + day1_data$depth[2:(nrow(day1_data) + 1)] + day1_data$depth[3:(nrow(day1_data) + 2)]
new_vector_noNA <- new_vector[!is.na(new_vector)]
sum(new_vector_noNA[1:length(new_vector_noNA) - 1] < new_vector_noNA[2:length(new_vector_noNA)])

## BONUS. ALLOW SIZE OF SLIDING WINDOW TO BE DYNAMIC

#tidyverse
#Use package lay to sum rows
library(lay)
window_size <- 3
day1_data %>%
  #We add window_size
  dplyr::mutate(purrr::map_dfc(.x = 1:(window_size - 1),
                               .f = function(i, depth){

                                 dplyr::tibble("depth_lag{i}" := lead(depth, n = i))

                               }, depth = .data$depth)) %>%
  dplyr::mutate(depth_sum = lay(across(.cols = everything()), .fn = sum)) %>%
  dplyr::filter(!is.na(depth_sum)) %>%
  #Apply same lead check
  dplyr::mutate(deeper = depth_sum < dplyr::lead(depth_sum)) %>%
  dplyr::pull(deeper) %>%
  sum(na.rm = TRUE)

#base R
window_size <- 3
i <- 1
new_vector <- day1_data$depth
while (i < window_size) {

  new_vector <- new_vector + day1_data$depth[1:nrow(day1_data) + i]
  i <- i + 1

}

new_vector_noNA <- new_vector[!is.na(new_vector)]
sum(new_vector_noNA[1:(length(new_vector_noNA) - 1)] < new_vector_noNA[2:length(new_vector_noNA)])

#Make into funcs and compare run times
tidy_windowfunc <- function(window_size = 3){

  day1_data %>%
    #We add window_size
    dplyr::mutate(purrr::map_dfc(.x = (1:window_size) - 1,
                                 .f = function(i, depth){

                                   dplyr::tibble("depth_lag{i}" := lead(depth, n = i))

                                 }, depth = .data$depth)) %>%
    dplyr::mutate(depth_sum = lay(across(.cols = contains("lag")), .fn = sum)) %>%
    dplyr::filter(!is.na(depth_sum)) %>%
    #Apply same lead check
    dplyr::mutate(deeper = depth_sum < dplyr::lead(depth_sum)) %>%
    dplyr::pull(deeper) %>%
    sum(na.rm = TRUE)

}

base_windowfunc <- function(window_size = 3){

  i <- 1
  new_vector <- day1_data$depth
  while (i < window_size) {

    new_vector <- new_vector + day1_data$depth[1:nrow(day1_data) + i]
    i <- i + 1

  }

  new_vector_noNA <- new_vector[!is.na(new_vector)]
  sum(new_vector_noNA[1:(length(new_vector_noNA) - 1)] < new_vector_noNA[2:length(new_vector_noNA)])

}


#Benchmark results
bench_df <- purrr::map_df(.x = 1:40,
                          .f = function(i){

                            times <- bench::mark(tidy_windowfunc(window_size = i),
                                                 base_windowfunc(window_size = i))

                            data.frame(method = c("tidy", "base"),
                                       size = i,
                                       number_per_sec = times$`itr/sec`,
                                       speed_sec = 1/times$`itr/sec`)

                          })

library(ggplot2)
ggplot(bench_df) +
  geom_line(aes(x = size, y = number_per_sec, colour = method), size = 1) +
  geom_point(aes(x = size, y = number_per_sec, colour = method), stroke = 1.5, shape = 21, fill = "white") +
  scale_y_continuous(name = "Iterations/sec",
                     breaks = seq(0, 15000, 5000), limits = c(0, 15000)) +
  scale_x_continuous(name = "Window size") +
  scale_colour_discrete(name = "") +
  labs(title = "Advent of Code Day 1",
       subtitle = "base v. tidyverse approach") +
  theme_classic(base_family = "Courier New") +
  theme(axis.text = element_text(colour = "black"),
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black"),
        legend.title = element_blank())

ggplot2::ggsave("plots/Day1.png")
