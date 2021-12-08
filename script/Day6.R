# DAY 6

#Load data
ages <- as.integer(scan(file = here::here("data/Day6.txt"), sep = ","))
pop_size <- NULL

table_ages <- table(ages)

ages_vector <- tidyr::replace_na(table_ages[as.character(0:8)], 0)
names(ages_vector) <- 0:8

for (i in 1:80){

  reproducers <- ages_vector["0"]

  ages_vector[as.character(0:7)] <- ages_vector[as.character(1:8)]

  ages_vector["6"] <- ages_vector["6"] + reproducers
  ages_vector["8"] <- reproducers

  pop_size <- append(pop_size, sum(ages_vector))

}

pop_size[length(pop_size)]

## PUZZLE 2

ages <- as.integer(scan(file = here::here("data/Day6.txt"), sep = ","))
pop_size <- NULL

table_ages <- table(ages)

ages_vector <- tidyr::replace_na(table_ages[as.character(0:8)], 0)
names(ages_vector) <- 0:8

for (i in 1:256){

  reproducers <- ages_vector["0"]

  ages_vector[as.character(0:7)] <- ages_vector[as.character(1:8)]

  ages_vector["6"] <- ages_vector["6"] + reproducers
  ages_vector["8"] <- reproducers

  pop_size <- append(pop_size, sum(ages_vector))

}

pop_size[length(pop_size)]

## BONUS! EXPONENTIAL MODEL!

#Estimate the value of R for exponential growth
Rmodel <- lm(lead(pop_size) ~ pop_size)
#Plot show's it's clearly exponential growth
ggplot() +
  geom_line(aes(x = 1:length(pop_size), y = pop_size)) +
  geom_line(aes(x = 2:length(pop_size), y = predict(Rmodel)), colour = "blue")

#Can we use this to get our result
Rest <- Rmodel$coefficients[2]

300*(Rest)^256
