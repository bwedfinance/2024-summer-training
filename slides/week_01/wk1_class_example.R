# wk1_class_example
# Last updated 2024-06-03 by Krista Kaput

# load -------------
library(tidyverse)

# CLass examples -----------

# Variables can be stored as single values or as *vectors* of multiple values
x <- 3 

y <- c(2, 4, 5) 
z <- c("a", "b", "c") 

df <- tibble(letters = z, nums = y) 

df 

# basic r use ------------

# creating variables using the assignment operator "<-"
var1 <- 12
var2 <- 34

# you can do operations on variables
var1 + var2

# we need to store them as a new variable
var1_var2_sum <- var1 + var2

# you can group variables together in a vector using the 
# concatenate function "c()" to create vectors
var_values <- c(var1, var2)
var_names <- c("Tom Brady", "Nolan Ryan")

# you can run functions across the values in a vector
mean(var_values)

# individual positions in vectors can be referenced
var_names[2]

# vectors of equal length can be grouped into tables, called data frames
athlete_df <- data.frame(name = var_names, jersey = var_values)

# column names are referenced using "$"
athlete_df$name

# you can run functions on data frame columns
mean(athlete_df$jersey)

# but you need to be sure you're using the right datatype
mean(athlete_df$name)


View(athlete_df)





