# Name: Hongrui Liu
# Student number: 1004779475
# user account: hongrui.liu@mail.utoronto.ca
# username: tmp_hongruiliu

# This is the driver script. It uses the function
# from Drift.Utilities.R
# It accepts a user input from the command line
# Run "Rscript Drift.Analysis.R n" (n is an even number)
# will give us the plot of the proportion of
# the heterozygous individuals within the population
# as the population keeps reproducing randomly

source("Drift.Utilities.R")
args <- commandArgs(trailingOnly = TRUE)

# this function will check if the user put an even integer
check_args <- function(args) {
  # if user put more than 2 values, stop the program
  if (length(args) > 1) {
    cat("only one value is accepted", "\n")
    quit()
  }
  # if the user does not put any values at the command line,
  # prompt them and stop the program
  if (length(args) == 0) {
    cat("Please enter an even number representing the population size", "\n")
    quit()
  } else {
    # if the user put none-number type, stop the program
    if (is.na(as.numeric(args))) {
      cat("please enter an number, do not enter a string", "\n")
      quit()
    } else {
      # if the user put any number other than the even integers
      # stop the program
      convert_args <- as.numeric(args)
      if (convert_args %% 2 != 0) {
        cat("please enter an even integer")
        quit()
      } else {
        return(convert_args)
      }
    }
  }
}

n <- check_args(args)
# we convert the args into a number, which is the population size

m <- 100
fractions <- sim.many.generations(n, m)
plot(0:100, fractions$heterozygous, type = "l")