source("Drift.Utilities.R")
args <- commandArgs(trailingOnly = TRUE)

check_args <- function(args) {
  if (length(args) > 1) {
    cat("You put more than one argument, please unter ONLY ONE even integer representing the population size", "\n")
    quit()
  }
  if (length(args) == 0) {
    cat("Please enter an even number representing the population size", "\n")
    quit()
  } else {
    if (is.na(as.numeric(args))) {
      cat("please enter an number, do not enter a string", "\n")
      quit()
    } else {
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

args <- check_args(args)
print(args)