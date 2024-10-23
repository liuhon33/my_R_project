source("Drift.Utilities.R")
father <- c("A1", "A2")
mother <- c("A1", "A2")
# this function is just for testing the 1b) function

# testing the function
children_matrix <- replicate(100, rand_inherit(father, mother))
# Transpose the matrix so that each row represents a child
children_df <- as.data.frame(t(children_matrix))

# Assign meaningful column names
colnames(children_df) <- c("Father_Allele", "Mother_Allele")

factor(children_df[ ,"Father_Allele"])
summary(factor(children_df[ ,"Father_Allele"]))
