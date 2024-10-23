# comments
# Names
# Introduction

# 1a)
n = 5
# gen_init is the function that initialize the function,
# returning the first generation of the population.
# in this population, every individual is heterouzygous with
# gene A1 and A2
gen_init <- function(n){
    gene1 <- rep("A1", n)
    gene2 <- rep("A2", n)
    gen0 <- data.frame(gene1, gene2)
    # gen0 is the initial generation
    return(gen0)
}

# 1b)
rand_inherit <- function(father, mother){
    # father/mother is a vector
    father_allele <- sample(father, 1)
    mother_allele <- sample(mother, 1)
    child <- c(father_allele, mother_allele)
    return(child)
}
