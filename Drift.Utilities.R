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
rand_inherit <- function(p, father, mother) {
    # father/mother is a vector

    # check if parent is homozygous
    inherit_allele <- function(parent, p) {
        if (parent[1] == parent[2]) {
            return(parent[1])
        } else {
            # Heterozygous: sample based on p
            return(sample(c("A1", "A2"), size = 1, prob = c(p, 1 - p)))
        }
    }
    father_allele <- inherit_allele(father, p)
    mother_allele <- inherit_allele(mother, p)
    child <- c(father_allele, mother_allele)
    return(child)
}
