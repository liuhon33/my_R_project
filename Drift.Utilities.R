# comments
# Names
# Introduction

# 1a)
n <- 100
# gen_init is the function that initialize the function,
# returning the first generation of the population.
# in this population, every individual is heterouzygous with
# gene A1 and A2
gen_init <- function(n) {
  gene1 <- rep("A1", n)
  gene2 <- rep("A2", n)
  gen0 <- data.frame(gene1, gene2, stringsAsFactors = FALSE)
  # gen0 is the initial generation
  return(gen0)
}

# 1b) rand_inherit is the function that takes the input
# p(probability of interiting A1), and the vector of father
# alleles and mother alleles
rand_inherit <- function(p, father, mother) {
  # first check if parent is homozygous
  # if the parent is homo, no need to sample
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
  # children genotype is the combination of the parent alleles
  return(child)
}

# 1c)
# Note that we made an assumption: We ignored the sex
# of these population. If we consider sex, we cannot randomly
# select the 2 individuals and let them have the offspring
gen_next <- function(gen, p = 0.5, offspring_per_pair = 2) {
  n <- nrow(gen)
  # initialize the next generation
  next_gen <- data.frame(gene1 = character(0), gene2 = character(0))
  # the number of population is just the number of rows
  for (i in 1 : (n / 2)) {
    # n is the current population
    # I use unname, unlist just to make the output a vector
    # instead of a list or a table (input is the slicing 
    # of a dataframe)
    father <- unname(unlist(gen[(2 * i - 1), ]))
    mother <- unname(unlist(gen[2 * i, ]))

    # generate the offspring per pair
    # even though in the lecture we only assume that
    # each pair has 2 offsring, but I like to increase more
    for (j in 1:offspring_per_pair) {
      child <- rand_inherit(p, father, mother)
      # this child is a vector
      child <- as.list(child)
      next_gen <- rbind(next_gen, child)
    }
  }
  colnames(next_gen) <- c("gene1", "gene2")
  next_gen <- next_gen[sample(nrow(next_gen)), ]
  # this line reorders the data frame of the next generation
  # notice that I could use sample(n), but this is under the
  # assuption that each pair has two offspring. but my code did
  # not assume this, so I have tu use nrow(next_gen)
  rownames(next_gen) <- NULL
  # this line reorders the row numbers
  return(next_gen)
}

# 1d)
population_fraction <- function(gen) {
  # use slicing
  # slice A1, A1
  a1a1 <- gen[(gen$gene1 == "A1") & (gen$gene2 == "A1"), ]
  a1a2 <- gen[(gen$gene1 == "A1") & (gen$gene2 == "A2"), ]
  a2a1 <- gen[(gen$gene1 == "A2") & (gen$gene2 == "A1"), ]
  a2a2 <- gen[(gen$gene1 == "A2") & (gen$gene2 == "A2"), ]
  return(c(nrow(a1a1), nrow(a1a2) + nrow(a2a1), nrow(a2a2)))
}
