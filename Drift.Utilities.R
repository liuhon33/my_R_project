# comments
# Names
# Introduction

# 1a)
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
  # slice A1, A2 and A2, A1
  a1a2 <- gen[(gen$gene1 == "A1") & (gen$gene2 == "A2"), ]
  a2a1 <- gen[(gen$gene1 == "A2") & (gen$gene2 == "A1"), ]
  # slice homozygous for A2
  a2a2 <- gen[(gen$gene1 == "A2") & (gen$gene2 == "A2"), ]
  return(c(nrow(a1a1), nrow(a1a2) + nrow(a2a1), nrow(a2a2)))
}

# 1e)
sim.many.generations <- function(n, m) {
  # m is the number of the generations
  # n is the population size
  gen <- gen_init(n)
  results <- data.frame(
    Generation = 0,
    A1A1 = population_fraction(gen)[1],
    heterozygous = population_fraction(gen)[2],
    A2A2 = population_fraction(gen)[3],
    stringsAsFactors = FALSE
  )
  gen <- gen_init(n)
  # this is the generation 0
  for (i in 1:m) {
    gen <- gen_next(gen)
    fractions <- population_fraction(gen)
    results <- rbind(
      results,
      data.frame(
        Generation = i,
        A1A1 = fractions[1],
        heterozygous = fractions[2],
        A2A2 = fractions[3],
        stringsAsFactors = FALSE
      )
    )
  }
  return(results)
}

fractions <- sim.many.generations(100, 100)
fractions2 <- sim.many.generations(20, 100)

plot(0:100, fractions$heterozygous, type = "l")
plot(0:100, fractions2$heterozygous, type = "l")
