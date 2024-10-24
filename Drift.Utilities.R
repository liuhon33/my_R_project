# Name: Hongrui Liu
# Student number: 1004779475
# user account: hongrui.liu@mail.utoronto.ca
# username: tmp_hongruiliu

# this script contains five function:
# gen_init, rand_inherit, gen_next, population_fraction
# and sim.many.generations corresponding to part 1a) to 1e)
# the later function will be built from the previous functions
# if you want to run the code, simple remove the # sign at the
# end of the page for the testing code

# 1a) gen_init is the function that initialize the function,
# returning the first generation of the population. The input
# n is the number of population
# In this population, every individual is heterouzygous with
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
# alleles and mother alleles, and gives us the output of the
# children genotype
rand_inherit <- function(p, father, mother) {
  # if the parent is homozygous, directly inherit the allele
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

# 1c) gen_next is the function that predict the genotype of
# the next generation. The input is a dataframe of parent generation
# genotypes, the output is the shuffled result of the children
# generation, assuming that we do not consider sex, every parents
# has two children, and everyone has a mate.
# p is the probability of a heterouzygote giving A1 allele
# to the next generation
gen_next <- function(gen, p = 0.5, offspring_per_pair = 2) {
  # n is the current population
  n <- nrow(gen)
  # initialize the next generation
  next_gen <- data.frame(gene1 = character(0), gene2 = character(0))
  # the number of population is just the number of rows
  for (i in 1 : (n / 2)) {
    # use unname, unlist to convert the dataframe into a vector
    father <- unname(unlist(gen[(2 * i - 1), ]))
    mother <- unname(unlist(gen[2 * i, ]))

    # generate the offspring per pair
    # I added a functionality that now each parent can have
    # more or less than 2 children
    for (j in 1:offspring_per_pair) {
      child <- rand_inherit(p, father, mother)
      # this child is a vector
      child <- as.list(child)
      next_gen <- rbind(next_gen, child)
    }
  }
  colnames(next_gen) <- c("gene1", "gene2")
  # this line reorders the data frame of the next generation
  # if every pair has 2 offspring, then nrow(next_gen) = nrow(gen)
  next_gen <- next_gen[sample(nrow(next_gen)), ]
  # Reorders the row numbers
  rownames(next_gen) <- NULL
  return(next_gen)
}

# 1d) population_fraction gives us the fractions of the genotype
# of the current generation. For example, if the current gen0
# has 4 individuals, A1A1, A1A1, A1A2, A2A2, the output of
# population_fraction will be a vector (0.5, 0.25, 0.25)
# the first number of the vector is the fraction of A1A1, etc.
population_fraction <- function(gen) {
  # slice A1A1 genotype
  a1a1 <- gen[(gen$gene1 == "A1") & (gen$gene2 == "A1"), ]
  # slice A1A2 and A2A1 genotype
  a1a2 <- gen[(gen$gene1 == "A1") & (gen$gene2 == "A2"), ]
  a2a1 <- gen[(gen$gene1 == "A2") & (gen$gene2 == "A1"), ]
  # slice homozygous for A2
  a2a2 <- gen[(gen$gene1 == "A2") & (gen$gene2 == "A2"), ]
  total <- nrow(gen)
  # divided by total give us the fraction
  a1a1_fraction <- nrow(a1a1) / total
  # A1A2 is the same as the A2A1
  hetero_fraction <- (nrow(a1a2) + nrow(a2a1)) / total
  a2a2_fraction <- nrow(a2a2) / total
  return(c(a1a1_fraction,
           hetero_fraction,
           a2a2_fraction))
}

# 1e) This function has four inputs: m is the number of the generations
# n is the population size. p is the probablity of a inheriting gene A1
# for a heterozygote. The default value of p is 0.5 according to the
# biological laws. We assume each pair has 2 offsprings
sim.many.generations <- function(n, m, p = 0.5, offspring_per_pair = 2) {
  # initialize the population
  gen <- gen_init(n)
  # initilize the dataframe
  results <- data.frame(
    Generation = 0,
    A1A1 = population_fraction(gen)[1],
    # note that population_fraction output is a vector
    # the first position is the fraction of the A1A1
    heterozygous = population_fraction(gen)[2],
    A2A2 = population_fraction(gen)[3],
    stringsAsFactors = FALSE
  )
  for (i in 1:m) {
    gen <- gen_next(gen, p = p, offspring_per_pair = offspring_per_pair)
    fractions <- population_fraction(gen)
    # Append the fractions of the next generation to the results
    results <- rbind(
      results,
      data.frame(
        # convert the vector to a dataframe, then use rbind()
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


# These are my testing lines

# fractions <- sim.many.generations(100, 100, offspring_per_pair = 2)
# fractions2 <- sim.many.generations(20, 100)

# plot(0:100, fractions$heterozygous, type = "l")
# plot(0:100, fractions2$heterozygous, type = "l")
