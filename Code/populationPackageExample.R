library(population)


# Initial number of individuals
n0 <- 15
n1 <- 20
n2 <- 15
n3 <- 10
n4 <- 10
# Age-specific survival rates
s0 <- 0.7
s1 <- 0.8
s2 <- 0.8
s3 <- 0.8
s4 <- 0.9
# Age-specific number of offspring
b1 <- 0.0
b2 <- 0.8
b3 <- 0.8
b4 <- 0.8
b5 <- 0.8
# Project 10 years ahead repeated 10000 times
years <- 15
runs <- 100
get_cores(runs)
get_runs <- 10000

results <- project(
  years = years,
  runs = runs,
  initial_population = c(n0, n1, n2, n3, n4),
  survival = cbind(c(s0, s1, s2, s3, s4), 0.0), # no environmental stochasticity
  litter = cbind(c(b1, b2, b3, b4, b5), 0.0) # no environmental stochasticity
)


# Plot projection
plot_projection(results, "mean")
plot_projection(results, kind="median")
# Equivalent model with a post-breeding Leslie matrix
postM <- matrix(nrow=5, ncol=5, byrow=TRUE, data = c(
  s0*b1, s1*b2, s2*b3, s3*b4, s4*b5,
  s0, 0, 0, 0, 0,
  0, s1, 0, 0, 0,
  0, 0, s2, 0, 0,
  0, 0, 0, s3, 0
))
popvector <- c(n0, n1, n2, n3, n4)
N <- numeric(years)
N[1] <- sum(popvector)
for (i in 2:years) {
  popvector <- postM
  N[i] <- sum(popvector)
}
# Check we get similar results
lines(1:years, N, col="blue", lwd=2)


years <- 10
runs <- 100
init.pop <- c(30, 20, 15, 12, 10, 9, 8, 7, 6, 5)
surv.md <- c(0.5, 0.7, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9)
surv.sd <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
surv.msd <- cbind(surv.md, surv.sd)
litter.md <- c(0.2, 1.1, 2.8, 2.8, 2.8, 2.8, 2.8, 2.8, 1.8, 0.2)
litter.sd <- c(0.1, 0.2, 0.15, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
litter.msd <- cbind(litter.md, litter.sd)
nclass <- 4 # vary number of classes
projection <- project(
  years = years,
  runs = runs,
  initial_population = init.pop[1:nclass],
  survival = surv.msd[1:nclass,],
  litter = litter.msd[1:nclass,]
)

plot_projection(projection, "mean")

