## Very simple Tule elk population example.  Does not account for adult lifespan..

P0 <- 30
MAX.Y <- 40
R <- rnorm(MAX.Y, 0.3, 0.15)
K <- rnorm(MAX.Y, 400, 100)

p <- rep(P0, MAX.Y)
for (t in 1:(MAX.Y - 1))
{
  p[t+1] <- p[t] + R[t] * (1 - p[t]/K[t]) * p[t]
}
plot(p, xlab = "Year", ylab = "Elk Count", type = "b")





## example of log likelihood function
N <- 10
n <- 4
f <- function(N,n,p) {log(dbinom(n,N,p))}

ps <- seq(0,1,0.01)
plot(ps, f(N,n,ps), type = "l", ylab = "L-L", xlab = "p")
abline(v = 0.4, lty = "dashed")

g <- optimize(f, lower = 0, upper = 1, N = 10, n = 4, maximum = T)
