# Maximum likelihood demo

y <- 5
n <- 14

curve(choose(n,y) * x^5 * (1-x)^(n-y),
      from = 0, to = 1,
      xlab = "pi", ylab = "Likelihood")
abline(v = y/n, col = "red")

# Ho: pi = 0.40; Ha: pi < 0.40

# Exact p-value
pbinom(5, 14, 0.40)

# Approximate p-value (normal approximation)
pnorm(5/14, 0.40, sqrt(.40*.60/14))

# Visually
barplot(dbinom(0:14, 14, 0.40),
        names.arg = 0:14,
        xlab = "Y", ylab = "Probability")

