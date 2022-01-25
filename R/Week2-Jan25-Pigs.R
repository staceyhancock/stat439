## R Code for Pigs Example - 1/25/22

## Test if probability pig lands on its back is 0.20 or not.
## H0: pi = 0.2 vs Ha: pi != 0.2

## Class data: 

## Likelihood function: n = 10 ,y = 4
n <-
y <- 
# x = variable = parameter pi:
curve(x^y * (1-x)^(1-y), 0, 1, xlab = "pi", ylab= "Likelihood")
# Maximized at pi = 4/10

## p-value?
## Find probability of 4 or something less likely
barplot(dbinom(0:n, n, 0.2), names.arg = 0:n)
abline(h = dbinom(y, n, .2))
1-pbinom(y-1, n, .2)

# or use built-in function
binom.test(y, n, .2)

## What if we observed the same sample proportion in 100 tosses?
n <- 100
y <- 
barplot(dbinom(0:n, n, 0.2), names.arg = 0:n)
# Now 40 is extremely unlikely
binom.test(y, n, .2)
