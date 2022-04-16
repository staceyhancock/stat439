## Asthma Prevalence over Time
library(tidyverse)

## Researcher records whether 500 children have asthma or not
## at age 13, then follows up with these same children to see
## if they have asthma or not 7 years later (age 20).

dat <- matrix(c(50, 8, 22, 420), 2, 2)
dimnames(dat) <- list(Age13 = c("Yes","No"), Age20 = c("Yes","No"))
dat

# Summary stats - proportion with asthma at each age
# Age 13
p13 <- rowSums(dat)[1]/sum(dat)
# Age 20
p20 <- colSums(dat)[1]/sum(dat)

# Think that prevalence of asthma decreases over time:
# Ha: Probability of asthma at age 13 > Probability of asthma at age 20
# or
# Ha: Probability of asthma at age 13 but not 20 >
#     Probability of no asthma at age 13 but asthma at 20

# Under H0 that the probabilities are equal:
# (Yes,No) cell ~ Bin(22+8,.5)

# Calculate one-sided p-value:
pbinom(22, 30, .5, lower.tail=FALSE)
# Reject H0.

# R function for McNemar's test calculates approximate two-sided p-value:
mcnemar.test(dat, correct=FALSE)
# Only does chi-squared approximation and two-sided.

# Wald confidence interval
n12 <- 22
n21 <- 8
n <- 500
se <- (1/n) * sqrt( (n12+n21) - (n12-n21)^2/n )
p13 - p20 + c(-1,1)*pnorm(0.975)*se


## What if we had ignored the dependence?
## Recorded data as two independent samples of 500 individuals each:

dat.indep <- matrix(c(72,58,428,442), 2 ,2)
dimnames(dat.indep) <- list(Age = c("13","20"), Asthma = c("Yes","No"))
dat.indep

# Wrong analysis!!! Treat as independent samples:
chisq.test(dat.indep, correct=FALSE) # Uses two-sided alternative -->
# or prop.test
# One-sided p-value:
0.188/2
# Fail to Reject H0. Opposite conclusion!

# Wrong analysis!! Treat as independent samples:
n13 <- rowSums(dat)[1]
n20 <- colSums(dat)[1]
se_indep <- sqrt( p13*(1-p13)/n13 + p20*(1-p20)/n20)
p13 - p20 + c(-1,1)*pnorm(0.975)*se_indep


##
### Modeling approach
##

# Data in ungrouped wide form:
asthma <- data.frame(
  id = 1:500,
  age13 = c(rep("Yes", 50+22), rep("No", 8+420)),
  age20 = c(rep("Yes", 50), rep("No", 22), rep("Yes", 8), rep("No", 420))
)
# Check that data were entered correctly
xtabs(~ age13 + age20, data = asthma)

# Reshape to long form (required for most modeling and visualization):
library(tidyr)  # for pivot_wider and pivot_longer functions
asthma_long <- pivot_longer(asthma,
  cols = 2:3, # Columns to pivot to longer format
  names_to = "age", # Name of variable that corresponds to column in wide format
  values_to = "asthma" # Name of response variable
)

asthma_long %>% ggplot(aes(x = age, fill = asthma)) +
  geom_bar(stat = "count", position = "dodge") # Counts

asthma_long %>% ggplot(aes(x = age, fill = asthma)) +
  geom_bar(stat = "count", position = "stack") # Stacked counts

asthma_long %>% ggplot(aes(x = age, fill = asthma)) +
  geom_bar(stat = "count", position = "fill") # Stacked percentages

# Response must be 0's and 1's
asthma_long$y <- as.numeric(asthma_long$asthma == "Yes")

### Marginal model (population-averaged)
library(gee)
fit <- gee(y ~ age, id = id, family = binomial(link = "logit"), 
           corstr = "exchangeable", data = asthma_long)
summary(fit)

### Subject-specific model (with random effects)
library(lme4)
fit2 <- glmer(y ~ age + (1|id), family = binomial(link = "logit"), 
              data = asthma_long, nAGQ = 80)
summary(fit2)

# Warning about algorithms that fit these models... (the next code takes a long time to run)
tmp <- NULL
for(i in 1:100){
  tmpfit <- glmer(y ~ age + (1|id), family = binomial(link = "logit"), data = asthma_long, nAGQ = i)
  tmp[i] <- summary(tmpfit)$coef[2,1]
}
plot(x = 1:100, y = tmp, type = "l",
     xlab = "Number of GHQ points", ylab = "Estimated slope")

# Alternate package
library(nlme)
fit3 <- nlme(y ~ age, random = ~1|id, 
             data = asthma_long) #.... not sure why this isn't working

# Alternate package (can only do random intercept)
library(glmmML)
fit4 <- glmmML(y ~ age, cluster = id, family = binomial, data = asthma_long)


# Compare estimates of odds ratios --> very different!
exp(fit$coef[2]) # Population-averaged
exp(summary(fit2)$coef[2,1]) # Subject-specific
