## Estimating linear contrasts of coefficients

### Berkeley Data (grouped)
Berk.grp = data.frame(Sex = rep(c("Male","Female"),each=6), 
						Program = rep(c("A","B","C","D","E","F"),2),
						Admit = c(511,352,120,137,53,22,
								89,17,202,132,95,24),
						Deny = c(314,208,205,270,138,351,
								19,8,391,243,298,317))
Berk.grp

### Simple model - marginal association between admission and program:
mod1 <- glm(cbind(Admit, Deny) ~ Program, family=binomial, data=Berk.grp)
summary(mod1)

## Variance-covariance matrix of fitted coefficients:
vcov(mod1)

beta.hat <- coef(mod1)
V.hat <- vcov(mod1)

# Estimated odds ratio of admission for prog. B compared to C:
exp(beta.hat[2]-beta.hat[3])

# Interpret: The estimated odds of admission for program B are
# 216% higher than (3.16 times)  that for program C.

## Note: In lecture notes, we calculated CI for the odds ratio
## of admission for prog. C compared to B. (Try this on your own.)

# Estimated variance of beta2hat - beta1hat:
V.hat[2,2] + V.hat[3,3] - 2*V.hat[2,3]
# Estimated standard error:
se <- sqrt(V.hat[2,2] + V.hat[3,3] - 2*V.hat[2,3])
se

# Approximate 95% CI for beta2-beta1:
beta.hat[2]-beta.hat[3] + c(-1,1)*1.96*se
# For odds ratio:
exp( beta.hat[2]-beta.hat[3] + c(-1,1)*1.96*se )

## Interpret: 
# We are 95% confident that the true odds of admission when applying
# to Program B are between 155% to 292% higher than when applying
# to Program C.

### In matrix form: Log-odds of Program B - log-odds of Program C
A <- matrix(c(0,1,-1,0,0,0), nrow=1)
# Estimate:
A %*% beta.hat  # On log scale
exp(A %*% beta.hat)  # On odds scale

# More carefully:
beta.hat <- matrix(beta.hat, ncol = 1)


# Standard error:
sqrt( A %*% V.hat %*% t(A) )

est <- as.vector(A %*% beta.hat)
se <- as.vector(sqrt( A %*% V.hat %*% t(A) ))

# 95% CI for A %*% beta = (beta2 - beta3) - log-odds scale
est + c(-1,1)*qnorm(.975)*se

# 95% CI for OR comparing B to C:
exp(est + c(-1,1)*qnorm(.975)*se)

### Exercise: 95% CI for OR comparing E to F, then A to F:
contrast.ci <- function(A, mod, conf.level = 0.95){
    # A = matrix to multiply by beta
    # Parameter of interest: exp(A %*% beta)
    # mod = glm object
    beta.hat <- coef(mod)
    V.hat <- vcov(mod)
    est <- as.vector(A %*% beta.hat)
    se <- as.vector(sqrt( A %*% V.hat %*% t(A) ))
    exp(est + c(-1,1)*(-qnorm((1-conf.level)/2))*se)
}

# E to F
contrast.ci(matrix(c(0,0,0,0,1,-1),nrow=1), mod1)

# A to F
contrast.ci(matrix(c(0,0,0,0,0,-1),nrow=1), mod1)
