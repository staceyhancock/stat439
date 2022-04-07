library(VGAM)  # vglm
library(MASS)  # polr, housing

######## PO Model with quantitative predictor
# Mental Impairment Data - Example 6.3.4 p. 178

dat <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Mental.dat", header=T)
# impair: 1 = well, 2 = mild, 3 = moderate, 4 = impaired (response)
# ses: 1 = high; 0 = low
# life: "life events index" - composite measure of the number and severity
#       of important life events within the past three years

dat$impair <- as.ordered(dat$impair)
levels(dat$impair) <- c("well", "mild", "mod", "imp")
dat$ses <- as.factor(dat$ses)
levels(dat$ses) <- c("low", "high")

# Use polr function in MASS library: Response must be ordered factor.
?polr
fit1 <- polr(impair ~ life, data = dat)
summary(fit1)

# Beta estimate:
b <- -fit1$coef
# Alpha estimates:
a <- fit1$zeta

# Plot cumulative probabilities:
cols <- c("darkgreen","darkblue","purple","red")
curve(1/(1+exp(-(a[1]+b*x))), 0, 10,
      ylim=c(0,1), col=cols[1], lty=2,
      main = "Cumulative Probabilities",
      xlab="Life Events",
      ylab="P(Y<=j)")
curve(1/(1+exp(-(a[2]+b*x))), add=T, col=cols[2], lty=3)
curve(1/(1+exp(-(a[3]+b*x))), add=T, col=cols[3], lty=4)
legend(7, .9, c("Well","Mild","Mod"), col=cols[1:3], lty=c(2:4))

# Plot individual probabilities:
curve(1/(1+exp(-(a[1]+b*x))), 0, 10,
      ylim=c(0,1), col=cols[1], lty=2,
      main="Category Probabilities", 
      xlab="Life Events",
      ylab="P(Y=j)")
curve(1/(1+exp(-(a[2]+b*x)))-1/(1+exp(-(a[1]+b*x))) ,add=T, col=cols[2], lty=3)
curve(1/(1+exp(-(a[3]+b*x)))-1/(1+exp(-(a[2]+b*x))), add=T, col=cols[3], lty=4)
curve(1-1/(1+exp(-(a[3]+b*x))), add=T, col=cols[4], lty=5)
legend(7, .9, c("Well","Mild","Mod","Impaired"), col=cols, lty=c(2:5))


fit2 <- polr(impair ~ ses + life, data = dat)
summary(fit2)
# Beta estimates:
b2 <- -fit2$coef
# Alpha estimates:
a2 <- fit2$zeta

# Plot cumulative probabilities:
cols <- c("darkgreen","darkblue","purple","red")
# ses = low
curve(1/(1+exp(-(a2[1]+b2[2]*x))), 0, 10,
      ylim=c(0,1), col=cols[1], lty=1,
      main = "Cumulative Probabilities",
      xlab="Life Events",
      ylab="P(Y<=j)")
curve(1/(1+exp(-(a2[2]+b2[2]*x))), add=T, col=cols[2], lty=1)
curve(1/(1+exp(-(a2[3]+b2[2]*x))), add=T, col=cols[3], lty=1)
# ses = high
curve(1/(1+exp(-(a2[1]+b2[1]+b2[2]*x))), add=T, col=cols[1], lty=2)
curve(1/(1+exp(-(a2[2]+b2[1]+b2[2]*x))), add=T, col=cols[2], lty=2)
curve(1/(1+exp(-(a2[3]+b2[1]+b2[2]*x))), add=T, col=cols[3], lty=2)

legend(8, .95, c("Well","Mild","Mod"), fill=cols[1:3])
legend(0, .1, c("High SES","Low SES"), lty=c(2,1))

anova(fit2, fit1)

######## PO Model with categorical predictor
# Built-in data set of 1,681 renters from a housing satisfaction survey
# in Copenhagen
# Variables:
# Sat = subject's level of satisfaction with housing conditions
# Infl = subject's feeling of influence on apartment management
# Type = type of rental accommodation occupied
# Cont = degree of contact subject had with other residents
# Freq = cell count
data(housing)  # Loads data set
attach(housing)

# Note that Sat is an ordered factor:
Sat

# Marginal tables of each explanatory variable vs. Sat:
Infl.tab <- xtabs(Freq ~ Infl+Sat)
Type.tab <- xtabs(Freq ~ Type+Sat)
Cont.tab <- xtabs(Freq ~ Cont+Sat)
# All marginal associations seem strong:
chisq.test(Infl.tab)
chisq.test(Type.tab)
chisq.test(Cont.tab)

# Re-arrange data:
Sat.Low <- housing[Sat=="Low",c(2:5)]
Sat.Med <- housing[Sat=="Medium",5]
Sat.High <- housing[Sat=="High",5]

housing.new <- data.frame(Sat.Low,Sat.Med,Sat.High)
names(housing.new) <- c("Infl","Type","Cont","Low","Med","High")

# Treat Sat as response -
# Fit proportional odds model using Infl as explanatory variable.
# Use vglm function in VGAM library:
mod1 <- vglm(cbind(Low,Med,High) ~ Infl, family = cumulative(parallel=TRUE), data = housing.new)
