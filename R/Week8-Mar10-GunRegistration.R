## Poisson regression with overdispersion Example

## Read in Gun Registration data by copying and pasting the following into R.
## Source: Albyn Jones
## http://people.reed.edu/~jones/141/Guns.html

## Variables: Socioeconomic data from 1990/1991 -
# pop = population of state (in 1000s of people)
# area = area of state (in 1000 square miles)
# urban = percent urban population
# poverty = percent below poverty line
# gunreg = whether there are gun registration laws or not
# homicides = number of homicides in the past year
"GunReg" <- 
structure(.Data = list(
"pop" = c(4089, 2372, 30380, 3291, 598, 13277, 1135, 2795, 11543, 5996, 
   4860, 9368, 4432, 5158, 6737, 635, 7760, 18058, 10939, 11961, 1004,
   3560, 4953, 17349, 1770, 5018, 570, 3750, 3377, 680, 6623,
   1039, 5610, 2495, 3713, 4252, 1235, 2592, 808, 1593, 1105,
   1548, 1284, 3175, 2922, 703, 6286, 567, 4955, 1801, 460.), 
"area" = c(52.4, 53.2, 163.7, 5.5, 0.1, 65.8, 10.9, 56.3, 57.9, 10.6, 
   12.4, 96.8, 86.9, 69.7, 53.8, 70.7, 8.7, 54.5, 44.8, 46.1, 1.5, 32, 
   42.1, 268.6, 84.9, 71.3, 656.4, 114, 104.1, 2.5, 59.4, 83.6, 36.4, 
   82.3, 40.4, 51.8, 35.4, 48.4, 147, 77.4, 9.4, 121.6, 110.6, 69.9, 
   98.4, 77.1, 42.8, 9.6, 65.5, 24.2, 97.8), 
"urban" = c(60, 54, 93, 79, 100, 85, 89, 61, 85, 84, 81, 70, 71, 53, 
   50, 53, 89, 84, 74, 69, 86, 55, 61, 80, 87, 76, 68, 88, 82,
   73, 63, 57, 65, 69, 52, 68, 45, 47, 53, 66, 51, 73, 88,
   68, 71, 50, 69, 32, 66, 36, 65.), 
"poverty" = c(19, 18.4, 14.2, 5.8, 19.2, 14.1, 10, 10.1, 13.3, 10.2, 
   9.3, 13.9, 12, 13.6, 13.2, 13.5, 9, 14.1, 11.8, 10.8, 8.2, 16.5, 
   16.9, 16.8, 9.8, 26.2, 11.2, 14.2, 12.1, 8.1, 16, 13.7, 14.1, 11.1, 
   17.4, 22, 12.5, 23.8, 15.8, 10.9, 7.1, 20.9, 10.7, 15.8, 11.3, 13.5, 
   10.6, 7.1, 9.2, 17.2, 10.6), 
"gunreg" = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.), 
"homicides" = c(410, 240, 3710, 170, 489, 1300, 44, 62, 1270, 200, 540, 
   1020, 100, 550, 730, 11, 350, 2550, 760, 740, 38, 350, 470, 2660,
   43, 220, 56, 290, 155, 32, 720, 21, 380, 150, 260, 760,
   23, 370, 29, 43, 32, 160, 135, 220, 120, 9, 550, 24, 240,
   135, 20.)), 
names = c("pop", "area", "urban", "poverty", "gunreg", "homicides"), 
row.names = c("AL", "AR", "CA", "CT", "DC", "FL", "HI", "IA", "IL", "MA", 
   "MD", "MI", "MN", "MO", "NC", "ND", "NJ", "NY", "OH", "PA", "RI", "SC", 
   "TN", "TX", "UT", "WA", "AK", "AZ", "CO", "DE", "GA", "ID", "IN", "KS", 
   "KY", "LA", "ME", "MS", "MT", "NE", "NH", "NM", "NV", "OK", "OR", "SD", 
   "VA", "VT", "WI", "WV", "WY"), class = "data.frame")

##
### Research question: Do gun registration laws affect the rate of homicides in a state?
##

##
### Exploratory data analysis
##

#### In class-----
summary(GunReg)
table(GunReg$gunreg)

library(yarrr)
pirateplot(homicides/pop ~ gunreg, inf.method="ci", theme=2, data=GunReg, xlab="Gun Registration Law", ylab="Rate of Homicides per 1000 people")

## Confounders?
plot(homicides/pop ~ urban,  data=GunReg, 
     xlab="Percent Urban", ylab="Rate of Homicides per 1000 people")
abline(lm(homicides/pop ~ urban, data=GunReg))
identify(GunReg$urban, GunReg$homicides/GunReg$pop)

GunReg[c(3,5,36),]

# Would also need to be associated with gunreg
pirateplot(urban ~ gunreg, inf.method="ci", theme=2, data=GunReg, 
           xlab="Gun Registration Law", ylab="Percent Urban")

## Both associated with homicide rate and gun reg laws --> urban is a confounding variable

library(car)
scatterplotMatrix(GunReg)

cor(GunReg)

# Ordered by population (smallest to largest)
GunReg[order(GunReg$pop),]
# Ordered by homicides (smallest to largest)
GunReg[order(GunReg$homicides),]

# Homicide rates (per 1,000 person-years):
GunReg$rate <- GunReg$homicides/GunReg$pop
summary(GunReg$rate)
GunReg[order(GunReg$rate),]
hist(GunReg$rate, breaks=50, col="blue", xlab="Homicide Rate per 1000 person-years")
# Large outlier - which state? DC

# Homicide rates stratifying by gun registration laws:
Y <- aggregate(homicides ~ gunreg, data = GunReg, sum)
T <- aggregate(pop ~ gunreg, data = GunReg, sum)
by.strata <- cbind(Y,T)[,-3]
by.strata$homrate <- by.strata$homicides/by.strata$pop
by.strata

boxplot(GunReg$rate ~ GunReg$gunreg, xlab="Gun registration laws", ylab="Homicide Rate per 1000 person-years")
identify(GunReg$gunreg+1, GunReg$rate, labels=row.names(GunReg))  # x-axis points = 1, 2
# Click on outliers on plot to identify; then type esc

##
### Modeling
##

# Poisson model addressing research question (note offset - why needed?):
mod1 <- glm(homicides ~ gunreg, family=poisson, offset=log(pop), data=GunReg)
summary(mod1)

# Something weirder
GunReg$x <- rnorm(51, 40, 5)
summary(glm(homicides ~ gunreg+x, family=poisson, offset=log(pop), data=GunReg))
# Note highly significant p-value for random noise predictor!

# Extremely large deviance compared to df
# GoF p-value =
pchisq(summary(mod1)$deviance, df=49, lower.tail=FALSE)

# Where is the lack of fit?
sort(residuals(mod1, type="deviance"))   

# Compare to:
GunReg$lrate <- log(GunReg$homicides/GunReg$pop)
mod.lm <- lm(lrate ~ gunreg, data=GunReg) 
summary(mod.lm)  ## Note low r-squared --> gun registration law indicator doesn't explain much of the variability among homicide rates!

### Why does mean(log(Y)) != log(mean(Y))

rdata <- rexp(51, rate = 1/0.086)  # Randomly generate observed rates
mean(log(rdata))
log(mean(rdata))


# Make sure you understand the inherant difference between
# this model and the Poisson regression model!
# Residual diagnostics for linear model:
par(mfrow=c(2,2))
plot(mod.lm)
# Possible problem with constant variance.
# Normality assumption looks ok.
# DC an outlier.

### Estimating overdispersion
res.pearson <- residuals(mod1, type = "pearson")

## Check Pearson residual calculation for AL
obs <- 410
pred <- exp(-2.5493 + 0.2532 + log(4089))
pres <- (obs - pred)/sqrt(pred)

## Mean of squared pearson resid = estimate of overdisp
mean(res.pearson^2)

plot(res.pearson^2 ~ fitted(mod1), xlab="Fitted Homicide Counts", ylab="Squared Pearson Residuals")
sfit <- supsmu(fitted(mod1), residuals(mod1, type="pearson")^2)
lines(sfit$x[ order(sfit$x) ], sfit$y[ order(sfit$x) ], col="red", lwd=2)
abline(h=1, lty=2, col="blue", lwd=2)

### --> Definitely overdisperson - est disp >> 1
## Quasi-Poisson
mod1.quasi <- glm(homicides ~ gunreg, family=quasipoisson, offset=log(pop), data=GunReg)
summary(mod1.quasi)

 
##### Start Here Tue Mar 10 #####  
# Control for other confounders:
mod2 <- glm(homicides ~ gunreg+poverty+urban, family=poisson, offset=log(pop), data=GunReg)
summary(mod2)
# Deviance still extremely large
sort(residuals(mod2, type="deviance"))
# Deviance GoF:
pchisq(3919.4, 47, lower.tail=FALSE)


### How well does the model really fit?
### Plot observed data and fitted model: Tricky since we have three predictors.
plot(rate ~ poverty, data=GunReg)
points(predict(mod2, type="response")/GunReg$pop ~ GunReg$poverty, pch=3, col="red")
legend("topleft", c("Observed Rate", "Fitted Rate"), pch=c(1,3), col=c(1,"red"))

## Overdispersion?
plot(residuals(mod2, type="pearson")^2 ~ fitted(mod2), xlab="Fitted Homicide Counts", ylab="Squared Pearson Residuals")
sfit <- supsmu(fitted(mod2), residuals(mod2, type="pearson")^2)
lines(sfit$x[ order(sfit$x) ], sfit$y[ order(sfit$x) ], col="red", lwd=2)
abline(h=1, lty=2, col="blue", lwd=2)
# Identify outliers
identify(fitted(mod2), residuals(mod2, type="pearson")^2, label=row.names(GunReg))

## Identify influential points
plot(cooks.distance(mod2) ~ hatvalues(mod2), ylab="Cook's Distance", xlab="Leverage")
text(hatvalues(mod2)-.015, cooks.distance(mod2), row.names(GunReg), cex=.7)
abline(h=qf(.5, length(mod2$coef), mod2$df.residual), col="blue")
abline(v=3*mean(hatvalues(mod2)), col="red")  
# Effect of WA on fit?
summary(mod2)$coef
summary(update(mod2, subset=-26))$coef

## Use squared Pearson residuals to estimate dispersion parameter phi:
presid2 <- residuals(mod2,type="pearson")^2
phihat <- sum(presid2)/mod2$df.residual   # 85.981
# We estimate that the variance is about 86 times larger
# than what we would expect under the Poisson distn.


### Quasi-Poisson Model to fit overdispersion
mod2.quasi <- glm(homicides ~ gunreg+poverty+urban, family=quasipoisson, offset=log(pop), data=GunReg)
summary(mod2.quasi)
# Notes: 
#  - estimated dispersion parameter matches estimate above
#  - std. errors = std. errors of mod2 multiplied by sqrt(85.981) - now standard errors are valid!
#  - Residual deviance is the same as mod2 --> need to divide by phihat for LRT

# Deviance GoF - need to divide by phihat
LRT.GoF <- mod2.quasi$deviance/phihat
pchisq(LRT.GoF, 47, lower.tail=FALSE)


# LRT test statistic needs to be adjusted by dividing by dispersion parameter:
anova(update(mod2.quasi, .~.-gunreg), mod2.quasi)
LRT.incorrect <- summary(update(mod2.quasi, .~.-gunreg))$deviance - summary(mod2.quasi)$deviance
## Diff in deviance = 67.048 --> differences in binomial likelihood
# Correct test statistic when allowing for overdispersion:
LRT.correct <- LRT.incorrect/phihat
# Use chi-squared with 1 df for p-value:
pchisq(LRT.correct, df=1, lower.tail=FALSE)

## Conclusion?
# No evidence that, when controlling for poverty and percent urban,
# gun registration laws have an effect on the homicide rate.

# But wait.... what about the outlier WA?
mod2.quasi.out <- update(mod2.quasi, subset=-26)
summary(mod2.quasi.out)
# Observation 26 (WA) still has a large influence on the results!
# Much lower estimate of the dispersion parameter if WA removed.
# gunreg coef now marginally significant.

#
## Alternative model: Fit negative binomial regression model:
#
library(MASS)
?glm.nb  # Uses log link by default
mod2.nb <- glm.nb(homicides ~ gunreg+poverty+urban+offset(log(pop)), data=GunReg)
summary(mod2.nb)
