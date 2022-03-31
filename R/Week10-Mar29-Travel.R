### Multicategory logit models
### Multinomial Logistic Regression
library(tidyverse)
### Data: Greene (1995)
## Travel Choices:
# Measure travelers' decisions regarding travel mode
# between Sydney and Melbourne, Australia.
# Modes of travel (choices):
# mode = 0 --> air
# mode = 1 --> train
# mode = 2 --> bus
# mode = 3 --> car
# (Note that this is a nominal response variable -
# the numbers have no quantitative meaning)

## Explanatory variables of interest:
# hinc = household income ($k)
# psize = number of individuals traveling in a party
# Can you think of other unmeasured confounders?

## Read in data and attach:
travel <- read.table("https://math.montana.edu/shancock/data/TravelChoices.txt",
                     header=TRUE)
travel$mode.fac <- factor(travel$mode, levels=c(0:3), 
                          labels=c("air","train","bus","car"))

attach(travel)
head(travel)
dim(travel)
# Sample size = 210

## Some descriptive statistics:

# What proportion are in each travel category?
cbind(table(mode), table(mode)/sum(table(mode)))
# --> travel by train most common (30%), then
# car (28%)
# air (27.6%)
# bus (14.2%)

travel %>% ggplot(aes(x = mode)) +
  geom_bar()

# Travel mode vs. party size?
table(mode.fac, psize)
# As psize increases, probability of traveling by air decreases.
# Note sparse data for large party sizes (only one observation with psize = 6)

travel %>% ggplot(aes(x = mode.fac, y = hinc)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, width = .15, height = 0)

travel %>% ggplot(aes(x = mode.fac, y = psize)) +
  geom_violin()

travel %>% ggplot(aes(x = mode.fac, y = psize)) +
  geom_pirate()

library(yarrr)
pirateplot(psize ~ mode.fac, data = travel)

# mosaic(x = travel$psize, y = travel$mode.fac) - figure that out later

travel %>% ggplot(aes(x = psize, fill = mode.fac)) + 
  geom_bar(identity = "stat") # to do later

# Travel mode vs. hinc?
boxplot(hinc ~ mode.fac, ylab="Household Income",)
lapply( split(hinc,mode), FUN=summary)
# Median household income among travelers choosing air or car appears
# to be higher than those traveling by train or bus.

## Fit multinomial logistic regression model
# Use multinom function in nnet library

library(nnet)
mfit <- multinom( mode.fac ~ hinc + psize, data=travel)
summary(mfit) # Not too helpful

# Source in Stats 439 R functions in order to use function summ.mfit:
source("/Users/staceyhancock/Documents/Repos/staceyhancock/stat439/R/Stat439-RFunctions.R")

summ.mfit(mfit)
# rrr = "relative risk ratio" = est. odds of level ___ to level ___
# Interpretations of rrr for psize in each equation?

# Practice: psize coefficient in car vs air (exponentiated) = 1.823
# If the party size increases by one individual,
# the [relative risk ratio]/[conditional odds ratio] of travel
# by car compared to air increases by 82.4%

# Plot fitted probabilities vs. hinc for given party size:
co <- coef(mfit) # Note is a 3x3 matrix
is.matrix(co)
dim(co)

# For party size = 2
ps <- 2
# Probability of air:
curve(1/(1+exp(co[1,1]+co[1,2]*x+co[1,3]*ps) +
				exp(co[2,1]+co[2,2]*x+co[2,3]*ps) +
				exp(co[3,1]+co[3,2]*x+co[3,3]*ps)), from=0, to=80,
	xlab="Household Income ($k)", ylab="Fitted Probabilities",
	main="Fitted Probabilities of Travel Type for Parties of Size 2",
	col="red", lty=1, ylim=c(0,1), lwd=2)
# Probability of train:
curve(exp(co[1,1]+co[1,2]*x+co[1,3]*ps)/
			(1+exp(co[1,1]+co[1,2]*x+co[1,3]*ps) +
				exp(co[2,1]+co[2,2]*x+co[2,3]*ps) +
				exp(co[3,1]+co[3,2]*x+co[3,3]*ps)), add=T,
				col="blue",lty=2, lwd=2)
# Probability of bus:
curve(exp(co[2,1]+co[2,2]*x+co[2,3]*ps)/
			(1+exp(co[1,1]+co[1,2]*x+co[1,3]*ps) +
				exp(co[2,1]+co[2,2]*x+co[2,3]*ps) +
				exp(co[3,1]+co[3,2]*x+co[3,3]*ps)), add=T,
				col="darkgreen",lty=3, lwd=2)
# Probability of car:
curve(exp(co[3,1]+co[3,2]*x+co[3,3]*ps)/
			(1+exp(co[1,1]+co[1,2]*x+co[1,3]*ps) +
				exp(co[2,1]+co[2,2]*x+co[2,3]*ps) +
				exp(co[3,1]+co[3,2]*x+co[3,3]*ps)), add=T,
				col="purple",lty=4, lwd=2)
legend(50,.9,c("Air","Train","Bus","Car"),
	col=c("red","blue","darkgreen","purple"),
	lty=c(1,2,3,4))

# Any interaction between psize and hinc on mode?
mfit.int <- multinom( mode.fac ~ hinc * psize, data=travel)
summ.mfit(mfit.int)

anova(mfit, mfit.int)
# Conclusion?

### Fitting separate logistic models to each (less efficient):
# Compare to multinomial coefs.
# Train vs. Air:
mod.TA <- glm( (mode==1) ~ hinc+psize, data=travel, family=binomial,
			subset = (mode==0 | mode==1))
# The logical argument "mode==0 | mode==1" tells R to only take observations
# from the data set that have mode 0 OR (|) mode 1.
glmCI(mod.TA)

# Bus vs. Air:
mod.BA <- glm( (mode==2) ~ hinc+psize, data=travel, family=binomial,
			subset = (mode==0 | mode==2))
glmCI(mod.BA)

# Car vs. Air:
mod.CA <- glm( (mode==3) ~ hinc+psize, data=travel, family=binomial,
			subset = (mode==0 | mode==3))
glmCI(mod.CA)

## Only the intercept term for Bus vs. Air seems to differ from
## the multinomial fit.

detach(travel)