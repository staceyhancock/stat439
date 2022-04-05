### Lecture 15 - Multicategory logit models
### Multinomial Logistic Regression 
### Travel Example (continued)

# Source in Stats 439 R functions in order to use function summ.mfit and LinContr.mfit:
source("/Users/staceyhancock/Documents/Repos/staceyhancock/stat439/R/Stat439-RFunctions.R")

### Data: Greene (1995)
## Travel Choices:
# Measure travelers' decisions regarding travel mode
# between Sydney and Melbourne, Australia.
# Modes of travel (choices):
# mode = 0 --> air
# mode = 1 --> train
# mode = 2 --> bus
# mode = 3 --> car
## Explanatory variables of interest:
# hinc = household income ($k)
# psize = number of individuals traveling in a party

## Read in data:
travel <- read.table("https://math.montana.edu/shancock/data/TravelChoices.txt",
                     header=TRUE)
travel$mode <- factor(travel$mode,levels=c(0:3), labels=c("air","train","bus","car"))

## More practice interpreting interaction model:
library(nnet)
mfit.int <- multinom( mode ~ hinc * psize, data=travel)
summary(mfit.int)
summ.mfit(mfit.int)

## Estimated variance-covariance for estimated coefficients
vcov(mfit.int)

## 95% confidence interval for the RRR of train to air for a $1000
## increase in hinc for parties of 2:
co <- coef(mfit.int)
co
V <- vcov(mfit.int)
row.names(V)
est <- co[1,2] + 2*co[1,4]
est
se <- sqrt(V[2,2] + 4*V[4,4] + 4*V[2,4])
se
est + c(-1,1)*qnorm(.975)*se
exp(est + c(-1,1)*qnorm(.975)*se)

## Dan Gillen's R function for linear contrasts with multinomial models
## (need to source course R functions)
## To see what function is doing:
LinContr.mfit

## Set arguments and work through function line by line:
## To use function - contr.names need to match names of variance-covariance matrix, including quotes:
row.names(vcov(mfit.int))
contr.names <- c("train:hinc", "train:hinc:psize")
contr.coef <- c(1,2)
model <- mfit.int

## Learning what each line of the function is doing:
beta.hat <- as.vector( t( summary( model )$coefficients ) )
beta.hat
se <- as.vector( t( summary( model )$standard.errors ) )
se
cov.beta <- vcov( model )
cov.beta
contr.index <- is.element( dimnames( cov.beta )[[1]], contr.names )
contr.index
beta.hat <- beta.hat[ contr.index ]
beta.hat
cov.beta <- cov.beta[ contr.index,contr.index ]
cov.beta
est <- contr.coef %*% beta.hat
est
rrr.est <- exp( est )
rrr.est
se.est <- sqrt( contr.coef %*% cov.beta %*% contr.coef )
se.est
zStat <- est / se.est
zStat
pVal <- 2*pnorm( abs(zStat), lower.tail=FALSE )
pVal
ci95.lo <- exp( est - qnorm(.975)*se.est )
ci95.lo
ci95.hi <- exp( est + qnorm(.975)*se.est )
ci95.hi
cat( "\nTest of H_0: " )
for( i in 1:(length( contr.names )-1) ){
		cat( contr.coef[i], "*", contr.names[i], "+ " )
}
cat( contr.coef[i+1], "*", contr.names[i+1], "= 0 :\n\n" )
rslt <- data.frame( rrr.est, se.est, zStat, pVal, ci95.lo, ci95.hi )
round( rslt, 3 )

## Order of arguments: names of coefficients involved, linear contrast coefficients, fitted model object
LinContr.mfit(contr.names = c("train:hinc","train:hinc:psize"), contr.coef = c(1,2), mfit.int)

## RRR for odds of train to car for a $1000 increase in income for parties of size 2:
## Estimate:
co[1,2] + 2*co[1,4] - co[3,2] - 2*co[3,4]
exp(co[1,2] + 2*co[1,4] - co[3,2] - 2*co[3,4])

LinContr.mfit(contr.names = c("train:hinc", "train:hinc:psize", "car:hinc", "car:hinc:psize"), contr.coef = c(1, 2, -1, -2), mfit.int)

## Effect of party size on the odds of car to air for those making $50,000 per year?
LinContr.mfit(contr.names = c("car:psize","car:hinc:psize"), contr.coef = c(1,50), mfit.int)
## for those making $20,000 per year?
LinContr.mfit(contr.names = c("car:psize","car:hinc:psize"), contr.coef = c(1,20), mfit.int)

