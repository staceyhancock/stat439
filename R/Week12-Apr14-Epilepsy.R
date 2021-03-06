##
### GLMMs and GEEs with Epilepsy Data
##

##### Load Required Libraries #####
library(tidyverse)  # ggplot2, tidyr
library(lme4)    # Functions: glmer
library(gee) # Functions: gee

##
#### Read in data set into R:
##
epilepsy <- read.table("http://www.hsph.harvard.edu/fitzmaur/ala2e/epilepsy.dat", 
                       header=FALSE)
names(epilepsy) <-	c("ID","trt","age","Week0","Week2","Week4","Week6","Week8")
epilepsy$trt <- factor(epilepsy$trt, levels=c(0,1), labels=c("Placebo","Progabide"))

## Convert to long form:
epi_long <- pivot_longer(epilepsy,
                         cols = 4:8,
                         names_to = "Time", names_prefix = "Week",
                         values_to = "Count")
epi_long$Time <- as.numeric(epi_long$Time)
  
## Create new variables
epi_long <- epi_long %>% mutate(
  PostBase = as.numeric(Time != 0),
  Weeks = 8*(PostBase==0) + 2*(PostBase==1)
)


##### Exploratory Data Analysis #####

## Number of seizures by group by time period:
boxplot(Count ~ trt+Time, at=c(1,2,4,5,7,8,10,11,13,14), data=epi_long, col=c("red","blue"), ylab="Number of Seizures", xlab="", names=rep(c("Plac","Prog"),5), main="Number of Seizures by Group and Week")
mtext( c("Baseline",paste("Week",c(2,4,6,8))), side=1, at=c(1.5,4.5,7.5,10.5,13.5), line=3 )

# Calculate rates per week:
epi_long <- epi_long %>% mutate(
  Rate = case_when(
    Time == 0 ~ Count/8,
    Time != 0 ~ Count/2
  )
)

# Plot of mean rates:
means <- tapply(epi_long$Rate, list(epi_long$Time,epi_long$trt), mean)
matplot(matrix(c(0,2,4,6,8)), means,
        col=c(1,1), lty=c(3,1), type="o",
        pch=c(1,16), xlab="Time (weeks)",
        ylab="Mean rate of seizures (per week)",
        ylim=c(2.5,5.0),
        main="Mean Rate of Seizures by Treatment Group")
legend(3.5, 3.0, c("Placebo","Progabide"), lty=c(3,1))

## Plots of individual counts:
matplot(matrix(c(0,2,4,6,8)), t(epilepsy[,4:8]),
	col=(as.numeric(epilepsy$trt=="Placebo")+2), type="l", ylim = c(0,180),
	xlab="Week",ylab="No. of Seizures ", main="Response Profiles by ID and Treatment")
legend(5,150,c("Placebo","Progabide"),col=c(2,3),lty=c(1,1))

# Who is the outlier at baseline in the Progabide group?
plot(Count[trt=="Progabide"] ~ Time[trt=="Progabide"],
     xlab="Week", ylab="Seizures",
     main="Counts of Seizures for Progabide Group", col="blue",
     data=epi_long)
points(Count[trt=="Placebo"] ~ Time[trt=="Placebo"], pch=2, 
       col="red", data=epi_long)
identify(epi_long$Count ~ epi_long$Time, labels=epi_long$ID)
# ID 49 data:
epi_long[epi_long$ID==49,]

# This patient could have a large impact on the analysis -
# Book does analysis with and without ID 49.


##### Fitting GLMMs #####
# Since the number of weeks each count refers to differs
# (8 weeks for baseline by 2 weeks afterwards)
# we need to include an "offset" --> Model mean rate per week

# With glmer (and lmer) function, random effects specified in parentheses:
?lmer
?glmer

## Fit GLMM
mod1 <- glmer(Count ~ trt*PostBase + (PostBase | ID), offset=log(Weeks),
              family=poisson, data=epi_long)
summary(mod1)

# Estimated random effects
ranef(mod1)
# Subject-specific coefficients (beta_k + b_ki)
coef(mod1)

# Treating time as quantitative:
mod2 <- glmer(Count ~ trt*Time + (Time | ID), offset=log(Weeks),
	            family=poisson, data=epi_long)
summary(mod2)
# How does the interpretation of coefficients change?
# Note larger AIC


##### Fitting Marginal Models #####
?gee
mod.gee <- gee(Count ~ trt*PostBase + offset(log(Weeks)),
               id = ID, family = poisson(link = "log"), 
               corstr = "exchangeable", data = epi_long)

# corstr="exchangeable" --> compound symmetry covariance structure.
# family=poisson --> Poisson variance function (not distribution)
summary(mod.gee)

mod.gee2 <- gee(Count ~ trt*PostBase + offset(log(Weeks)),
                id = ID, family = poisson(link = "log"), 
                corstr = "AR-M", data = epi_long)
summary(mod.gee2)
# Note similar coefficient estimates and Wald tests
# GEE std. errs robust to covariance structure assumption
