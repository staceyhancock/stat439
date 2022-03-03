## Logistic Regression with Categorical Predictors

## Help file giving information on how to specify response variable
## for binomial glm family:
?family  # Look under "Details"

##### Berkeley admissions data

### Enter data as (ungrouped) binary observations:
## = each row is a single observational unit

# Indicator for sex: 1 = male; 0 = female
Sex <- c(rep(1,2681),rep(0,1835))

# Indicator for admission: 1 = admitted; 0 = denied
Admit <- c(rep(1,1195),rep(0,1486),rep(1,559),rep(0,1276))

# Program:
Program <- factor(c(rep("A",511),rep("B",352),rep("C",120),rep("D",137),rep("E",53),rep("F",22),
	rep("A",314),rep("B",208),rep("C",205),rep("D",270),rep("E",138),rep("F",351),
	rep("A",89),rep("B",17),rep("C",202),rep("D",132),rep("E",95),rep("F",24),
	rep("A",19),rep("B",8),rep("C",391),rep("D",243),rep("E",298),rep("F",317)),
	levels = c("A","B","C","D","E","F"), ordered = FALSE)
	
Berkeley <- data.frame(Sex, Admit, Program)
head(Berkeley)  # Each row = one individual
tail(Berkeley)

## Practice with dummy variables
mod0 <- glm(Admit ~ Program, family = binomial, data = Berkeley)
summary(mod0)

# Relevel so that baseline is Program E
mod0_E <- glm(Admit ~ relevel(Program, "E"),
              family = binomial, data = Berkeley)

Berkeley$Program_RelevelE <- relevel(Berkeley$Program, "E")
mod0_E <- glm(Admit ~ Program_RelevelE,
              family = binomial, data = Berkeley)

###

mod1 <- glm(Admit ~ Sex*Program, family=binomial, data=Berkeley)
summary(mod1)

mod2 <- glm(Admit ~ Sex+Program, family=binomial, data=Berkeley)
summary(mod2)

# Likelihood Ratio Test to compare the two models:
anova(mod2, mod1, test="LRT")


### Enter data as grouped binomial data:
Berk.grp <- data.frame(Sex = rep(c("Male","Female"),each=6), 
						Program = rep(c("A","B","C","D","E","F"),2),
						Admit = c(511,352,120,137,53,22,
								89,17,202,132,95,24),
						Deny = c(314,208,205,270,138,351,
								19,8,391,243,298,317))
Berk.grp

mod1.grp <- glm(cbind(Admit,Deny) ~ Sex*Program, family=binomial, data=Berk.grp)	
summary(mod1.grp)
# Note the identical coefficient estimates to model fit to binary data.
# But different deviance. Residual deviance = 0; why?							

mod2.grp <- update(mod1.grp, .~.-Sex:Program)	
summary(mod2.grp)

# But same differences in deviances between the two models:
anova(mod2.grp, mod1.grp, test="LRT")	

# Null deviance
mod0.grp <- glm(cbind(Admit,Deny) ~ 1, family = binomial, data = Berk.grp)

### LRT H0: mod2.grp (additive), Ha: mod1.grp (interaction)
dev0 <- mod2.grp$deviance
deva <- mod1.grp$deviance

# Test statistic
G2 <- dev0 - deva
# p-value
pchisq(G2, df = 5, lower.tail=FALSE)


### Relationship to likelihood ratio chi-square statistic
### when testing independence from Ch. 2:
Berk.grp2 <- data.frame(Sex = c("Male","Female"), Admit = c(1195,559), Deny = c(1486,1276))	
mod0.grp2 <- glm(cbind(Admit,Deny) ~ Sex, family=binomial, data=Berk.grp2)
summary(mod0.grp2)  # Compare coef. of SexMale to sample log(OR)
mod0.grp2$fitted   # Compare to conditional row percentages in sample
anova(mod0.grp2, test = "LRT")

Berk.table <- matrix(c(1195,1486,559,1276),2,2,byrow=TRUE, dimnames=list(Sex = c("Male","Female"), Admission=c("Admit","Deny")))
## Expected counts under independence:
ex <- chisq.test(Berk.table)$expected
## G^2 test statistic = null deviance in mod0.grp2
G2 <- 2*sum(Berk.table*log(Berk.table/ex))
					