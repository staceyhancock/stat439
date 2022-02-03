## Example Data

p1 <- 17/75
p2 <- 14/75

## Difference in proportions

theta_hat <- p1 - p2
se_theta_hat <- sqrt(p1*(1-p1)/75 + p2*(1-p2)/75)

## Interpret theta_hat = 0.04

# The difference (skier - snowboarder)
# in relative frequency of enrolling
# between those who received the skier brochure
# and those who received the snowboarding brochure
# is 0.04 in favor of the skier brochure.

# OR
# The proportion of students who enrolled in our sample
# was 0.04 higher for those who received the skier
# brochure, compared to those who received the snowboarder
# brochure.

# NOT... 4% higher

### Interpret SE(theta_hat) = 0.066
# - Measure of how variable theta_hat would be sample to sample.
# - Use for inference: since 0.066 is larger than 0.04, we have
# nearly no evidence of a significant difference in probability
# of enrolling between the two brochure types.
# - On average, the difference in sample proportion who enrolled
# between brochures among a random sample fo 150 potential students 
# should be about
# 0.07 away from the true difference.


### Relative risk
p1/p2
# 1.214
# Interpret: 
# (The chance of enrolling when given a skier brochure is
# The proportion who enrolled when given a skier brochure
# 21.4% higher
# than the proportion who enrolled when given a snowboarder brochure
# in the sample.

# The proportion who enrolled when given a skier brochure
# was 1.214 times the proportion who enrolled when given
# a snowboarder brochure.

### 95% CI for the true relative risk: theta = pi1/pi2
theta_hat <- p1/p2
theta_hat

# Go to the log scale:
log_theta_hat <- log(theta_hat)
log_theta_hat

# SE of log(theta_hat)
SE <- sqrt( (1-p1)/(75*p1) + (1-p2)/(75*p2) )
SE

# 95% CI for log(theta)
log_CI <- log_theta_hat + c(-1,1)*qnorm(0.975)*SE
log_CI

# 95% CI for theta
exp(log_CI)
# Convert to percent increase/decrease
(exp(log_CI)- 1)*100
# Interpret:
# We are 95% confident that the probability a student enrolls
# if given the skier brochure is between 35% lower to 128% higher
# than the probability if given the snowboarder.


### Odds ratio
sample_odds <- 17*61/(14*58)
sample_odds

# Interpret OR:
# The odds of enrolling when given a skier brochure are
# 27.7% higher
# than the odds of enrolling when given a snowboarder brochure
# in the sample.

# Interpret odds for skiers: 
17/58
# For every student who does not enroll when 
# given the skier brochure, 0.29 students enrolled.
