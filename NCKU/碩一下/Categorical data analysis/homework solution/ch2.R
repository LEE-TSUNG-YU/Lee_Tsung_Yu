# Chapter 2.2.4
# test for difference of proportions,
# get Wald CI for difference of proportions
prop.test(c(189, 104), c(11034, 11037), conf.level=0.95, correct=FALSE)

library(PropCIs)
# score CI for difference of proportions
diffscoreci(189,11034,104,11037, conf.level = 0.95)
# score CI for relative risk
riskscoreci(189, 11034, 104, 11037, conf.level=0.95)

# Chapter 2.3.3
library(epitools)
oddsratio(c(189,10845,104,10933), method="wald", conf=0.95, correct=FALSE)
library(PropCIs)
orscoreci(189, 11034, 104, 11037, conf.level=0.95)

# Chapter 2.4.4
GenderGap <- matrix(c(495,272,590,330,265,498), ncol=3, byrow=TRUE)
chisq.test(GenderGap)
stdres <- chisq.test(GenderGap)$stdres # standardized residuals
stdres
library(vcd)
mosaic(GenderGap, gp=shading_Friendly, residuals=stdres,
       residuals_type="Std\nresiduals", labeling=labeling_residuals)

# Chapter 2.5.2
Malform <- matrix(c(17066, 14464, 788, 126, 37, 48, 38, 5, 1, 1), ncol=2)
library(vcdExtra)
CMHtest(Malform, rscores = c(0, 0.5, 1.5, 4.0, 7.0)) # row scores
sqrt(6.5699) # M test statistic
1- pnorm(2.56318) # one-sided standard normal P-value for M statistic

# Chapter 2.6.2
tea <- matrix(c(3,1,1,3), ncol=2)
# two-sided, alternative hypothesis: true odds ratio is not equal to 1
fisher.test(tea) 
# one-sided, alternative hypothesis: true odds ratio is greater than 1
fisher.test(tea, alternative="greater")

library(epitools)
# mid P-values for testing independence
ormidp.test(3, 1, 1, 3, or=1) # or: H0 value
# mid-P confidence interval for odds ratio
or.midp(c(3, 1, 1, 3), conf.level=0.95)$conf.int

# Chapter 2.6.6 Bayesian Inference
library(PropCIs)
# arguments are y1, n1, y2, n2, alpha1, beta1, alpha2, beta2, post. prob.
orci.bayes(11, 11, 0, 1, 0.5, 0.5, 0.5, 0.5, 0.95, nsim = 1000000)
# posterior interval for difference of proportions
diffci.bayes(11, 11, 0, 1, 0.5, 0.5, 0.5, 0.5, 0.95, nsim = 1000000)

# 2.17
data <- matrix(c(871,821,336,347,42,83), ncol=3, byrow=TRUE)
data
chisq.test(data, correct = F)
stdres <- chisq.test(data)$stdres # standardized residuals
stdres

data <- matrix(c(871,821,347,42), ncol=2, byrow=TRUE)
data
chisq.test(data, correct = F)

data <- matrix(c(1692,336,389,83), ncol=2, byrow=TRUE)
data
chisq.test(data, correct = F)

# 2.21
# (a)
data <- matrix(c(2,4,13,3,2,6,22,4,0,1,15,8,0,3,13,8), ncol=4, byrow=TRUE)
chisq.test(data, correct = F)
stdres <- chisq.test(data)$stdres # standardized residuals
stdres
# (b)
data <- matrix(c(2,4,13,3,2,6,22,4,0,1,15,8,0,3,13,8), ncol=4, byrow=TRUE)
library(vcdExtra)
CMHtest(data, rscores = c(3,10,20,35),
        cscores = c(1,3,4,5)) # row scores
sqrt(6.5699) # M test statistic
1- pnorm(2.56318) # one-sided standard normal P-value for M statistic
# 2.23
library(epitools)
# mid P-values for testing independence
ormidp.test(21, 2, 15, 3, or=1) # or: H0 value
# mid-P confidence interval for odds ratio
or.midp(c(21, 2, 15, 3), conf.level=0.95)$conf.int

# 2.29
# (a)
# arguments are y1, n1, y2, n2, alpha1, beta1, alpha2, beta2, post. prob.
diffci.bayes(1230, 1587, 859, 1272, 0.5, 0.5, 0.5, 0.5, 0.95, nsim = 1000000)
orci.bayes(1230, 1587, 859, 1272, 0.5, 0.5, 0.5, 0.5, 0.95, nsim = 1000000)
pi1 <- rbeta(10000, 1230.5, 357.5); pi2 <- rbeta(10000, 859.5, 413.5)
mean(pi1 > pi2)
