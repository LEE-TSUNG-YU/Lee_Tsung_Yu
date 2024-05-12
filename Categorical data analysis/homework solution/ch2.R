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

# 2.17
data <- matrix(c(871,821,336,347,42,83), ncol=3, byrow=TRUE)
data
chisq.test(data)
stdres <- chisq.test(data)$stdres # standardized residuals
stdres

data <- matrix(c(871,821,347,42), ncol=2, byrow=TRUE)
data
chisq.test(data)

data <- matrix(c(1692,336,389,83), ncol=2, byrow=TRUE)
data
chisq.test(data)
