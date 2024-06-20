# Chapter 5
# 5.1.2
Crabs <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Crabs.dat", header = T)
fit <- glm(y ~ weight + width + factor(color) + factor(spine),
           family=binomial, data=Crabs)
summary(fit)
1- pchisq(225.76-185.20, 172-165) # P-value for test that all beta's = 0
library(car)
Anova(fit) # likelihood-ratio tests for individual explanatory variables
# 5.1.6
fit <- glm(y ~ width + factor(color), family=binomial, data=Crabs)
-2*logLik(fit)
AIC(fit) # adds 2(number of parameters) = 2(5) = 10 to-2*logLik(fit)

fit <- glm(y ~ weight + width + factor(color) + factor(spine),
           family=binomial, data=Crabs)
library(MASS)
stepAIC(fit) # stepwise backward selection using AIC
attach(Crabs) # need response variable in last column of data file
Crabs2 <- data.frame(weight, width, color, spine, y) # y in last column
library(leaps)
library(bestglm)
bestglm(Crabs2, family=binomial, IC="AIC") # can also use IC="BIC"

# 5.2.2
Marijuana <- data.frame(race = c("white", "white", "other", "other"),
                        gender = c("female", "male", "female", "male"),
                        yes = c(420, 483, 25, 32),
                        no = c(620, 579, 55, 62))
fit <- glm(yes/(yes+no) ~ gender + race, weights=yes+no,
           family=binomial,data=Marijuana)
fit$deviance # residual deviance goodness-of-fit statistic
fit$df.residual # residual df
1- pchisq(fit$deviance, fit$df.residual) # P-value for deviance goodness-of-fit test
fitted(fit) # estimated probabilities of marijuana use

attach(Marijuana)
n <- yes+no
fit.yes <- n*fitted(fit); fit.no <- n*(1- fitted(fit))
data.frame(race, gender, yes, fit.yes, no, fit.no)
summary(fit)
cbind(rstandard(fit,type="pearson"), residuals(fit,type="pearson"),
      residuals(fit,type="deviance"), rstandard(fit,type="deviance"))

# 5.1
# a.
Crabs <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Crabs.dat", header = T)
fit <- glm(y ~ weight + width, family = binomial, data = Crabs)
summary(fit)
# b.
fit1 <- glm(y ~ weight, family = binomial, data = Crabs)
fit2 <- glm(y ~ width, family = binomial, data = Crabs)
anova(fit1, fit, test = "LRT")
anova(fit2, fit, test = "LRT")
# c.
library(MASS)
fit <- glm(y ~ weight + factor(color) + factor(spine), family = binomial, data = Crabs)
stepAIC(fit)

# 5.3
Crabs2 <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Crabs2.dat", header = T)
Crabs2$Year <- Crabs2$Year - 1993
fit <- glm(y ~ ., family = binomial, data = Crabs2)
summary(fit)
stepAIC(fit)

# 5.7
AIDS <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/AIDS.dat", header = T)
fit <- glm(yes/(yes+no) ~ azt+ race, weights=yes+no, family=binomial, 
           data=AIDS)
summary(fit)
1-pchisq(fit$deviance, fit$df.residual)
# since the p-value is 0.2395 > 0.05, the model fits adequately.

# 5.9
# a.
data <- data.frame(department = c(1:6),
                   yes = c(601, 370, 322, 269, 147, 46),
                   no = c(332, 215, 596, 523, 437, 668))
fit <- glm(yes/(yes+no) ~ factor(department),
           weights = (yes+no), family = binomial, data = data)
summary(fit)
# b.
data <- data.frame(department = rep(c(1:6),each = 2),
                   gender = rep(c("male", "female"),6),
                   yes = c(512, 89, 353, 17, 120, 202,
                           138, 131, 53, 94, 22, 24),
                   no = c(313, 19, 207, 8, 205, 391,
                          279, 244, 138, 299, 351, 317))
fit <- glm(yes/(yes+no) ~ factor(department) + factor(gender),
           weights = (yes+no), family = binomial, data = data)
summary(fit)
# gender effect
exp(-0.09987) # 0.9049551 for male.

# marginal table collapsed over department.
data <- matrix(data = c(1198, 1493, 557, 1278),
               nrow = 2, byrow = T, dimnames = list(c("Yes","No"),c("male","female")))
data
# odds ratio
(1198*1278)/(1493*557) # 1.84108

# 5.15
data <- data.frame(Center = rep(c(1:5),each = 2),
                   treatment = rep(c("Drug","Placebo"),5),
                   case = c(5,9,13,10,7,5,9,8,14,14),
                   y = c(0,0,1,0,0,0,6,2,5,2))
data$treatment <- relevel(as.factor(data$treatment), ref = "Placebo")

fit <- glm(y/case ~ factor(Center) + factor(treatment), weights = case, family = binomial, data = data)
summary(fit)

# b.
fit <- glm(y/case ~ factor(Center) + factor(treatment)-1, weights = case, family = binomial, data = data)
summary(fit)

# c.
data <- data.frame(Center = rep(c(2,4,5),each = 2),
                   treatment = rep(c("Drug","Placebo"),3),
                   case = c(13,10,9,8,14,14),
                   y = c(1,0,6,2,5,2))
data$treatment <- relevel(as.factor(data$treatment), ref = "Placebo")
fit <- glm(y/case ~ factor(Center) + factor(treatment)-1, weights = case, family = binomial, data = data)
summary(fit)

# 5.17
sorethroat <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/SoreThroat.dat", header = T)
fit1 <- lm(Y ~ D+T, data = sorethroat)
summary(fit1)
fit2 <- glm(Y ~ D+T, family = binomial, data = sorethroat)
summary(fit2)

# 5.19
sample.size <- function(alpha, beta, p1, p2){
  nominator <- (qnorm(alpha/2) + qnorm(beta))^2*(p1*(1-p1)+p2*(1-p2))
  denominator <- (p1-p2)^2
  return(nominator/denominator)
}
# a.
sample.size(alpha = 0.1, beta = 0.2, p1 = 0.2, p2 = 0.3) # 228.7546
# b.
sample.size(alpha = 0.1, beta = 0.1, p1 = 0.2, p2 = 0.3) # 316.8624
sample.size(alpha = 0.05, beta = 0.2, p1 = 0.2, p2 = 0.3) # 290.4086
