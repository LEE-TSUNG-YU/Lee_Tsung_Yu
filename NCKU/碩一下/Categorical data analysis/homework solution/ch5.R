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
stepAIC(fit)

# 5.7

# 5.9

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

