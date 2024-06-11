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

