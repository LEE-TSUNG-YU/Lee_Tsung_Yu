# 3.2
# a.
data <- data.frame(percent = c(0.876,0.55,0.487,0.434,0.452,0.34,0.245,0.28,0.15,0.08,
                               0.031,0.024),
                   year = seq(0,11,1))
fit.lm <- lm(percent~year, data = data)
summary(fit.lm)
predict(fit.lm, data.frame(year = c(12)))

# b.
fit.lr <- glm(percent ~ year, family = "binomial", data = data)
summary(fit.lr)
logit <- function(x){ return (exp(x)/(1+exp(x))) }
predict(fit.lr, data.frame(year = c(12)), type = "response")

# 3.5
# a
data <- read.table("D:/Horseshoe Crab.txt", header = T)

fit.lm <- lm(y ~ weight, data = data)
summary(fit.lm)
predict(fit.lm, data.frame(weight = c(5.2)))

fit.lr <- glm(y ~ weight, family = "binomial", data = data)
summary(fit.lr)
# predict at x = 5.20
predict(fit.lr, data.frame(weight = c(5.2)), type = "response")
logit(predict(fit.lr, data.frame(weight = c(5.2))))

# 3.6
y <- c(5,18,19,25,7,7,2); n <- c(6,21,20,36,17,18,3)
x <- c(1,2,3,4,5,6,7)
fit <- glm(y/n ~ x, family=binomial(link=logit), weights=n)
summary(fit)
exp(confint(fit))

# 3.8
y <- c(24,35,21,30); n <- c(1379,638,213,254)
x <- c(0,2,4,5)
fit <- glm(y/n ~ x, family = binomial(link = logit), weights = n)
summary(fit)
confint(fit)
exp(confint(fit))

# 3.11
data <- data.frame(y = c(8, 7, 6, 6, 3, 4, 7, 2, 3, 4,
                           9, 9, 8,14, 8,13,11, 5, 7, 6),
                   x = c(rep(1,10),rep(0,10)))
fit.pr <- glm(y ~ x, family = poisson(link=log), data = data)
summary(fit.pr)
exp(fit.pr$coefficients[2])

# 3.13
data <- read.table("D:/Horseshoe Crab.txt", header = T)
fit.pr <- glm(sat ~ weight, family = poisson(link=log), data = data)
summary(fit.pr)
predict(fit.pr, data.frame(weight = c(2.44)), type = "response")


