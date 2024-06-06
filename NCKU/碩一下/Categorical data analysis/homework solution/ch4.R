
# Example 4.1.3
Crabs <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Crabs.dat", header = T)
plot(jitter(y, 0.08) ~ width, data = Crabs)
library(gam)
gam.fit <-gam(y ~ s(width), family=binomial, data=Crabs)#s =smooth function.
# plots generalized additive model smoothing fit
curve(predict(gam.fit, data.frame(width=x), type="resp"), add=TRUE)
fit <-glm(y ~width, family=binomial, data=Crabs) #link=logit is default
# logistic regression fit is added to the plot
curve(predict(fit, data.frame(width=x), type="resp"), add=TRUE)
summary(fit)
predict(fit, data.frame(width = 21.0), type="response") # estimated probability of satellite at width =21.0

predict(fit, data.frame(width = mean(Crabs$width)), type="response") # estimated probability of satellite at mean width

# Chapter 4.2.1
Crabs <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Crabs.dat", header = T)
fit <- glm(y ~ width, family=binomial, data=Crabs)
summary(fit)
confint(fit) # profile likelihood confidence interval
library(car)
Anova(fit) # likelihood-ratio test of width effect

# Chapter 4.2.3
Crabs <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Crabs.dat", header = T)
fit <- glm(y ~ width, family=binomial, data=Crabs)
pred.prob <- fitted(fit) # ML fitted value estimate of P(Y=1)
lp <- predict(fit, se.fit=TRUE) # linear predictor
LB <- lp$fit- 1.96*lp$se.fit # confidence bounds for linear predictor
UB <- lp$fit + 1.96*lp$se.fit # better: use qnorm(0.975) instead of 1.96
LB.p <- exp(LB)/(1 + exp(LB)) # confidence bounds for P(Y=1)
UB.p <- exp(UB)/(1 + exp(UB))
cbind(Crabs$width, pred.prob, LB.p, UB.p)
plot(jitter(y,0.1) ~ width, xlim=c(18,34), pch=16, ylab="Prob(satellite)", data=Crabs)
data.plot <- data.frame(width=(18:34))
lp <- predict(fit, newdata=data.plot, se.fit=TRUE)
pred.prob <- exp(lp$fit)/(1 + exp(lp$fit))
LB <- lp$fit- qnorm(0.975)*lp$se.fit
UB <- lp$fit + qnorm(0.975)*lp$se.fit
LB.p <- exp(LB)/(1 + exp(LB)); UB.p <- exp(UB)/(1 + exp(UB))
lines(18:34, pred.prob)
lines(18:34, LB.p, col="red"); lines(18:34, UB.p, col="blue")

# Example 4.3.2
Marijuana <- data.frame(race = c("white", "white", "other", "other"),
                        gender = c("female", "male", "female", "male"),
                        yes = c(420, 483, 25, 32),
                        no = c(620, 579, 55, 62))
fit <- glm(yes/(yes+no) ~ gender + race, weights = yes + no, family=binomial, data=Marijuana)
summary(fit)
library(car)
Anova(glm(yes/(yes+no) ~ gender + race, weights=yes+no, family=binomial, data=Marijuana))

# 4.4.1 example
Crabs <- read.table("C:/github_LTY/Lee_Tsung_Yu/Categorical data analysis/dataset/Crabs.dat.dat", header = T)
fit <- glm(y ~ width + factor(color), family=binomial, data=Crabs)
summary(fit)

summary(glm(y ~ width, family=binomial, data=Crabs))
library(car)
Anova(glm(y ~ width + factor(color), family=binomial, data=Crabs))
fit2 <- glm(y ~ width + color, family=binomial, data=Crabs)
summary(fit2) # color treated as quantitative with scores (1, 2, 3, 4)
anova(fit2, fit, test="LRT") # likelihood-ratio test comparing models

Crabs$c4 <- ifelse(Crabs$color == 4, 1, 0) # indicator for color cat. 4
fit3 <- glm(y ~ width + c4, family=binomial, data=Crabs)
summary(fit3)
anova(fit3, fit, test="LRT") # likelihood-ratio test comparing models

glm(y ~ width + c4 + width:c4, family=binomial, data=Crabs)

# Chapter 4.5.1
fit3 <- glm(y ~ width + c4, family=binomial, data=Crabs)
predict(fit3, data.frame(c4=1, width=mean(Crabs$width)), type="response")
predict(fit3, data.frame(c4=0, width=mean(Crabs$width)), type="response")
predict(fit3,data.frame(c4=mean(c4),width=quantile(Crabs$width)), type="resp")

fit3 <- glm(y ~ width + c4, family=binomial, data=Crabs)
library(mfx)
logitmfx(fit3, atmean=FALSE, data=Crabs) # with atmean=TRUE, finds effect only at the mean

# Chapter 4.6.1
prop <- sum(Crabs$y)/nrow(Crabs) # sample proportion of 1's for y variable
prop
fit <- glm(y ~ width + factor(color), family=binomial, data=Crabs)
predicted <- as.numeric(fitted(fit) > prop) # predict y=1 when est.> 0.6416
xtabs(~ Crabs$y + predicted) # Classification table with sample proportion cutoff

# Chapter 4.6.2
fit <- glm(y ~ width + factor(color), family=binomial, data=Crabs)
library(pROC)
rocplot <- roc(y ~ fitted(fit), data=Crabs)
plot.roc(rocplot, legacy.axes=TRUE) # Specficity on x axis if legacy.axes=F
auc(rocplot) # auc = area under ROC curve = concordance index

# 4.1
LI <-c(8,8,10,10,12,12,12,14,14,14,16,16,16,18,20,20,20,22,22,24,26,28,32,34,38,38,38)
y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,1,0,1,1,1,0)
data <- data.frame(LI = LI, y = y)
fit <- glm(y ~ LI, family = binomial, data = data)
summary(fit)
# a.
predict(fit, data.frame(LI = c(26)), type = "response")
# c.
predict(fit, data.frame(LI = c(12,26)), type = "response")
predict(fit, data.frame(LI = c(8,38)), type = "response")
# d.
predict(fit, data.frame(LI = c(8)), type = "response")
# e.
library(mfx)
logitmfx(fit, atmean = F, data = data)

# 4.3
remission <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Remission.dat", header = T)
fit2 <- glm(remissions/cases ~ LI,family = "binomial",weights = cases, data = remission)
summary(fit2)

# 4.5
Shuttle <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Shuttle.dat", header = T)
# (a)
fit <- glm(TD~Temp, family = "binomial", data = Shuttle)
summary(fit)
# (b)
predict(fit, data.frame(Temp = c(31)), type = "response")
# (c)
Anova(fit)

# 4.7
Kyphosis <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Kyphosis.dat", header = T)
# (a)
fit <- glm(y ~ x, family = "binomial", data = Kyphosis)
summary(fit)
# (b)
plot(jitter(y, 0.08) ~ x, data = Kyphosis, xlab = "Age", ylab = "Y")
# (c)
Kyphosis$x2 <- (Kyphosis$x)^2
fit2 <- glm(y ~ x + x2, family = "binomial", data = Kyphosis)
summary(fit2)
prd <- predict(fit, data.frame(x = Kyphosis$x), type="resp")
lines(sort(Kyphosis$x), prd[order(Kyphosis$x)], col='blue', type='l', lwd = 2) 
prd2 <- predict(fit2, data.frame(x = Kyphosis$x, x2 = Kyphosis$x2), type="resp")
lines(sort(Kyphosis$x), prd2[order(Kyphosis$x)], col='red', type='l', lwd = 2) 
legend("topright", legend=c("x", "x + x^2"),
       col=c("blue", "red"),lty = c(1,1), cex=0.6)
# 4.9
# (a)
Crabs <- read.table("C:/github_LTY/Lee_Tsung_Yu/NCKU/碩一下/Categorical data analysis/dataset/Crabs.dat", header = T)
fit1 <- glm(y ~ factor(color), family = binomial, data = Crabs)
summary(fit1)
fit2 <- glm(y ~ relevel(factor(color), ref = "4"), family = binomial, data = Crabs)
summary(fit2)
# (b)
library(car)
Anova(fit2)
# (c)
fit <- glm(y ~ color, family = binomial, data = Crabs)
summary(fit)
# (e)
fit <- glm(y ~ weight + color, family = binomial, data = Crabs)
summary(fit)

# 4.19
fit1 <- glm(y ~ width + factor(color), family = binomial, data = Crabs)
summary(fit1)
fit2 <- glm(y ~ width*factor(color), family = binomial, data = Crabs)
summary(fit2)
anova(fit1, fit2, test="LRT")
plot(jitter(y, 0.08) ~ width, data = Crabs)
for(i in c(1,2,3,4)){
  data <- Crabs[Crabs$color == i, c(5,6)]
  prd <- predict(fit2, data, type = "response")
  lines(sort(data$width), prd[order(data$width)], col = i, type = "l", lwd = 2)
}
legend("bottomright", legend=c("color = 1", "color = 2", "color = 3", "color = 4"),
       col=c(1,2,3,4),lty = c(1,1,1,1), cex=0.6, lwd = c(2,2,2,2))
