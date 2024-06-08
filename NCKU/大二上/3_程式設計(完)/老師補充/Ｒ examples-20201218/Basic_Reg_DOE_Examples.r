
# Regression Reference:
# (1) A Modern Approach to Regression with R - Simon J. Sheather
# (2) Nonlinear Regression with R (use R) 2008

# Simple linear regression
# From ?lm
require(graphics)

## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

anova(lm.D9)
summary(lm.D90)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)

# From ?predict
require(graphics)

## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

# Multiple linear regression 
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
confint(fit)
confint(fit, "wt")

anova(fit)
summary(fit)

plot(fit)

#===================================================================
#===================================================================
# DOE 
# Referece: An R companion to "experimental design" 
#------------------------------
daily.intake <- c(5260,5470,5640,6180,6390,6515,
                  6805,7515,7515,8230,8770)

mean(daily.intake)

sd(daily.intake)

quantile(daily.intake)


simple.z.test.CI = function(x,sigma,conf.level=0.95) {
  n = length(x);xbar=mean(x)
  alpha = 1 - conf.level
  zstar = qnorm(1-alpha/2)
  SE = sigma/sqrt(n)
  xbar + c(-zstar*SE,zstar*SE)
}

simple.z.test.CI(daily.intake,1142)

t.test(daily.intake,mu=7725)

#=============================================================

install.packages("ISwR")
library(ISwR)

attach(energy)
energy


#Two Sample t-test
t.test(expend~stature)

t.test(expend~stature, var.equal=T)

var.test(expend~stature)

wilcox.test(expend~stature)

attach(intake)
intake

t.test(pre, post, paired=T)

wilcox.test(pre, post, paired=T)

qqnorm(expend)

boxplot(expend,stature)

install.packages("TeachingDemos")
library(TeachingDemos)
?sigma.test

#=================================================================
set.seed(1)
x=rnorm(5,mean=40,sd=10)
y=rnorm(5,mean=60,sd=10)
z=rnorm(5,mean=60,sd=10)
data.1 = stack(data.frame(x,y,z))
data.1
names(data.1)

plot(data.1)

oneway.test(values ~ ind, data=data.1,var.equal=T)

lm.out=lm(values ~ ind, data=data.1)
anova(lm.out)
summary(lm.out)

plot(lm.out)

names(lm.out)

lm.out$fitted.values

lm.out$effects

lm.out$residuals

plot(lm.out$residuals)

plot(lm.out$fitted.values,lm.out$residuals)

qqnorm(lm.out$residuals)   

# test normality 
shapiro.test(lm.out$residuals)


# Another way to proceed anova
aov(values ~ ind, data=data.1)

#=============================================================================
require(graphics)

InsectSprays

plot(count ~ spray, data = InsectSprays)

# test equal variance under normal assumption 
bartlett.test(InsectSprays$count, InsectSprays$spray)

bartlett.test(count ~ spray, data = InsectSprays)


# Levene test (test equal variance) 
data.list=by(InsectSprays$count,InsectSprays$spray,function(x)abs(x-median(x)))

# Test autocorrelation (dependence) 
install.packages("lmtest")
library("lmtest")
## generate two AR(1) error terms with parameter
## rho = 0 (white noise) and rho = 0.9 respectively
err1 <- rnorm(100)
err2 <- err1+err1[-1]
#test autocorrelation
dwtest(err1~1)
dwtest(err2~1)

## generate regressor and dependent variable
x <- rep(c(-1,1), 50)
y1 <- 1 + x + err1
## perform Durbin-Watson test
dwtest(y1 ~ x)

#===============================================================================


Fabric=data.frame(
  Resistance=c(1.93, 2.38, 2.20, 2.25, 2.55, 2.72, 2.75, 2.70,
               2.40, 2.68, 2.31, 2.28, 2.33, 2.40, 2.28, 2.25),
  Treatment=rep(c("A","B","C","D"),each=4))

attach(Fabric)

by(Resistance,Treatment,sum)
by(Resistance,Treatment,mean)

summary(aov(Resistance~Treatment))


# ??????????????? A, B-A, C-A, D-A  ??? mean effect 
summary.lm(aov(Resistance~Treatment))


# ?????? contrasts for effects of  A-D, B-C, A+D-B-C 
contrast.matrix=cbind(c(1, 0, 0, -1),c(0, 1, -1, 0),c(1, -1, -1, 1))
contrasts(Treatment)<-contrast.matrix

# ??????????????? # ?????? total mean(Intercept):mu, 
# Treatment1:A-D, 
# Treatment2:B-C, 
# Treatment3:A+D-B-C  ??? mean effect
summary.lm(aov(Resistance~Treatment))

# ???????????? contrast effect 
# A-D 
(sum(Resistance[1:4])- sum(Resistance[13:16]))/8

# B-C 
(sum(Resistance[5:8])- sum(Resistance[9:12]))/8

# A+D-B-C 
(sum(Resistance[c(1:4,13:16)])- sum(Resistance[5:12]))/16




#SNK test
install.packages("agricolae")
library(agricolae)

model<-aov(Resistance~Treatment,data=Fabric)
comparison <- SNK.test(model,"Treatment",
                       main="Resistance of Fabric. Dealt with different Treatment")
SNK.test(model,"Treatment", group=FALSE)
# version old SNK.test()

df<-df.residual(model)
MSerror<-deviance(model)/df
comparison <- SNK.test(Resistance,Treatment,df,MSerror, group=TRUE)


#scheffe.test
#library(agricolae) 
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
comparison <- scheffe.test(model,"virus", group=TRUE,
                           main="Yield of sweetpotato\nDealt with different virus")

# For testing several contrasts, one needs to do it step by step calculation


#Tukey HSD test
model<-aov(Resistance~Treatment,data=Fabric)
comparison <- HSD.test(model,"Treatment", group=TRUE,
                       main="Resistance of Fabric. Dealt with different Treatment")
#stargraph
bar.group(comparison,ylim=c(0,3),density=4,border="blue")
#endgraph

HSD.test(model,"Treatment", group=FALSE)

#LSD test
model<-aov(Resistance~Treatment,data=Fabric)
comparison <- LSD.test(model,"Treatment", group=TRUE,
                       main="Resistance of Fabric. Dealt with different Treatment")

LSD.test(model,"Treatment",p.adj="bon")

# Dunnett test
install.packages("multcomp")
library("multcomp") 

attach(Fabric)
model<-aov(Resistance~Treatment,data=Fabric)
dunnett.out=glht(model,linfct=mcp(Treatment="Dunnett"))
summary(dunnett.out)
