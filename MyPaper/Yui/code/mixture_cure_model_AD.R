library(mixcure)
library(magrittr)
library(stringr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(gmm)
library(gee)
library(randomForest)
library(glmnet)
library(survminer)
library(rcompanion)
library(fitdistrplus)
library(DataExplorer)
library(gt)

source("C:/github_LTY/Lee_Tsung_Yu/MyPaper/Yui/code/sorted/function.r")
Summary.Table <- function(data){
  output <- data.frame(feature = names(data),
                       type = sapply(data, class),
                       unique_value = sapply(data, function(x){if(!is.numeric(x)){length(unique(x))}
                         else{"---"}}),
                       num_missing = sapply(data, function(x){sum(is.na(x))}),
                       missing_rate = sapply(data, function(x) round(mean(is.na(x))*100, 2)))
  colnames(output) <- (c("feature", "type", "# of unique value",
                         "# of missing value", "missing rate(%)"))
  rownames(output) <- NULL
  return(output)
}

# Part 1: AD_data_329.csv ####
## Read data ####
AD.1 <- read.csv("C:/github_LTY/Lee_Tsung_Yu/MyPaper/Yui/data/AD_data_329.csv")
## EDA ####

# Part 2: AD_data_301.csv ####
## Read data ####
AD.1 <- read.csv("C:/github_LTY/Lee_Tsung_Yu/MyPaper/Yui/data/AD_data_301.csv")

## EDA ####
table(AD.1$event)
table(AD.1$APOE)
table(AD.1$Gender,AD.1$APOE)
chisq.test(table(AD.1$Gender,AD.1$APOE)) # p-value = 0.2026
median(AD.1$CASI_score) # The median of CASI = 80.5


# Part 3: AD_data_374.csv ####
## Read data ####
AD.1 <- read.csv("C:/github_LTY/Lee_Tsung_Yu/MyPaper/Yui/data/AD_data_impute_374.csv")

## EDA ####
table(AD.1$Gender) # 0: 221, 1: 153
table(AD.1$event) # 0: 118, 1: 256
table(AD.1$Education) # integer, range 0 ~ 22
table(AD.1$APOE) # 0: 229, 1: 145
table(AD.1$diagnosis_age) # integer, range 23 ~ 85
table(AD.1$MMSE_score) # integer, range 0 ~ 30
table(AD.1$CDR_score) # 5 categories: 0, 0.5, 1, 2, 3
table(AD.1$CASI_score) # continuous, range 0 ~ 100
table(AD.1$MMSE_age) # integer, range 25 ~ 88
table(AD.1$Gender,AD.1$APOE)
chisq.test(table(AD.1$Gender,AD.1$APOE)) # p-value = 0.2092
table(AD.1$Gender,AD.1$event)
chisq.test(table(AD.1$Gender,AD.1$event)) # p-value = 0.002758
table(AD.1$event,AD.1$APOE)
chisq.test(table(AD.1$event,AD.1$APOE)) # p-value < 0.001
median(AD.1$CASI_score) # The median of CASI = 80.5

# Run coefficients estimates ####
names(AD.1)
AD.2 <- cbind(AD.1, AD.1[,3])
names(AD.2)[11] <- "Fin.Dementia"
AD.3 <- AD.1[,c(7,3,3,2,6,10)]
names(AD.3)[3] <- "Fin.Dementia"

# Set initial alpha
Lambda.1 <- initial_Lambda.1(Time = AD.3$diagnosis_age,
                             Status = AD.3$event,
                             id = c(1:dim(AD.3)[1]),
                             X = AD.3[,-c(1:3)],
                             Z = AD.3[,-c(1:3)],
                             corstr = "independence")
Lambda1 <- Lambda.1$Lambda
(alpha.01 <- c(-sum(AD.3$event)/sum(AD.3$event*log(AD.3$diagnosis_age)*(AD.3$event-Lambda1))))
# if initial_Lambda.1 is error, you can run alpha.01 = 1
alpha.01 <- 1

# set initial coefficients
logit_model <- glm(factor(Fin.Dementia)~ .,
                   data = AD.3[,-c(1:2)],
                   family = binomial(link = "logit"))
(gamma.00 <- coef(logit_model))
my_gmm.2 <- gmm(moments.G1, x = AD.3, t0 = rep(0,4), type="iterative",
                method = "Nelder-Mead", control = list(reltol = 1e-20,maxit = 20000))
(beta.00 <- my_gmm.2$coefficients)
names(beta.00) <- c("Intercept",names(AD.3)[-c(1:3)])
names(beta.00)

# Get estimated coefficients
K.1 <- 0
para.test.0 <- c(gamma.00,beta.00,alpha.01)
# iterating 
tryCatch({
  while(1){
    gamma.1 <- matrix(gamma.00,ncol=1)
    beta.1 <- matrix(beta.00,ncol=1)
    X.t.1 <- cbind(1,AD.3[,-c(1:3)])
    Z.t.1 <- cbind(1,AD.3[,-(1:3)])
    X.t.1 <- as.matrix(X.t.1)
    Z.t.1 <- as.matrix(Z.t.1)
    S3 <- exp(-exp(X.t.1%*%beta.1)*(AD.3$diagnosis_age^alpha.01))
    pi <- exp(Z.t.1%*%gamma.1)
    SC <- 1
    #SC <- exp(-data.ob.1$Surv.time/29565) ignore it
    Wi <- AD.3$event+((1-AD.3$event)*pi*(S3)/(((1)*(SC))+(pi*S3)))
    AD.3$Fin.Dementia <- Wi
    # incidence part
    my_gmm.t1 <- gmm(moments.g.1,x=AD.3,t0=gamma.00,type="iterative",wmatrix = "optimal",
                     method = "Nelder-Mead", control = list(reltol = 1e-25, maxit=20000))
    (gamma.01 <- my_gmm.t1$coefficients)
    # latency part
    my_gmm.t2 <- gmm(moments.G1,x=AD.3,t0=beta.00,type="iterative",wmatrix = "optimal",
                     method = "Nelder-Mead", control = list(reltol = 1e-25,maxit=20000))
    
    (beta.01 <- my_gmm.t2$coefficients)
    Beta.b <- matrix(beta.01,ncol=1)
    my_gmm.t3 <- gmm(moments.alpha,x=AD.3,t0=c(alpha.01),optfct=c("nlminb"))
    (alpha.S1 <- my_gmm.t3$coefficients)
    
    para.test <- c(gamma.01,beta.01,alpha.S1)
    # AIC
    value.log <- LL.aic.logis(beta.01,gamma.01,alpha.S1,AD.3)
    
    ### Criterion 
    if((max(abs((para.test[-c(1,5)])-(para.test.0[-c(1,5)])))<0.01)|value.log$LL==Inf|K.1>100)#if the result worng, can try 0.05 or 0.1
      break
    # LL.0<-LL.s
    gamma.00 <- gamma.01
    beta.00 <- beta.01
    alpha.01 <- alpha.S1
    K.1 <- K.1+1
    para.test.0 <- para.test
    print(para.test)
  }
},error=function(e){c(gamma.01,beta.01,alpha.S1)})

(gamma.t.01 <- gamma.00) # estimate
exp(gamma.t.01) 
(beta.t.01 <- beta.00) # estimate
exp(beta.t.01)
(alpha.t.S1 <- alpha.01)

result <- matrix(NA, nrow = 8, ncol = 4)
result[,1] <- c(gamma.t.01, beta.t.01)
result[,3] <- round(exp(c(gamma.t.01, beta.t.01)),3)

# AIC
value.log <- LL.aic.logis(beta.t.01,gamma.t.01,alpha.t.S1,AD.3)
2*(length(gamma.t.01) + length(beta.t.01) + length(alpha.t.S1)) + value.log$LL

# Calculating estimated variances # Sandwich
Z.t.1 <- data.matrix(cbind(1,AD.3[,-c(1:3)]))
X.t.1 <- data.matrix(cbind(1,AD.3[,-c(1:3)]))
N <- dim(AD.3)[1]
gamma.1 <- matrix(gamma.t.01,ncol=1)
beta.1 <- matrix(beta.t.01,ncol=1)
pi <- exp(Z.t.1%*%gamma.1)/(1+exp(Z.t.1%*%gamma.1))
# Bread 1
G1.sec<-0;B1.sec<-0;B2.sec<-0;A1.sec<-0;A2.sec<-0
for(i in 1:N){
  pi.sec.t<--(matrix(Z.t.1[i,],ncol=1)%*%matrix(Z.t.1[i,],nrow=1))*pi[i]*(1-pi[i])
  G1.sec <- G1.sec+pi.sec.t
  B1.sec.t<--AD.3$Fin.Dementia[i]*(matrix(X.t.1[i,],ncol=1)%*%matrix(X.t.1[i,],nrow=1))*(AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))
  B1.sec <- B1.sec+B1.sec.t
  B2.sec.t<--AD.3$Fin.Dementia[i]*matrix(X.t.1[i,],ncol=1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))*(AD.3$diagnosis_age[i]^alpha.t.S1)*log(AD.3$diagnosis_age[i])
  B2.sec <- B2.sec+B2.sec.t
  A1.sec.t<--AD.3$Fin.Dementia[i]*(AD.3$diagnosis_age[i]^alpha.t.S1)*log(AD.3$diagnosis_age[i])*matrix(X.t.1[i,],nrow=1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))
  A1.sec <- A1.sec+A1.sec.t
  A2.sec.t<--AD.3$Fin.Dementia[i]*((log(AD.3$diagnosis_age[i]))^2)*(AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))-(AD.3$event[i]/(alpha.t.S1^2))
  A2.sec <- A2.sec+A2.sec.t
}
Bread.t1 <- cbind(G1.sec,matrix(0,nrow=dim(Z.t.1)[2],ncol=dim(X.t.1)[2]),matrix(0,nrow=dim(Z.t.1)[2],ncol=1))
Bread.t2 <- cbind(matrix(0,nrow=dim(X.t.1)[2],ncol=dim(Z.t.1)[2]),B1.sec,B2.sec)
Bread.t3 <- cbind(matrix(0,nrow=1,ncol=dim(Z.t.1)[2]),A1.sec,A2.sec)
BREAD <- rbind(Bread.t1,Bread.t2,Bread.t3)

# Bread 2
GG.sec<-0;GB.sec<-0;GA.sec<-0;BB.sec<-0;BA.sec<-0;AA.sec<-0
for(i in 1:N){
  GG.sec.t<-matrix(Z.t.1[i,],ncol=1)%*%matrix(Z.t.1[i,],nrow=1)*(AD.3$Fin.Dementia[i]*(1-AD.3$Fin.Dementia[i]))
  GG.sec<-GG.sec+GG.sec.t
  GB.sec.t<-matrix(Z.t.1[i,],ncol=1)%*%matrix(X.t.1[i,],nrow=1)*(AD.3$Fin.Dementia[i]*(1-AD.3$Fin.Dementia[i]))*(AD.3$event[i]-(AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1))))
  GB.sec<-GB.sec+GB.sec.t
  GA.sec.t<-matrix(Z.t.1[i,],ncol=1)%*%(AD.3$Fin.Dementia[i]*(1-AD.3$Fin.Dementia[i]))*log(AD.3$diagnosis_age[i])*(AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))))
  GA.sec<-GA.sec+GA.sec.t
  BB.sec.t<-(c((AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))))^2)*c(AD.3$Fin.Dementia[i]*(1-AD.3$Fin.Dementia[i])))*matrix(X.t.1[i,],ncol=1)%*%matrix(X.t.1[i,],nrow=1)
  BB.sec<-BB.sec+BB.sec.t
  BA.sec.t<-matrix(X.t.1[i,],ncol=1)*((AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))))^2)*(AD.3$Fin.Dementia[i]*(1-AD.3$Fin.Dementia[i]))*log(AD.3$diagnosis_age[i])
  BA.sec<-BA.sec+BA.sec.t
  AA.sec.t<-((log(AD.3$diagnosis_age[i])*(AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1))))))^2)*(AD.3$Fin.Dementia[i]*(1-AD.3$Fin.Dementia[i]))
  AA.sec<-AA.sec+AA.sec.t
}
Bread2.t1<-cbind(GG.sec,GB.sec,GA.sec)
Bread2.t2<-cbind(t(GB.sec),BB.sec,BA.sec)
Bread2.t3<-cbind(t(GA.sec),t(BA.sec),AA.sec)
BREAD2<-rbind(Bread2.t1,Bread2.t2,Bread2.t3)

# Meat
Meat.1<-0
for(i in 1:N){
  G1.first<-Z.t.1[i,]*(AD.3$Fin.Dementia[i]-pi[i])
  B1.first<-AD.3$Fin.Dementia[i]*matrix(X.t.1[i,],ncol=1)*(AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))))
  A1.first<-AD.3$Fin.Dementia[i]*log(AD.3$diagnosis_age[i])*(AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))))+(AD.3$event[i]/alpha.t.S1)
  Meat.t<-matrix(c(G1.first,B1.first,A1.first),ncol=1)
  Meat.1<-Meat.1+(Meat.t%*%t(Meat.t))
}
#
# Sandwitch
estimated.V<-(solve(-BREAD-BREAD2)%*%(Meat.1)%*%t(solve(-BREAD-BREAD2)))
# estimated.V<-(solve(BREAD)%*%(Meat.1)%*%solve(BREAD))
(SD.SW<-sqrt(diag(estimated.V)))

SD.SW[1:length(gamma.t.01)]
SD.SW[(length(gamma.t.01)+1):(length(gamma.t.01)+length(beta.t.01))]
SD.SW[length(gamma.t.01)+length(beta.t.01)+1]

result[,2] <- c(SD.SW[1:length(gamma.t.01)], SD.SW[(length(gamma.t.01)+1):(length(gamma.t.01)+length(beta.t.01))])

G.Z<-abs(gamma.t.01/SD.SW[1:length(gamma.t.01)])
B.Z<-abs(beta.t.01/SD.SW[(length(gamma.t.01)+1):(length(gamma.t.01)+length(beta.t.01))])
Alpha.Z<-abs(alpha.t.S1/SD.SW[length(gamma.t.01)+length(beta.t.01)+1])
# var.SW
# Z test
2*(1-pnorm(G.Z))
2*(1-pnorm(B.Z))
2*(1-pnorm(Alpha.Z))
result[,4] <- c(2*(1-pnorm(G.Z)), 2*(1-pnorm(B.Z)))
result[1,3] <- result[5,3] <- NA
colnames(result) <- c("estimate", "S.E.", "odds ratio", "p-value")
rownames(result) <- rep(c("Intercept",names(AD.3)[-c(1:3)]),2)
round(result,3)
write.csv(result,"C:/github_LTY/Lee_Tsung_Yu/MyPaper/Yui/result/result_374_1.csv", row.names = T)

beta.t.01
#(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*0)+(beta.t.01[3]*9)+(beta.t.01[4]*0)+(beta.t.01[5]*84)))^(1/alpha.t.S1)
(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*0)+(beta.t.01[3]*0)+(beta.t.01[4]*0.5)))^(1/alpha.t.S1)
#(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*9)+(beta.t.01[3]*0)))^(1/alpha.t.S1)
(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*1)))^(1/alpha.t.S1)
#table(AD.2$event)

# boostrap
beta.est.1.v<-c();gamma.est.1.v<-c()
alpha.est.1.v<-c()
AD.bv<-AD.2[,c(7,3,11,2,5,6,8:10)]

for(boot in 1:100){
  set.seed(boot)
  N.boot.a<-sample(1:(dim(AD.bv)[1]),replace=T)
  AD.bv.1<-AD.bv[N.boot.a,]
  #
  
  alpha.01<-0.2
  #
  #incidence
  U.logit.1<-rbinom(length(AD.bv.1$Fin.Dementia),1,c(AD.bv.1$Fin.Dementia))
  lambda.m<-cv.glmnet(data.matrix(AD.bv.1[,-c(1:3)]),U.logit.1,family=binomial,standardize = FALSE,alpha=1)
  (gamma.00<-coef(lambda.m,s=lambda.m$lambda.min)[1:(dim(AD.bv.1[,-c(1:3)])[2]+1)])
  defult.lambda<-lambda.m$lambda.min
  
  my_gmm.2<-gmm(moments.G1,x=AD.bv.1,t0=rep(0,dim(AD.bv.1[,-c(1:3)])[2]+1),type="iterative",
                wmatrix = "optimal",method = "Nelder-Mead",control = list(reltol = 1e-20,maxit=20000))
  (beta.00<-my_gmm.2$coefficients)
  names(beta.00)<-c("Intercept",names(AD.bv.1)[-c(1:3)])
  names(gamma.00)<-c("Intercept",names(AD.bv.1)[-c(1:3)])
  names(beta.00)
  beta.00
  #
  K.1<-0
  para.test.0<-c(beta.00,gamma.00,alpha.01)
  
  while(1){
    gamma.1<-matrix(gamma.00,ncol=1)
    beta.1<-matrix(beta.00,ncol=1)
    X.t.1<-cbind(1,AD.bv.1[,-c((1:3))])
    Z.t.1<-cbind(1,AD.bv.1[,-c(1:3)])
    X.t.1<-as.matrix(X.t.1)
    Z.t.1<-as.matrix(Z.t.1)
    S3<-exp(-exp(X.t.1%*%beta.1)*(AD.bv.1$diagnosis_age^alpha.01))
    pi<-exp(Z.t.1%*%gamma.1)
    SC<-1
    #SC<-exp(-data.ob.1$Surv.time/120)
    #SC<-exp(-(data.ob.1$Surv.time/350.16)^13.66)
    Wi<-AD.bv.1$event+((1-AD.bv.1$event)*pi*(S3)/(((1)*(SC))+(pi*S3)))
    AD.3$Fin.Dementia<-Wi
    #
    #model.lasso<-glmnet(data.matrix(data.ob.1[,-c(1:3)]),Wi,family=binomial,standardize = FALSE,lambda=defult.lambda,alpha=1)
    #gamma.01<-coef(model.lasso)[1:(dim(data.ob.1[,-c(1:3)])[2]+1)]
    lambda.m1<-cv.glmnet(data.matrix(AD.bv.1[,-c(1:3)]),Wi,family=binomial,standardize = FALSE,alpha=1)
    (gamma.01<-coef(lambda.m1,s=lambda.m1$lambda.min)[1:(dim(AD.bv.1[,-c(1:3)])[2]+1)])
    
    
    # bebias
    #Z.t.1<-data.matrix(cbind(1,data.ob.1[,-c(1:3)]))
    #gamma.tt<-matrix(gamma.01,ncol=1)
    # first derivating
    #pi<-exp(Z.t.1%*%gamma.tt)/(1+exp(Z.t.1%*%gamma.tt))
    #pi.first<-sapply(1:(dim(Z.t.1)[1]),function(i){Z.t.1[i,]*(data.ob.1$cured[i]-pi[i])})
    #pi.first.1<-matrix(apply(pi.first,1,sum),ncol=1)
    # second derivating
    #pi.sec<-0
    #for(i in 1:dim(Z.t.1)[1]){
    #  pi.sec.t<-(matrix(Z.t.1[i,],ncol=1)%*%matrix(Z.t.1[i,],nrow=1))*pi[i]*(1-pi[i])
    #  pi.sec<-pi.sec+pi.sec.t}
    #pi.sec.1<-(-pi.sec)/(dim(Z.t.1)[1])
    #pi.sec.1.inv<-solve(pi.sec.1)
    #gamma.1.debias<-gamma.tt-(pi.sec.1.inv%*%(pi.first.1))/(dim(Z.t.1)[1])
    #gamma.01<-c(gamma.1.debias)
    
    #Cox
    my_gmm.t2<-gmm(moments.G1,x=AD.bv.1,t0=beta.00,type="iterative",wmatrix = "optimal",
                   method = "Nelder-Mead", control = list(reltol = 1e-25, maxit=20000))
    (beta.01<-my_gmm.t2$coefficients)
    #
    Beta.b<-matrix(beta.01,ncol=1)
    my_gmm.t3<-gmm(moments.alpha,x=AD.bv.1,t0=c(alpha.01),optfct=c("nlminb"))
    (alpha.S1<-my_gmm.t3$coefficients)
    para.test<-c(beta.01,gamma.01,alpha.S1)
    ###
    if((max(abs(para.test-para.test.0))<0.01)|K.1>50|alpha.S1<0)
      break
    #beta.storge<-rbind(beta.storge,beta.01)
    #gamma.storge<-rbind(gamma.storge,gamma.01)
    gamma.00<-gamma.01
    beta.00<-beta.01
    alpha.01<-alpha.S1
    K.1<-K.1+1
    para.test.0<-para.test
    print(para.test)
  }
  
  gamma.t.01<-gamma.01
  beta.t.01<-beta.01
  alpha.t.S1<-alpha.S1
  
  beta.est.1.v<-rbind(beta.est.1.v,beta.t.01)
  gamma.est.1.v<-rbind(gamma.est.1.v,gamma.t.01)
  alpha.est.1.v<-c(alpha.est.1.v,alpha.t.S1)
}

SD.beta.boot<-sqrt(apply(beta.est.1.v,2,var))
SD.gamma.boot<-sqrt(apply(gamma.est.1.v,2,var))
SD.alpha.boot<-sqrt(var(alpha.est.1.v))

#
G.Z<-abs(gamma.t.01/SD.SW[1:length(gamma.t.01)])
G.Z.boot<-abs(gamma.t.01/SD.gamma.boot)
B.Z<-abs(beta.t.01/SD.SW[(length(gamma.t.01)+1):(length(gamma.t.01)+length(beta.t.01))])
B.Z.boot<-abs(beta.t.01/SD.beta.boot)
Alpha.Z<-abs(alpha.t.S1/SD.SW[length(gamma.t.01)+length(beta.t.01)+1])
Alpha.Z.boot<-abs(alpha.t.S1/SD.alpha.boot)
#var.SW
#Z test
2*(1-pnorm(G.Z))
2*(1-pnorm(G.Z.boot))

2*(1-pnorm(B.Z))
2*(1-pnorm(B.Z.boot))

2*(1-pnorm(Alpha.Z))
2*(1-pnorm(Alpha.Z.boot))
