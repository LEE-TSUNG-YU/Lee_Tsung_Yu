library(mixcure)
#
library(magrittr)
library(stringr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(gmm)
library(gee)
library(randomForest)
#library(islasso)
library(glmnet)
library(survminer)
library(survival)
library(rcompanion)
library(fitdistrplus)
library(DataExplorer)
library(gt)

# EDA ####
AD.1 <- read.csv("C:/github_LTY/Lee_Tsung_Yu/MyPaper/Yui/AD_data.csv") # old data
AD.1 <- read.csv("C:/github_LTY/Lee_Tsung_Yu/MyPaper/Yui/AD_data_0907new.csv") # 
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
Summary.Table(AD.1) %>% gt()

# table(AD.1$event) # 0: 113(34.3%), 1: 216(65.7%)
# table(AD.1$APOE) # 0: 231(70.2%), 1: 98(29.8%)
# table(AD.1$Gender,AD.1$APOE)
#     0   1
# 0 118  56
# 1 113  42
# chisq.test(table(AD.1$Gender,AD.1$APOE)) # p-value = 0.3754

mean(AD.1[AD.1$Gender == 0 & AD.1$APOE == 0,]$diagnosis_age)
median(AD.1$CASI_score)
## Missing values ####
### Imputing
# variable "Education" has 3 missing values.
AD.1[which(is.na(AD.1$Education)==T),5] <- median(AD.1[-which(is.na(AD.1$Education)==T),5])
# CDR has a missing data(-1) in row 231 
AD.1[which(AD.1$CDR_score==-1),9] <- 0.5

### CC
AD.1 <- AD.1[complete.cases(AD.1),]
AD.1 <- AD.1[-which(AD.1$CDR_score==-1),]

# Yui's model ####
# Our function by mixture cure model (logistic-weibull model) for GMM function
AD.2 <- cbind(AD.1,AD.1[,3])
names(AD.2)[11] <- c("Fin.Dementia")
#(colMeans(is.na(AD.2)))
head(AD.2)

# functions
initial_Lambda.1<-function(Time,Status,X,Z,id,corstr){
  X<-as.matrix(cbind(1,X))
  Z<-as.matrix(cbind(1,Z))
  w <- Status
  t2 <- Time
  K <- length(unique(id))
  n <- as.vector(table(id))
  Kn <- sum(n)
  cens <- Status
  t11 <- sort(Time)
  c11 <- Status[order(Time)]
  x111 <- as.matrix(X[order(Time), ])
  g11 <- w[order(Time)]
  tt1 <- unique(t11[c11 == 1])
  kk <- length(table(t11[c11 == 1]))
  dd <- as.matrix(table(t11[c11 == 1]))
  gg1 <- Status
  gg2 <- log(Time)
  gg1[gg1<1e-06] <- 1e-06
  gg3 <- log(gg1) + log(Time)
  #pmt1c <- eval(parse(text = paste("geese", "(", "w~Z[,-1]", ",id=", "id", ",family = binomial", ",corstr='", corstr, "'", ")", sep = "")))$beta
  pmt1c<-gee(w~Z[,-1],id=id,family=binomial,corstr=corstr)
  pmt1c<-pmt1c$coefficients
  #pmt1s <- eval(parse(text = paste("geese", "(", "w~X-1+", "offset(", "gg3", ")", ",id=", "id", ",family = poisson", ",corstr='", corstr, "'", ")", sep = "")))$beta
  pmt1s<-gee(w~X[,-1],as.data.frame(offset(gg3)),id=id,family=poisson,corstr=corstr)
  pmt1s<-pmt1s$coefficients
  ppmt2<-c(pmt1c, pmt1s)
  KK<-1
  repeat{
    gSSS1 <- rep(0, kk)
    KK1 <- 1
    repeat{
      gSS<-rep(0, kk)
      gSS1<-rep(1, kk)            
      gSS[1]<-dd[1]/(sum(g11[min((1:(Kn))[t11 == tt1[1]]):(Kn)]*exp(matrix(x111[min((1:(Kn))[t11 == tt1[1]]):(Kn),],ncol=dim(X)[2])%*%matrix(pmt1s,ncol=1))))
      for (i in 1:(kk-1)){
        
        gSS[i+1]<-gSS[i]+dd[i+1]/(sum(g11[min((1:(Kn))[t11==tt1[i+1]]):(Kn)]*exp(matrix(x111[min((1:(Kn))[t11==tt1[i+1]]):(Kn),],ncol=dim(X)[2])%*%matrix(pmt1s,ncol=1))))
      }
      gSS1<-exp(-gSS)
      gSS2<-rep(0, Kn)
      gSS3<-rep(0, Kn)
      for(i in 1:Kn){
        kk1<-1
        if(t2[i]<tt1[1]){
          gSS2[i]<-1
          gSS3[i]<-1e-08
        }else{
          if(t2[i]>=tt1[kk]) {
            gSS2[i]<-0
            gSS3[i]<-gSS[kk]
          }else{
            repeat{
              if(t2[i]>=tt1[kk1]) 
                kk1<-kk1+1 else break}
            {gSS2[i]<-(gSS1[kk1-1])^(exp(X[i,]%*%matrix(pmt1s,ncol=1)))
              gSS3[i]<-gSS[kk1 - 1]
            }
          }
        }
      }
      gg2<-log(gSS3)
      gg3<-log(gg1)+gg2
      #ww2 <- eval(parse(text = paste("geese", "(", "w~X-1+", "offset(", "gg3", ")", ",id=", "id", ",family = poisson", ",corstr='", corstr, "'", ")", sep = "")))
      ww2<-gee(w~X[,-1],as.data.frame(offset(gg3)),id=id,family=poisson,corstr=corstr)
      
      if(KK1<100&&(any(abs(ww2$beta-pmt1s)>1e-06)||any(abs(gSS1-gSSS1)>1e-06))){
        pmt1s<-c(ww2$coefficients)
        gSSS1<-gSS1
        KK1 <- KK1 + 1
      } else {
        gg1<-Status+((1-Status)*exp(Z%*%matrix(pmt1c,ncol=1))*gSS2)/(1+exp(Z%*%matrix(pmt1c,ncol=1))*gSS2)
        g11<-gg1[order(t2)]
        gg1[gg1<1e-06]<-1e-06
        gg3<-log(gg1)+gg2
        break
      }}
    pmt2c<-gee(w~Z[,-1],id=id,family=binomial,corstr=corstr)
    pmt2c<-pmt2c$coefficients
    
    pmt2s<-gee(w~X[,-1],as.data.frame(offset(gg3)),id=id,family=poisson,corstr=corstr)
    pmt2s<-pmt2s$coefficients
    
    if (any(abs(pmt2c-pmt1c)>1e-03)||max((pmt2s - pmt1s)^2)>1e-03){
      pmt1c<-pmt2c
      pmt1s <- pmt2s
      KK<-KK+1
    } else break
  }
  Lambda<-gSS3
  list(Lambda = Lambda,beta.00=pmt1s)
}
moments.g.1<-function(theta,data){
  y<-matrix(data[,3],ncol=1)
  Q<-data.matrix(data[,-c(1:3)])
  theta.1<-matrix(theta,ncol=1)
  Q<-cbind(1,Q)
  mu<-exp(Q%*%theta.1)
  G<-c(y-(mu/(1+mu)))*Q
  return(G)
}
moments.G1<-function(theta,data){
  alpha.u1<-alpha.01
  y<-matrix(data[,1],ncol=1)
  x<-data.matrix(data[,-c(1:3)])
  Wi<-matrix(data[,3],ncol=1)
  theta.1<-matrix(theta,ncol=1)
  status<-data[,2]
  G<-matrix(0,ncol=1,nrow=dim(data)[1])
  q.1<-cbind(1,x)
  mu<-q.1%*%theta.1
  G<-c(status-(exp(mu)*(y)^(alpha.u1)))
  G.1<-c(Wi)*c(G)
  G.2<-c(G.1)*q.1
  return(G.2)
}
moments.alpha<-function(theta,data){
  y<-matrix(data[,1],ncol=1)
  x<-data.matrix(data[,-c(1:3)])
  Wi<-matrix(data[,3],ncol=1)
  theta.1<-matrix(theta,ncol=1)
  status<-data[,2]
  q.1<-cbind(1,x)
  G.A<-(log(y)*(Wi*status-Wi*(y^(theta))*exp(q.1%*%Beta.b))+status/theta)
  return(G.A)
}
LL.aic.logis<-function(beta,gamma,alpha.01,data){
  #probability
  gamma.1<-matrix(gamma,ncol=1)
  beta.1<-matrix(beta,ncol=1)
  Y.1<-data[,1]
  Delta.1<-data[,2]
  UU<-data[,3]
  alpha.1<-alpha.01
  X.1<-cbind(1,data[,-c(1:3)])
  Z.1<-cbind(1,data[,-c(1:3)])
  X.1<-as.matrix(X.1)
  Z.1<-as.matrix(Z.1)
  P1<-exp(Z.1%*%gamma.1)/(1+exp(Z.1%*%gamma.1))
  #SC<-exp(-Y.1/29565)
  #SC<-1
  #log-likelihood
  #L.sum<-sum((UU*log(P1))+(1-UU)*log(1-P1))
  #Consider complete likelihood function
  L.t<-c()
  for(j in 1:dim(data)[1]){
    if(UU[j]==1){L.t<-c(L.t,UU[j]*log(P1[j]*(((alpha.1*(Y.1[j]^(alpha.1-1))*exp(X.1[j,]%*%beta.1)))^Delta.1[j])*exp(-exp(X.1[j,]%*%beta.1)*(Y.1[j]^alpha.1))))}else{
      L.t<-c(L.t,UU[j]*log(P1[j]*(((alpha.1*(Y.1[j]^(alpha.1-1))*exp(X.1[j,]%*%beta.1)))^Delta.1[j])*exp(-exp(X.1[j,]%*%beta.1)*(Y.1[j]^alpha.1)))+(1-UU[j])*log(1-P1[j]))
    }
  }
  
  #L.sum<-sum(UU*log(P1*(((alpha.1*(Y.1^(alpha.1-1))*exp(X.1%*%beta.1)))^Delta.1)*exp(-exp(X.1%*%beta.1)*(Y.1^alpha.1)))+
  #            (1-UU)*log(1-P1))
  #L.sum<-sum(log((P1*(((alpha.1*(Y.1^(alpha.1-1))*exp(X.1%*%beta.1)))^Delta.1)*exp(-exp(X.1%*%beta.1)*(Y.1^alpha.1)))+
  #                 (1-P1)*SC)) 
  return(list(LL=-2*sum(L.t)))
}

# select column
# 7: age, 3: event, 11: Fin.Dementia(event), 2: Gender, 6: APOE, (10: CASI_score, 8: MMSE_score)
AD.3 <- AD.2[,c(7,3,11,2,6,8)]
dim(AD.3)
# median imputation CDR_score
# AD.3[231,6]<-0.5

Lambda.1 <- initial_Lambda.1(Time = AD.3$diagnosis_age,
                           Status = AD.3$event,
                           id = c(1:dim(AD.3)[1]),
                           X = AD.3[,-c(1:3)],
                           Z = AD.3[,-c(1:3)],
                           corstr = "independence")
Lambda1 <- Lambda.1$Lambda

(alpha.01 <- c(-sum(AD.3$event)/sum(AD.3$event*log(AD.3$diagnosis_age)*(AD.3$event-Lambda1))))
#if initial_Lambda.1 is error, you can run alpha.01 = 1
alpha.01 <- 1
#
logit_model <- glm(factor(Fin.Dementia)~ .,
                   data = AD.3[,-c(1:2)],
                   family=binomial(link = "logit"))
(gamma.00 <- coef(logit_model))

# 4 means intercept and explanatory variables: 1+3, and the number can be adjust 5,6 or 7
my_gmm.2 <- gmm(moments.G1, x = AD.3, t0 = rep(0,4), type="iterative",
                method = "Nelder-Mead", control = list(reltol = 1e-20,maxit = 20000))

(beta.00 <- my_gmm.2$coefficients)
names(beta.00) <- c("Intercept",names(AD.3)[-c(1:3)])
names(beta.00)
#
K.1 <- 0
para.test.0 <- c(gamma.00,beta.00,alpha.01)

#iterating 
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

K.1
(gamma.t.01 <- gamma.00)
# exp(gamma.t.01)
(beta.t.01 <- beta.00)
# exp(beta.t.01)
(alpha.t.S1 <- alpha.01)

# AIC
value.log <- LL.aic.logis(beta.t.01,gamma.t.01,alpha.t.S1,AD.3)
2*(length(gamma.t.01) + length(beta.t.01) + length(alpha.t.S1)) + value.log$LL

# Calculating estimated variances # Sandwitch
#
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

#Meat
Meat.1<-0
for(i in 1:N){
  G1.first<-Z.t.1[i,]*(AD.3$Fin.Dementia[i]-pi[i])
  B1.first<-AD.3$Fin.Dementia[i]*matrix(X.t.1[i,],ncol=1)*(AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))))
  A1.first<-AD.3$Fin.Dementia[i]*log(AD.3$diagnosis_age[i])*(AD.3$event[i]-((AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))))+(AD.3$event[i]/alpha.t.S1)
  Meat.t<-matrix(c(G1.first,B1.first,A1.first),ncol=1)
  Meat.1<-Meat.1+(Meat.t%*%t(Meat.t))
}
#
#Sandwitch
estimated.V<-(solve(-BREAD-BREAD2)%*%(Meat.1)%*%t(solve(-BREAD-BREAD2)))
#estimated.V<-(solve(BREAD)%*%(Meat.1)%*%solve(BREAD))
(SD.SW<-sqrt(diag(estimated.V)))
#

#
SD.SW[1:length(gamma.t.01)]
SD.SW[(length(gamma.t.01)+1):(length(gamma.t.01)+length(beta.t.01))]
SD.SW[length(gamma.t.01)+length(beta.t.01)+1]

G.Z<-abs(gamma.t.01/SD.SW[1:length(gamma.t.01)])
B.Z<-abs(beta.t.01/SD.SW[(length(gamma.t.01)+1):(length(gamma.t.01)+length(beta.t.01))])
Alpha.Z<-abs(alpha.t.S1/SD.SW[length(gamma.t.01)+length(beta.t.01)+1])
#var.SW
#Z test
2*(1-pnorm(G.Z))
2*(1-pnorm(B.Z))
2*(1-pnorm(Alpha.Z))
#
non.cong<-c()
for(k in 1:dim(AD.3)[1]){
  #p.t<-exp(gamma.t.01[1]+gamma.t.01[2]*AD.3[k,4]+gamma.t.01[3]*AD.3[k,5]+gamma.t.01[4]*AD.3[k,6])
  p.t<-exp(gamma.t.01[1]+gamma.t.01[2]*AD.3[k,4])
  
  non.cong<-c(non.cong,1-(p.t/(1+p.t)))
}

Non.Congi<-cbind(AD.2[-231,1],AD.2[-231,3],non.cong)
hist(non.cong,main="The probability of never experiencing dementia until death (Complete case)")
which(non.cong==max(non.cong))

AD.2[228,]

write.csv(Non.Congi,file.choose())

hist(AD.2$diagnosis_age)
median(AD.2$MMSE_score)#25
median(AD.2$Education)#9
median(AD.2$CDR_score)#0.5
median(AD.2$CASI_score)#84
beta.t.01
#(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*0)+(beta.t.01[3]*9)+(beta.t.01[4]*0)+(beta.t.01[5]*84)))^(1/alpha.t.S1)
(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*0)+(beta.t.01[3]*0)+(beta.t.01[4]*0.5)))^(1/alpha.t.S1)
#(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*9)+(beta.t.01[3]*0)))^(1/alpha.t.S1)
(log(2)/exp(beta.t.01[1]+(beta.t.01[2]*1)))^(1/alpha.t.S1)
#table(AD.2$event)

s <- with(AD.3,Surv(diagnosis_age,event))
sWei <- survreg(s ~ Gender+Education+APOE+CDR_score,dist='weibull',data=AD.3)
summary(sWei)
(predict(sWei, newdata=list(Gender=0,Education=9,APOE=0,CDR_score=0.5),type="quantile",p=0.5))

S <- glm(event~Gender+APOE+CASI_score,data=AD.3,family = binomial(link = "logit"))
summary(S)
#The end for my coding of mixture cure model 

cox <- coxph(Surv(diagnosis_age,event) ~ Gender+APOE+CASI_score,data = AD.3)
summary(cox)