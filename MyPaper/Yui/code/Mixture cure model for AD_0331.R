library(mixcure)
library(survival)
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
library(rfUtilities)
library(survminer)
library(survival)
library(rcompanion)
library(fitdistrplus)

#============================

#data(leukaemia)
#z1 = mixcure(Surv(time, cens) ~ transplant, ~ transplant, data = leukaemia)
#print(z1)

#AD
#package mixcure by Peng
AD.1<-read.csv("C:/Users/Orange/Desktop/Analysis of the cognitive deterioration_1215/Mixture cure model/AD_data.csv")
head(AD.1)
str(AD.1)
table(AD.1$event)
table(AD.1$APOE)
dim(AD.1)
#APOE vs gender
table(AD.1$Gender,AD.1$APOE)
chisq.test(table(AD.1$Gender,AD.1$APOE))
#
#(colMeans(is.na(AD.1)))#variable "Education" has 3 missing values.
AD.1[which(is.na(AD.1$Education)==T),5]<-median(AD.1[-which(is.na(AD.1$Education)==T),5])
#

#Gender vs Education significantly different
#APOE vs Education indifferent

mean(AD.1[AD.1$Gender==0,5]);sqrt(var(AD.1[AD.1$Gender==0,5]));mean(AD.1[AD.1$Gender==1,5]);sqrt(var(AD.1[AD.1$Gender==1,5]))
median(AD.1[AD.1$Gender==1,5])

mean(AD.1[AD.1$APOE==0,5]);sqrt(var(AD.1[AD.1$APOE==0,5]));mean(AD.1[AD.1$APOE==1,5]);sqrt(var(AD.1[AD.1$APOE==1,5]))
median(AD.1[AD.1$APOE==1,5])


t.test(AD.1[AD.1$Gender==0,5],AD.1[AD.1$Gender==1,5])
t.test(AD.1[AD.1$APOE==0,5],AD.1[AD.1$APOE==1,5])

wilcox.test(AD.1[AD.1$Gender==0,5],AD.1[AD.1$Gender==1,5])
wilcox.test(AD.1[AD.1$APOE==0,5],AD.1[AD.1$APOE==1,5])

#Gender vs three scores
mean(AD.1[AD.1$Gender==0,8]);sqrt(var(AD.1[AD.1$Gender==0,8]));mean(AD.1[AD.1$Gender==1,8]);sqrt(var(AD.1[AD.1$Gender==1,8]))
median(AD.1[AD.1$Gender==1,10])

mean(AD.1[AD.1$APOE==0,10]);sqrt(var(AD.1[AD.1$APOE==0,10]));mean(AD.1[AD.1$APOE==1,10]);sqrt(var(AD.1[AD.1$APOE==1,10]))
median(AD.1[AD.1$APOE==0,10])

wilcox.test(AD.1[AD.1$Gender==0,10],AD.1[AD.1$Gender==1,10])
wilcox.test(AD.1[AD.1$APOE==0,10],AD.1[AD.1$APOE==1,10])


#APOE vs three scores

#
cor.test(AD.1$Education,AD.1$MMSE_score);cor.test(AD.1$Education,AD.1$CDR_score);cor.test(AD.1$Education,AD.1$CASI_score)
plot(AD.1$Education,AD.1$MMSE_score)
plot(AD.1$Education,AD.1$CDR_score)
plot(AD.1$Education,AD.1$CASI_score)

#
for(i in 0:1){
  for(j in 0:1){
    print(mean(AD.1[AD.1$Gender==i&AD.1$APOE==j,3]))
  }
}
  
GLM.logit<-glm(event~APOE+Education,data=AD.1,binomial(link = "logit"))
summary(GLM.logit)
GLM.logit<-glm(event~APOE+Gender,data=AD.1,binomial(link = "logit"))
summary(GLM.logit)

#coxph(Surv(diagnosis_age,event)~Gender+Education+APOE,data=AD.1)


z2 = mixcure(Surv(diagnosis_age, event)~Gender+Education+APOE+CASI_score,~Gender+Education+APOE+CASI_score, data = AD.1,savedata=T)

print(z2)


zt<-z2$ifit
#zt$fit$coefficients
               


#system.time(summary(mixcure(Surv(diagnosis_age, event) ~ Gender+Education+APOE+MMSE_score,~Gender+Education+APOE+MMSE_score, data = AD.1, savedata=T),R = 100))

#bootstrap
var.ind<-c()
system.time(
for(boot in 1:150){
  set.seed(boot+2024)
  boot.sample<-sample(1:329,329,replace=T)
  AD.boot<-AD.1[boot.sample,]
  z2.tt = mixcure(Surv(diagnosis_age, event) ~ Gender+APOE,~Gender+APOE, data = AD.boot)
  z3<-z2.tt$ifit
  var.ind<-rbind(var.ind,z3$fit$coefficients)
}
)
var.ind
var.ind[-which(abs(var.ind[,1])>15),]
dim(var.ind[-which(abs(var.ind[,1])>15),])
sqrt(apply(var.ind,2,var))

2*(1-pnorm(abs(zt$fit$coefficients/sqrt(apply(var.ind,2,var)))))

#Our function
head(AD.1)
length(table(AD.1$PIDN))
table(AD.1$Gender)
#
AD.2<-cbind(AD.1,AD.1[,3])
names(AD.2)[11]<-c("Fin.Dementia")
#(colMeans(is.na(AD.2)))
head(AD.2)

#
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
#first three: Time event final objective;
AD.3<-AD.2[,c(7,3,11,2,5,6,10)]

Lambda.1<-initial_Lambda.1(Time=AD.3$diagnosis_age,Status=AD.3$event,
                           id=c(1:dim(AD.3)[1]),
                           X=AD.3[,-c(1:3)],Z=AD.3[,-c(1:3)],
                           corstr="independence")
Lambda1<-Lambda.1$Lambda

(alpha.01<-c(-sum(AD.3$event)/sum(AD.3$event*log(AD.3$diagnosis_age)*(AD.3$event-Lambda1))))
alpha.01<-0.5
#
#data.ob.1$U.logit<-factor(data.ob.1$cured)
logit_model<-glm(factor(Fin.Dementia)~.,data = AD.3[,-c(1:2)],family=binomial(link = "logit"))
(gamma.00<-coef(logit_model))

my_gmm.2<-gmm(moments.G1,x=AD.3,t0=rep(0,5),type="iterative",
              method = "Nelder-Mead",control = list(reltol = 1e-20,maxit=20000))

(beta.00<-my_gmm.2$coefficients)
names(beta.00)<-c("Intercept",names(AD.3)[-c(1:3)])
names(beta.00)
#
K.1<-0
para.test.0<-c(gamma.00,beta.00,alpha.01)
gamma.storage<-c()
beta.storage<-c()
tryCatch({
while(1){
  gamma.1<-matrix(gamma.00,ncol=1)
  beta.1<-matrix(beta.00,ncol=1)
  X.t.1<-cbind(1,AD.3[,-c(1:3)])
  Z.t.1<-cbind(1,AD.3[,-(1:3)])
  X.t.1<-as.matrix(X.t.1)
  Z.t.1<-as.matrix(Z.t.1)
  S3<-exp(-exp(X.t.1%*%beta.1)*(AD.3$diagnosis_age^alpha.01))
  pi<-exp(Z.t.1%*%gamma.1)
  SC<-1
  #SC<-exp(-data.ob.1$Surv.time/29565)
  Wi<-AD.3$event+((1-AD.3$event)*pi*(S3)/(((1)*(SC))+(pi*S3)))
  AD.3$Fin.Dementia<-Wi
  #
  my_gmm.t1<-gmm(moments.g.1,x=AD.3,t0=gamma.00,type="iterative",wmatrix = "optimal",
                 method = "Nelder-Mead", control = list(reltol = 1e-25, maxit=20000))
  (gamma.01<-my_gmm.t1$coefficients)
  #
  my_gmm.t2<-gmm(moments.G1,x=AD.3,t0=beta.00,type="iterative",wmatrix = "optimal",
                 method = "Nelder-Mead", control = list(reltol = 1e-25,maxit=20000))
  
  (beta.01<-my_gmm.t2$coefficients)
  Beta.b<-matrix(beta.01,ncol=1)
  my_gmm.t3<-gmm(moments.alpha,x=AD.3,t0=c(alpha.01),optfct=c("nlminb"))
  (alpha.S1<-my_gmm.t3$coefficients)
  
  para.test<-c(gamma.01,beta.01,alpha.S1)
  ###
  if((max(abs((para.test)-(para.test.0)))<0.001)|K.1>100)
    break
  #LL.0<-LL.s
  gamma.00<-gamma.01
  K.1<-K.1+1
  para.test.0<-para.test
  print(para.test)
}
},error=function(e){c(gamma.01,beta.01,alpha.S1)})

K.1
(gamma.t.01<-gamma.01)
exp(gamma.t.01)
(beta.t.01<-beta.01)
exp(beta.t.01)
(alpha.t.S1<-alpha.S1)

#AD.3<-AD.2[,c(7,3,11,2,6,8)]

#GLM.logit<-glm(event~Gender+Education+APOE,data=AD.1,binomial(link = "logit"))
#AIC(GLM.logit)
#GLM.logit$coefficients

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
  L.sum<-sum(UU*log(P1*(((alpha.1*(Y.1^(alpha.1-1))*exp(X.1%*%beta.1)))^Delta.1)*exp(-exp(X.1%*%beta.1)*(Y.1^alpha.1)))+
              (1-UU)*log(1-P1))
  #L.sum<-sum(log((P1*(((alpha.1*(Y.1^(alpha.1-1))*exp(X.1%*%beta.1)))^Delta.1)*exp(-exp(X.1%*%beta.1)*(Y.1^alpha.1)))+
  #                 (1-P1)*SC)) 
  return(list(LL=-2*L.sum))
}


#LL.aic.logis(beta.t.01,gamma.t.01,alpha.t.S1,AD.3)
value.log<-LL.aic.logis(beta.t.01,gamma.t.01,alpha.t.S1,AD.3)

2*(length(gamma.t.01)+length(beta.t.01)+length(alpha.t.S1))+value.log$LL

#estimated variances
#Sandwitch
#
Z.t.1<-data.matrix(cbind(1,AD.3[,-c(1:3)]))
X.t.1<-data.matrix(cbind(1,AD.3[,-c(1:3)]))
N<-dim(AD.3)[1]
gamma.1<-matrix(gamma.t.01,ncol=1)
beta.1<-matrix(beta.t.01,ncol=1)
pi<-exp(Z.t.1%*%gamma.1)/(1+exp(Z.t.1%*%gamma.1))
#Bread 1
G1.sec<-0;B1.sec<-0;B2.sec<-0;A1.sec<-0;A2.sec<-0
for(i in 1:N){
  pi.sec.t<--(matrix(Z.t.1[i,],ncol=1)%*%matrix(Z.t.1[i,],nrow=1))*pi[i]*(1-pi[i])
  G1.sec<-G1.sec+pi.sec.t
  B1.sec.t<--AD.3$Fin.Dementia[i]*(matrix(X.t.1[i,],ncol=1)%*%matrix(X.t.1[i,],nrow=1))*(AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))
  B1.sec<-B1.sec+B1.sec.t
  B2.sec.t<--AD.3$Fin.Dementia[i]*matrix(X.t.1[i,],ncol=1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))*(AD.3$diagnosis_age[i]^alpha.t.S1)*log(AD.3$diagnosis_age[i])
  B2.sec<-B2.sec+B2.sec.t
  A1.sec.t<--AD.3$Fin.Dementia[i]*(AD.3$diagnosis_age[i]^alpha.t.S1)*log(AD.3$diagnosis_age[i])*matrix(X.t.1[i,],nrow=1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))
  A1.sec<-A1.sec+A1.sec.t
  A2.sec.t<--AD.3$Fin.Dementia[i]*((log(AD.3$diagnosis_age[i]))^2)*(AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))-(AD.3$event[i]/(alpha.t.S1^2))
  A2.sec<-A2.sec+A2.sec.t
}
Bread.t1<-cbind(G1.sec,matrix(0,nrow=dim(Z.t.1)[2],ncol=dim(X.t.1)[2]),matrix(0,nrow=dim(Z.t.1)[2],ncol=1))
Bread.t2<-cbind(matrix(0,nrow=dim(X.t.1)[2],ncol=dim(Z.t.1)[2]),B1.sec,B2.sec)
Bread.t3<-cbind(matrix(0,nrow=1,ncol=dim(Z.t.1)[2]),A1.sec,A2.sec)
BREAD<-rbind(Bread.t1,Bread.t2,Bread.t3)

#Bread 2
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
#boostrap
beta.est.1.v<-c();gamma.est.1.v<-c()
alpha.est.1.v<-c()
AD.bv<-AD.2[,c(7,3,11,8)]
for(boot in 1:100){
  set.seed(boot)
  N.boot.a<-sample(1:(dim(AD.bv)[1]),replace=T)
  AD.bv.1<-AD.bv[N.boot.a,]
  #
  tryCatch({
  Lambda.1<-initial_Lambda.1(Time=AD.bv.1$diagnosis_age,Status=AD.bv.1$event,
                             id=c(1:dim(AD.bv.1)[1]),
                             X=AD.bv.1[,-c(1:3)],Z=AD.bv.1[,-c(1:3)],
                             corstr="independence")
  Lambda1<-Lambda.1$Lambda
  
  (alpha.01<-c(-sum(AD.bv.1$event)/sum(AD.bv.1$event*log(AD.bv.1$diagnosis_age)*(AD.bv.1$event-Lambda1))))
  },error=function(e){alpha.01<-0.5})
  #alpha.01<-0.5
  #
  #data.ob.1$U.logit<-factor(data.ob.1$cured)
  logit_model<-glm(factor(Fin.Dementia)~.,data = AD.bv.1[,-c(1:2)],family=binomial(link = "logit"))
  (gamma.00<-coef(logit_model))
  
  my_gmm.2<-gmm(moments.G1,x=AD.bv.1,t0=rep(0,2),type="iterative",
                method = "Nelder-Mead",control = list(reltol = 1e-20,maxit=20000))
  
  (beta.00<-my_gmm.2$coefficients)
  names(beta.00)<-c("Intercept",names(AD.bv.1)[-c(1:3)])
  names(beta.00)
  #
  K.1<-0
  para.test.0<-c(gamma.00,beta.00,alpha.01)
  gamma.storage<-c()
  beta.storage<-c()
  tryCatch({
    while(1){
      gamma.1<-matrix(gamma.00,ncol=1)
      beta.1<-matrix(beta.00,ncol=1)
      X.t.1<-cbind(1,AD.bv.1[,-c(1:3)])
      Z.t.1<-cbind(1,AD.bv.1[,-(1:3)])
      X.t.1<-as.matrix(X.t.1)
      Z.t.1<-as.matrix(Z.t.1)
      S3<-exp(-exp(X.t.1%*%beta.1)*(AD.bv.1$diagnosis_age^alpha.01))
      pi<-exp(Z.t.1%*%gamma.1)
      SC<-1
      #SC<-exp(-data.ob.1$Surv.time/29565)
      Wi<-AD.bv.1$event+((1-AD.bv.1$event)*pi*(S3)/(((1)*(SC))+(pi*S3)))
      AD.bv.1$Fin.Dementia<-Wi
      #
      my_gmm.t1<-gmm(moments.g.1,x=AD.bv.1,t0=gamma.00,type="iterative",wmatrix = "optimal",
                     method = "Nelder-Mead", control = list(reltol = 1e-25, maxit=20000))
      (gamma.01<-my_gmm.t1$coefficients)
      #
      my_gmm.t2<-gmm(moments.G1,x=AD.bv.1,t0=beta.00,type="iterative",wmatrix = "optimal",
                     method = "Nelder-Mead", control = list(reltol = 1e-25,maxit=20000))
      
      (beta.01<-my_gmm.t2$coefficients)
      Beta.b<-matrix(beta.01,ncol=1)
      my_gmm.t3<-gmm(moments.alpha,x=AD.bv.1,t0=c(alpha.01),optfct=c("nlminb"))
      (alpha.S1<-my_gmm.t3$coefficients)
      
      para.test<-c(gamma.01,beta.01,alpha.S1)
      ###
      if((max(abs((para.test)-(para.test.0)))<0.001)|K.1>100)
        break
      #LL.0<-LL.s
      gamma.00<-gamma.01
      K.1<-K.1+1
      para.test.0<-para.test
      print(para.test)
    }
  },error=function(e){c(gamma.01,beta.01,alpha.S1)})
  
  K.1
  (gamma.b.01<-gamma.01)

  (beta.b.01<-beta.01)

  (alpha.b.S1<-alpha.S1)
  #
  beta.est.1.v<-rbind(beta.est.1.v,beta.b.01)
  gamma.est.1.v<-rbind(gamma.est.1.v,gamma.b.01)
  alpha.est.1.v<-c(alpha.est.1.v,alpha.b.S1)
  #
}

SD.beta.boot<-sqrt(apply(beta.est.1.v,2,var))
SD.gamma.boot<-sqrt(apply(gamma.est.1.v,2,var))
SD.alpha.boot<-sqrt(var(alpha.est.1.v))
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

#LASSO
AD.3<-AD.2[,c(7,3,11,2,5,6,8:10)]

Lambda.1<-initial_Lambda.1(Time=AD.3$diagnosis_age,Status=AD.3$event,
                           id=c(1:dim(AD.3)[1]),
                           X=AD.3[,-c(1:3,5,7,8)],Z=AD.3[,-c(1:3)],
                           corstr="independence")
Lambda1<-Lambda.1$Lambda

(alpha.01<-c(-sum(data.ob.1$status)/sum(data.ob.1$status*log(data.ob.1$Surv.time)*(data.ob.1$status-Lambda1))))

alpha.01<-0.2
#
#incidence
U.logit.1<-rbinom(length(AD.3$Fin.Dementia),1,c(AD.3$Fin.Dementia))
lambda.m<-cv.glmnet(data.matrix(AD.3[,-c(1:3)]),U.logit.1,family=binomial,standardize = FALSE,alpha=1)
(gamma.00<-coef(lambda.m,s=lambda.m$lambda.min)[1:(dim(AD.3[,-c(1:3)])[2]+1)])
defult.lambda<-lambda.m$lambda.min

#bebias
#Z.t.1<-data.matrix(cbind(1,AD.3[,-c(1:3)]))
#gamma.tt<-matrix(gamma.00,ncol=1)
#first derivating
#pi<-exp(Z.t.1%*%gamma.tt)/(1+exp(Z.t.1%*%gamma.tt))
#pi.first<-sapply(1:(dim(Z.t.1)[1]),function(i){Z.t.1[i,]*(AD.3$Fin.Dementia[i]-pi[i])})
#pi.first.1<-matrix(apply(pi.first,1,sum),ncol=1)
#second derivating
#pi.sec<-0
#for(i in 1:dim(Z.t.1)[1]){
#  pi.sec.t<-(matrix(Z.t.1[i,],ncol=1)%*%matrix(Z.t.1[i,],nrow=1))*pi[i]*(1-pi[i])
#  pi.sec<-pi.sec+pi.sec.t}
#pi.sec.1<-(-pi.sec)/(dim(Z.t.1)[1])
#pi.sec.1.inv<-solve(pi.sec.1)
#gamma.1.debias<-gamma.tt-(pi.sec.1.inv%*%(pi.first.1))/(dim(Z.t.1)[1])
#gamma.00<-c(gamma.1.debias)
#
my_gmm.2<-gmm(moments.G1,x=AD.3,t0=rep(0,dim(AD.3[,-c(1:3)])[2]+1),type="iterative",
              wmatrix = "optimal",method = "Nelder-Mead",control = list(reltol = 1e-20,maxit=20000))
(beta.00<-my_gmm.2$coefficients)
names(beta.00)<-c("Intercept",names(AD.3)[-c(1:3)])
names(gamma.00)<-c("Intercept",names(AD.3)[-c(1:3)])
names(beta.00)
names(gamma.00)

#
K.1<-0
para.test.0<-c(beta.00,gamma.00,alpha.01)

while(1){
  gamma.1<-matrix(gamma.00,ncol=1)
  beta.1<-matrix(beta.00,ncol=1)
  X.t.1<-cbind(1,AD.3[,-c(1:3)])
  Z.t.1<-cbind(1,AD.3[,-c(1:3)])
  X.t.1<-as.matrix(X.t.1)
  Z.t.1<-as.matrix(Z.t.1)
  S3<-exp(-exp(X.t.1%*%beta.1)*(AD.3$diagnosis_age^alpha.01))
  pi<-exp(Z.t.1%*%gamma.1)
  SC<-1
  #SC<-exp(-data.ob.1$Surv.time/120)
  #SC<-exp(-(data.ob.1$Surv.time/350.16)^13.66)
  Wi<-AD.3$event+((1-AD.3$event)*pi*(S3)/(((1)*(SC))+(pi*S3)))
  AD.3$Fin.Dementia<-Wi
  #
  #model.lasso<-glmnet(data.matrix(AD.3[,-c(1:3)]),Wi,family=binomial,standardize = FALSE,lambda=defult.lambda,alpha=1)
  #gamma.01<-coef(model.lasso)[1:(dim(AD.3[,-c(1:3)])[2]+1)]
  lambda.m1<-cv.glmnet(data.matrix(AD.3[,-c(1:3)]),Wi,family=binomial,standardize = FALSE,alpha=1)
  (gamma.01<-coef(lambda.m1,s=lambda.m1$lambda.min)[1:(dim(AD.3[,-c(1:3)])[2]+1)])
  
  
  #bebias
  #Z.t.1<-data.matrix(cbind(1,AD.3[,-c(1:3)]))
  #gamma.tt<-matrix(gamma.01,ncol=1)
  #first derivating
  #pi<-exp(Z.t.1%*%gamma.tt)/(1+exp(Z.t.1%*%gamma.tt))
  #pi.first<-sapply(1:(dim(Z.t.1)[1]),function(i){Z.t.1[i,]*(AD.3$Fin.Dementia[i]-pi[i])})
  #pi.first.1<-matrix(apply(pi.first,1,sum),ncol=1)
  #second derivating
  #pi.sec<-0
  #for(i in 1:dim(Z.t.1)[1]){
  #  pi.sec.t<-(matrix(Z.t.1[i,],ncol=1)%*%matrix(Z.t.1[i,],nrow=1))*pi[i]*(1-pi[i])
  #  pi.sec<-pi.sec+pi.sec.t}
  #pi.sec.1<-(-pi.sec)/(dim(Z.t.1)[1])
  #pi.sec.1.inv<-solve(pi.sec.1)
  #gamma.1.debias<-gamma.tt-(pi.sec.1.inv%*%(pi.first.1))/(dim(Z.t.1)[1])
  #gamma.01<-c(gamma.1.debias)
  
  #Cox
  my_gmm.t2<-gmm(moments.G1,x=AD.3,t0=beta.00,type="iterative",wmatrix = "optimal",
                 method = "Nelder-Mead", control = list(reltol = 1e-25, maxit=20000))
  
  (beta.01<-my_gmm.t2$coefficients)
  #
  Beta.b<-matrix(beta.01,ncol=1)
  my_gmm.t3<-gmm(moments.alpha,x=AD.3,t0=c(alpha.01),optfct=c("nlminb"))
  (alpha.S1<-my_gmm.t3$coefficients)
  para.test<-c(beta.01,gamma.01,alpha.S1)
  ###
  if((max(abs(para.test-para.test.0))<0.01)|K.1>100|alpha.S1<0)
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
#
gamma.t.01<-gamma.01
beta.t.01<-beta.01
alpha.t.S1<-alpha.S1

#=======sandwitch-Louis
#Sandwitch
#
Z.t.1<-data.matrix(cbind(1,AD.3[,-c(1:3)]))
X.t.1<-data.matrix(cbind(1,AD.3[,-c(1:3)]))
gamma.1<-matrix(gamma.01,ncol=1)
beta.1<-matrix(beta.01,ncol=1)
pi<-exp(Z.t.1%*%gamma.1)/(1+exp(Z.t.1%*%gamma.1))
N<-dim(AD.3)[1]
#Bread 1
G1.sec<-0;B1.sec<-0;B2.sec<-0;A1.sec<-0;A2.sec<-0
for(i in 1:N){
  pi.sec.t<--(matrix(Z.t.1[i,],ncol=1)%*%matrix(Z.t.1[i,],nrow=1))*pi[i]*(1-pi[i])
  G1.sec<-G1.sec+pi.sec.t
  B1.sec.t<--AD.3$Fin.Dementia[i]*(matrix(X.t.1[i,],ncol=1)%*%matrix(X.t.1[i,],nrow=1))*(AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))
  B1.sec<-B1.sec+B1.sec.t
  B2.sec.t<--AD.3$Fin.Dementia[i]*matrix(X.t.1[i,],ncol=1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))*(AD.3$diagnosis_age[i]^alpha.t.S1)*log(AD.3$diagnosis_age[i])
  B2.sec<-B2.sec+B2.sec.t
  A1.sec.t<--AD.3$Fin.Dementia[i]*(AD.3$diagnosis_age[i]^alpha.t.S1)*log(AD.3$diagnosis_age[i])*matrix(X.t.1[i,],nrow=1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))
  A1.sec<-A1.sec+A1.sec.t
  A2.sec.t<--AD.3$Fin.Dementia[i]*((log(AD.3$diagnosis_age[i]))^2)*(AD.3$diagnosis_age[i]^alpha.t.S1)*c(exp(t(beta.1)%*%matrix(X.t.1[i,],ncol=1)))-(AD.3$event[i]/(alpha.t.S1^2))
  A2.sec<-A2.sec+A2.sec.t
}
Bread.t1<-cbind(G1.sec,matrix(0,nrow=dim(Z.t.1)[2],ncol=dim(X.t.1)[2]),matrix(0,nrow=dim(Z.t.1)[2],ncol=1))
Bread.t2<-cbind(matrix(0,nrow=dim(X.t.1)[2],ncol=dim(Z.t.1)[2]),B1.sec,B2.sec)
Bread.t3<-cbind(matrix(0,nrow=1,ncol=dim(Z.t.1)[2]),A1.sec,A2.sec)
BREAD<-rbind(Bread.t1,Bread.t2,Bread.t3)
#Bread 2
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
#CFIM
#(var.IF<-sqrt(diag(solve(-BREAD))))
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
#boostrap
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
    
    
    #bebias
    #Z.t.1<-data.matrix(cbind(1,data.ob.1[,-c(1:3)]))
    #gamma.tt<-matrix(gamma.01,ncol=1)
    #first derivating
    #pi<-exp(Z.t.1%*%gamma.tt)/(1+exp(Z.t.1%*%gamma.tt))
    #pi.first<-sapply(1:(dim(Z.t.1)[1]),function(i){Z.t.1[i,]*(data.ob.1$cured[i]-pi[i])})
    #pi.first.1<-matrix(apply(pi.first,1,sum),ncol=1)
    #second derivating
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
  #
  gamma.t.01<-gamma.01
  beta.t.01<-beta.01
  alpha.t.S1<-alpha.S1
  #
  beta.est.1.v<-rbind(beta.est.1.v,beta.t.01)
  gamma.est.1.v<-rbind(gamma.est.1.v,gamma.t.01)
  alpha.est.1.v<-c(alpha.est.1.v,alpha.t.S1)
 #
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


#============Test=================================



moments.g.1<-function(theta,data){
  y<-matrix(data[,1],ncol=1)
  Q<-data.matrix(data[,-1])
  theta.1<-matrix(theta,ncol=1)
  Q<-cbind(1,Q)
  mu<-exp(Q%*%theta.1)
  G<-c(y-(mu/(1+mu)))*Q
  return(G)
}

X<-rnorm(500,5,1)

y<-c()
for(i in 1:500){
  y.1<-exp(0.3+2*X[i])/(1+exp(0.3+2*X[i]))
  y<-c(y,y.1)
}

D.1<-data.frame(Y=y,XX=X)
GLM.logit<-glm(Y~XX,data=D.1,binomial(link = "logit"))
summary(GLM.logit)


my_gmm.t1<-gmm(moments.g.1,x=D.1,t0=c(0,0),type="iterative",wmatrix = "optimal",
               method = "Nelder-Mead", control = list(reltol = 1e-25, maxit=20000))
(gamma.01<-my_gmm.t1$coefficients)
