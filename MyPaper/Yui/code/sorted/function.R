# 1. get initial values of coefficients
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