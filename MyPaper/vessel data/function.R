# library(Please install first)
library(forecast)
library(lmtest)
library(lattice)
library(tidyverse)
# library(readxl)
# library(data.table)

# function
cor.levelplot_fun <- function(data,boat_name="", start_location_name="", end_location_name="", cex=0.5){
  if (sum(colnames(data)=="Latitude")==1){
    data=data[,-which(colnames(data)=="Latitude")]
  }
  if (sum(colnames(data)=="Longtitude")==1){
    data=data[,-which(colnames(data)=="Longtitude")]
  }
  if (sum(colnames(data)=="Draft")==1){
    data=data[,-which(colnames(data)=="Draft")]
  }
  if (sum(colnames(data)=="Trim")==1){
    data=data[,-which(colnames(data)=="Trim")]
  }
  data <- na.omit(data)
  n <- dim(data)[1]
  p <- dim(data)[2]
  plot1 <- levelplot(cor(data, method = "pearson"),
                     col.regions=heat.colors(100),
                     main=paste0("correlation of ",boat_name,"","從",start_location_name,"到",end_location_name),xlab="",ylab="",
                     scales=list(x=list(at=seq(1,p),labels=colnames(data),rot=45,cex=1),y=list(cex=1)),
                     panel=function(...) {
                       arg <- list(...)
                       panel.levelplot(...)
                       panel.text(rep(seq(1,p),p),rep(seq(1,p),each=p),round(cor(data,method="pearson"),2),cex=cex)
                     })
  return(list(cor.data = cor(data,method="pearson"),plot = plot1))
}

cor.test.levelplot_fun <- function(data, boat_name="", start_location_name="", end_location_name="", cex=0.5){
  if (sum(colnames(data)=="Latitude")==1){
    data=data[,-which(colnames(data)=="Latitude")]
  }
  if (sum(colnames(data)=="Longtitude")==1){
    data=data[,-which(colnames(data)=="Longtitude")]
  }
  if (sum(colnames(data)=="Draft")==1){
    data=data[,-which(colnames(data)=="Draft")]
  }
  if (sum(colnames(data)=="Trim")==1){
    data=data[,-which(colnames(data)=="Trim")]
  }
  data <- na.omit(data)
  n <- dim(data)[1]
  p <- dim(data)[2]
  cor.test.pvalue <- matrix(NA, ncol = p, nrow = p)
  for (i in 1:p){
    for (j in 1:p){
      cor.test.pvalue[i,j] <- cor.test(x = data[,i], y = data[,j], alternative = "two.sided", method = "pearson", exact = TRUE)$p.value
    }
  }
  #cor.test.pvalue=matrix(p.adjust(p = as.numeric(cor.test.pvalue),method = "bonferroni"),ncol=p,nrow=p)
  dimnames(cor.test.pvalue) <- list(colnames(data), colnames(data))
  plot1 <- levelplot(cor.test.pvalue,
                     col.regions=heat.colors(100),
                     main=paste0("cor.test of ",boat_name,"","從",start_location_name,"到",end_location_name),xlab="",ylab="",
                     scales=list(x=list(at=seq(1,p),labels=colnames(data),rot=45,cex=1),y=list(cex=1)),
                     panel=function(...) {
                       arg <- list(...)
                       panel.levelplot(...)
                       panel.text(rep(seq(1,p),p),rep(seq(1,p),each=p),round(cor.test.pvalue,2),cex=cex)
                     })
  return(list(cor.pvalue = cor.test.pvalue, plot = plot1))
}

data.discription <- function(data){
  if (sum(colnames(data)=="Latitude")==1){
    data <- data[,-which(colnames(data)=="Latitude")]
  }
  if (sum(colnames(data)=="Longtitude")==1){
    data <- data[,-which(colnames(data)=="Longtitude")]
  }
  n <- dim(data)[1]
  p <- dim(data)[2]
  df <- matrix(NA, ncol = 4, nrow = p)
  dimnames(df) <- list(colnames(data), c("平均數","標準差","最小值","最大值"))
  for (i in 1:p){
    x_i <- data[,i]
    df[i,] <- c(x_i %>% na.omit %>% mean, 
                x_i%>% na.omit %>% sd, 
                x_i %>% na.omit %>% min, 
                x_i %>% na.omit %>% max)
  }
  return(df)
}

doPCA <- function(Ship1, alpha = 2.2e-16){
  ### This function is used to get which variable is correlated to "Power"
  ### parameters
  # 1) Ship1: The data we want to do the cor.test
  # 2) alpha: The Upper Bound of sum of p-value of cor.test
  ### output
  # 1) doPca: The index of the variable which is correlated to "Power"(included "Power"). Ex: c(4,1,2,3,5,6)
  doPca <- which(colnames(Ship1)=="Power")
  sumPvalue <- 0
  while (sumPvalue < alpha) {
    min <- 1
    for (a in 1:length(Ship1[1,])) {
      add <- NA
      if (a != which(colnames(Ship1)=="MassFlowRate_D")) {
        if (sum(a == doPca)==0) {
          b <- length(doPca) + 1 
          CorPvalue <- matrix(rep(NA,b*b),b)
          colnames(CorPvalue) <- colnames(Ship1) [c(doPca , a)]
          row.names(CorPvalue) <- colnames(Ship1)[c(doPca , a)]
          for (i in c(doPca , a)) {
            for (j in c(doPca , a)) {
              CorPvalue[which(i== c(doPca , a)),which(j== c(doPca , a))] <- cor.test(Ship1[,j],Ship1[,i],method = "pearson",exact = T)$p.value
            }
          }
          now <- sum(CorPvalue)/2
          if (now <= min) {
            min <- now
            add <- a
          }
        }
      }
      if (is.na(add) == F){
        doPca <- c(doPca, add)
      }
      sumPvalue <- min
    }
  }
  return(doPca)
}

get_order_arima <- function(y, Ship2){
  out <- F
  use <- F
  coef <- 123
  for (d in c(0,1)) {
    for (p in c(1:10,0)) {
      for (q in c(1:10,0)) {
        if (sum(c(p,d,q) == c(0,0,0)) == 3) {
          next()
        }
        try(ARIMA <- arima(y,order=c(p,d,q), xreg=Ship2, method="ML"))
        
        try(coef <- matrix(coeftest(ARIMA), ncol = 4))
        if (length(coef) != 1){
          if (sum(is.na(coef))==0) {
            pdq <- c(p,d,q)
            out <- T
            use <- T
            break
          }
        }
      }
      if (out == T) break
    }
    if (out == T) break
  }
  
  if (sum(pdq == 0)==3) {
    ARIMA <- "There is no result"
  }
  return(ARIMA)
}

get_order_arima_without_xreg <- function(y){
  out <- F
  use <- F
  for (d in c(1,0)) {
    for (p in c(1:10,0)) {
      for (q in c(1:10,0)) {
        try(ARIMA <- arima(y,order=c(p,d,q), method="ML"))
        
        coef <- matrix(coeftest(ARIMA),ncol = 4)
        if (sum(is.na(coef))==0) {
          pdq <- c(p,d,q)
          out <- T
          use <- T
          break
        }
      }
      if (out == T) break
    }
    if (out == T) break
  }
  
  if (sum(pdq == 0)==3) {
    ARIMA <- "There is no result"
  }
  return(ARIMA)
}

correction_fun <- function(CFD, df){
  library(tidyverse)
  Draft <- df[1] %>% as.numeric()
  Trim <- df[2] %>% as.numeric()
  
  Draft2 <- unique(CFD$Draft)[sum(Draft >= unique(CFD$Draft)) + 1]
  Draft1 <- unique(CFD$Draft)[sum(Draft >= unique(CFD$Draft))]
  # make sure that Draft1 and Draft2 are not numeric(0)
  if (length(Draft1) == 0){
    Draft1 <- 7.0
    Draft2 <- 8.5
  }else if (is.na(Draft2)){
    Draft1 <- 13.0
    Draft2 <- 14.5
  }
  
  Trim22 <- Trim12 <- unique(CFD$Trim)[sum(Trim >= unique(CFD$Trim)) + 1]
  Trim21 <- Trim11 <- unique(CFD$Trim)[sum(Trim >= unique(CFD$Trim))]
  # make sure that Trim11, Trim12, Trim21, Trim22 are not numeric(0)
  if (length(Trim11) == 0){
    Trim21 <- Trim11 <- -2.0
    Trim22 <- Trim12 <- -1.0
  }else if (is.na(Trim22)){
    Trim21 <- Trim11 <- 3.5
    Trim22 <- Trim12 <- 4.5
  }
  
  Trim_Draft_get_PE <- function(df, Draft0, Trim0){
    df %>% 
      dplyr::filter(Draft == Draft0 & Trim == Trim0) %>% 
      select(PE) %>% 
      unlist() %>%
      as.numeric %>%
      mean()
  }
  
  PE22 <- Trim_Draft_get_PE(CFD, Draft2, Trim22)
  PE21 <- Trim_Draft_get_PE(CFD, Draft2, Trim21)
  PE12 <- Trim_Draft_get_PE(CFD, Draft1, Trim12)
  PE11 <- Trim_Draft_get_PE(CFD, Draft1, Trim11)
  
  PE_2_1.5 <- (PE22 - PE21) / (Trim22 - Trim21) * (Trim - Trim21) + PE21
  PE_1_1.5 <- (PE12 - PE11) / (Trim12 - Trim11) * (Trim - Trim11) + PE11
  PE_1.5_1.5 <- (PE_2_1.5 - PE_1_1.5) / (Draft2 - Draft1) * (Draft - Draft1) + PE_1_1.5
  correction <- PE_1.5_1.5 / 18686.81  #mean of PE under "Draft = 11.5"
  return(correction)
}

auto.arima_fun <- function(y, Ship2){
  try(t.s <- auto.arima(y = y, xreg = Ship2, seasonal = FALSE, allowdrift = FALSE))
  if (exists("t.s")){
    return(t.s)
  }else{
    t.s <- get_order_arima(y = y, Ship2 = Ship2)
  }
  return(t.s)
}