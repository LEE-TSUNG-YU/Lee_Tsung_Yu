location <- getwd()
setwd(location)

source('function.R', encoding="utf-8")

# 對每一艘船的亞洲到美洲(_toE)與美洲到亞洲(_toW)配適模型
for (m in 1:6){
  for (n in c("_toE.csv", "_toW.csv")){
    
#pre-setting
file_list <- c("9337482.csv", "9462691.csv", "9462706.csv", "9462718.csv", "9462720.csv", "9462732.csv")
boat_list <- c("9337482", "9462691", "9462706", "9462718", "9462720", "9462732")

# 是("9337482", "9462691", "9462706", "9462718", "9462720", "9462732") 裡的第幾艘船?
boat_index <- m

Ship <- read.csv(paste0("IMS/",boat_list[boat_index],'.csv'))
# Ship <- Ship[,c(1:8,15:27)]

Ship[,"MassFlowRate_D"] <- Ship[,"M_EInletMassFlowRate"] - Ship[,"M_EOutletMassFlowRate"]
Ship[,"ShipSpeed_D"] <- Ship[,"GroundSpeed"] - Ship[,"WaterSpeed"]
# 整數換成小數
Ship[,"Power"] <- as.numeric(unlist(Ship[,"Power"]))

CFD <- read.csv("CFD8600/CFD8600.csv")
for (a in unique(Ship$Trim)) {
  for (b in unique(Ship$Draft)) {
    if (length(which(Ship$Trim==a & Ship$Draft==b))==0) {
      next
    }
    Ship[which(Ship$Trim==a & Ship$Draft==b),"Power"] <- Ship[which(Ship$Trim==a & Ship$Draft==b)[1],c("Draft", "Trim")] %>%
      as.data.frame %>% 
      apply(MARGIN = 1, FUN = correction_fun, CFD = CFD) %>%
      as.numeric() * Ship[which(Ship$Trim==a & Ship$Draft==b),"Power"]
  }
}

route <- read.csv(paste0("voyage3/", boat_list[boat_index], n))
result <- matrix(NA, ncol = (dim(route)[1] + 3), nrow = dim(route)[1]) %>% as.data.frame
colnames(result) <- c(paste('voyage', 1:dim(route)[1], sep = '_'), "variable", "pca", "arma")
rownames(result) <- paste('voyage', 1:dim(route)[1], sep = '_')

for (i in 1:(dim(route)[1]-1)){
  print(i)
  voyage1 <- (route[i,1] %>% as.numeric()):(route[i,2] %>% as.numeric())
  {
    # 第一段（基準值） ------------------------------------------------------------------------------
    Ship1 <- Ship[voyage1, c("Time", "Torque", "EngineSpeed", "Power", "WaterSpeed", "Trim", "Draft", "MassFlowRate_D")]
    if (!(class(Ship1[,"Time"]) %in% c('integer', 'numeric'))){
      Ship1[,"Time"] <- 1:dim(Ship1)[1]
    }
    Ship1 <- Ship1[which(Ship1$Power>0), ]
    Ship1 <- Ship1[which(Ship1$Torque>0), ]
    Ship1 <- Ship1[which(Ship1$EngineSpeed>0), ]
    Ship1 <- Ship1[which(Ship1$WaterSpeed>0), ]
    Ship1 <- na.omit(Ship1)
    
    Ship1 <- Ship1[, !(colnames(Ship1) %in% c("Trim", "Draft"))]
    
    ### PCA
    # doPca <- doPCA(Ship1 = as.data.frame(Ship1))
    doPca <- c(2:5)
    usePca <- as.data.frame(Ship1)[,doPca]
    pca <- prcomp(formula = ~ . , data=usePca, scale = F) 
    pca.data <- as.data.frame(Ship1)[,doPca]
    z1 <- (as.matrix(pca.data) %*% pca$rotation)[,1]
    
    #生成PC
    Ship1[,"V"] <- z1
    
    #選取至超過90%
    k=2
    while (summary(pca)$importance[3, k-1] < 0.9) {
      z <- (as.matrix(pca.data) %*% pca$rotation)[, k]
      
      Ship1[, paste0("V", k)] <- z
      
      k <- k+1
    }
    
    #配適模型
    y <- Ship1[,"MassFlowRate_D"]
    
    Ship2 <- Ship1[, !(colnames(Ship1) %in% colnames(Ship1)[doPca])]
    
    Ship2 <- Ship2[, !(colnames(Ship2) %in% c("MassFlowRate_D"))] %>% as.matrix()
    
    #--------------------------------------
    while (TRUE){
      if (ncol(Ship2)==0){
        time.series <- auto.arima_fun(y = y, seasonal = FALSE)
        break
      }else{
        time.series <- auto.arima_fun(y = y, Ship2 = Ship2)
        nrows <- nrow(coeftest(time.series))
        if ('Time' %in% colnames(Ship2)){
          if (coeftest(time.series)[nrows - ncol(Ship2) + which(colnames(Ship2) %in% 'Time'),1] > 0){
            save_var_name <- colnames(Ship2)[!(colnames(Ship2) %in% 'Time')]
            Ship2 <- Ship2[, !(colnames(Ship2) %in% 'Time')] %>% as.matrix()
            colnames(Ship2) <- save_var_name
            next
          }
        }
        if (sum(coeftest(time.series)[(nrows-ncol(Ship2)+1):nrows, 4]>0.05)>0){
          drop_var_name <- row.names(coeftest(time.series))[(nrows-ncol(Ship2)+1):nrows][which.max(coeftest(time.series)[(nrows-ncol(Ship2)+1):nrows, 4])]
          save_var_name <- row.names(coeftest(time.series))[(nrows-ncol(Ship2)+1):nrows][-which.max(coeftest(time.series)[(nrows-ncol(Ship2)+1):nrows, 4])]
          Ship2 <- Ship2[, !(colnames(Ship2) %in% drop_var_name)] %>% as.matrix()
          colnames(Ship2) <- save_var_name
        }else{
          break
        }
      }
    }
    result[i, "variable"] <- paste(colnames(Ship2), collapse = ",")
    result[i, "pca"] <- paste(names(pca.data), collapse = ",")
    result[i, "arma"] <- paste(time.series[["arma"]], collapse = ",")
  }
  
  # # 因為forecast()需要Arima function產出的結果，不能直接用arima的結果代入，所以新建一個模型，order使用arima得到的結果。
  # ts.model.forecast <- Arima(y,order=time.series$order, xreg=as.matrix(Ship2), method="ML")
  # # 因為使用了兩種不同的function，所以要檢查兩個function得到的結果是否一致
  # paste0("In voyage ", i, ", two different function have a different result: ", isTRUE(sum(predict(ts.model.forecast, n.ahead = 50, newxreg = head(as.matrix(Ship2), 50), se.fit = TRUE)$pred != predict(time.series$ARIMA, n.ahead = 50, newxreg = head(Ship2, 50), se.fit = TRUE)$pred))) %>% print()
  
  col_names <- colnames(Ship2)
  j_vector <- 1:(dim(route)[1])
  j_vector <- j_vector[!(j_vector %in% i)]
  for (j in j_vector){
    print(j)
    voyage2 <- (route[j,1] %>% as.numeric()) : (route[j,2] %>% as.numeric())
    {
      ## 第二段 ----------------------------------------------------------------------------
      Ship1 <- Ship[voyage2, c("Time", "Torque", "EngineSpeed", "Power", "WaterSpeed", "Trim", "Draft", "MassFlowRate_D")]
      if (!(class(Ship1[,"Time"]) %in% c('integer', 'numeric'))){
        Ship1[,"Time"] <- 1:dim(Ship1)[1]
      }
      Ship1 <- Ship1[which(Ship1$Power>0), ]
      Ship1 <- Ship1[which(Ship1$Torque>0), ]
      Ship1 <- Ship1[which(Ship1$EngineSpeed>0), ]
      Ship1 <- Ship1[which(Ship1$WaterSpeed>0), ]
      Ship1 <- na.omit(Ship1)
      
      Ship1 <- Ship1[, !(colnames(Ship1) %in% c("Trim", "Draft"))]
      
      # 因為非第一輪，故直接使用第一次的模型帶入
      pca.data <- as.data.frame(Ship1)[,doPca]
      z1 <- (as.matrix(pca.data) %*% pca$rotation)[,1]
      
      #生成PC
      Ship1[,"V"] <- z1
      
      #選取至超過90%
      k=2
      while (summary(pca)$importance[3, k-1] < 0.9) {
        z <- (as.matrix(pca.data) %*% pca$rotation)[, k]
        
        Ship1[, paste0("V", k)] <- z
        
        k <- k+1
      }
      
      #配適模型
      y <- Ship1[,"MassFlowRate_D"]
      
      Ship2 <- Ship1[, !(colnames(Ship1) %in% colnames(Ship1)[doPca])]
      
      Ship2 <- Ship2[, !(colnames(Ship2) %in% c("MassFlowRate_D"))] %>% as.matrix()
      
      #--------------------------------------
      if (is.null(time.series$xreg)){
        pre <- predict(object = time.series, n.ahead = dim(Ship2)[1])
      }else{
        Ship2 <- Ship2[, col_names] %>% as.matrix()
        colnames(Ship2) <- col_names
        pre <- predict(object = time.series, n.ahead = dim(Ship2)[1], newxreg = as.data.frame(Ship2))
      }
    }
    #----------------------------------------------------------
    result[i,j] <- paste0(round(mean(pre$pred, na.rm = TRUE), 2), "/", 
                          round(mean(y, na.rm = TRUE), 2), "(", 
                          round(mean(abs(((y-pre$pred)/y)), na.rm = TRUE)*100, 2), "%)")
    
    # forecast.result <- forecast(object = ts.model.forecast, h = dim(Ship2)[1], xreg = as.matrix(Ship2), level = 0.99) %>% as.data.frame()
    # forecast.result <- forecast.result[,c("Point Forecast", "Lo 99", "Hi 99")]
    # forecast.result[, c("True Value")] <- y
    
    
    # filter_fun <- function(Ship){
    #   df <- Ship[voyage2, c("Time", "Torque", "EngineSpeed", "Power", "WaterSpeed", "Trim", "Draft", "MassFlowRate_D")]
    #   df <- df[which(df$Power>0), ]
    #   df <- df[which(df$Torque>0), ]
    #   df <- df[which(df$EngineSpeed>0), ]
    #   df <- df[which(df$WaterSpeed>0), ]
    #   df <- na.omit(df)
    #   return(df)
    # }
    # df <- filter_fun(Ship = Ship)
    # forecast.result[, c("Time")] <- df$Time
    
    # write.csv(forecast.result, paste0("C:/Users/peterchen/Desktop/output/", boat_list[m], "_", i,"_",j, n), row.names = FALSE)
  }
}
result
write.csv(result, paste0("R_output2/", boat_list[m], n))

  }
}
