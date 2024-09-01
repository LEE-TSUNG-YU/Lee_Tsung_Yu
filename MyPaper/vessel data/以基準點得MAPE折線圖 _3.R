file_name_vec <- c("9337482.csv", "9462720.csv", "9462732.csv")
boat_name_vec <- c("9337482", "9462720", "9462732")

for (file_name in boat_name_vec){
  for (n in c("_toE.csv", "_toW.csv")){
    df <- read.csv(paste0("D:/20200114/python20210324/after_delete/", file_name, n))
    n <- dim(df)[1]
    print(paste(df$start_time[1], df$end_time[1], sep = "\n~"))
    print(paste(df$start_time[2:n], df$end_time[2:n], sep = "\n~"))
    print('')
    print('')
  }
}
rm(list = ls())

#English
{
  # 9337482
  # E
  Ship.ARIMA <- c(2.63)
  Ship.LSTM <-  c(1.78)
  
  png(paste0("C:/Users/peterchen/Desktop/以Baseline date得MAPE折線圖/9337482_E_ENG.png"),width = 800,height = 600)
  par(mar = c(5.7, 4.1, 3.5, 2.1))
  plot(Ship.ARIMA, type = "b", xlab = "", ylab = "MAPE",
       xaxt = "n", pch = 1, xlim = c(0.85,length(Ship.ARIMA)), ylim = c(0, max(20, max(Ship.ARIMA, Ship.LSTM))),
       main = "Baseline date 2018/11/11~2018/11/28", cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5)
  points(Ship.LSTM, type = "b", pch = 1, cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5, col = "red")
  axis(side = 1, at = 1:length(Ship.ARIMA), labels = rep("", length(Ship.ARIMA)), cex.axis = 0.75, srt = 45)
  text(x = 1:length(Ship.ARIMA), y = -2.75, labels = c("2019/06/08\n~2019/06/26"), 
       cex = 1.2, xpd=TRUE, srt=90)
  abline(10, 0, lty = 2)
  abline(20, 0, lty = 2)
  # points(which(ShipMAPE > 10 & ShipMAPE <20), ShipMAPE[which(ShipMAPE > 10 & ShipMAPE <20)],
  #        col = "blue", pch = 16, cex = 3)
  # points(which(ShipMAPE > 20), ShipMAPE[which(ShipMAPE > 20)],
  #        col = "red", pch = 16, cex = 3)
  
  lines(x = rep(0.9, 2), y = c(-100, 100), lwd = 2, col = "red")
  text(1.3, 18, "Dry-docking\n(2018/10/24)", cex = 2, font = 100)
  legend("topright", c("Regression with ARIMA error", "LSTM-FCN"), col = c("black", "red"), lty = 1, lwd = 2, cex = 2)
  dev.off()
  
  # W
  Ship.ARIMA <- c(10.36, 9.11)
  Ship.LSTM <-  c(2.27, 2.09)
  
  png(paste0("C:/Users/peterchen/Desktop/以Baseline date得MAPE折線圖/9337482_W_ENG.png"),width = 800,height = 600)
  par(mar = c(5.7, 4.1, 3.5, 2.1))
  plot(Ship.ARIMA, type = "b", xlab = "", ylab = "MAPE",
       xaxt = "n", pch = 1, xlim = c(0.85,length(Ship.ARIMA)), ylim = c(0, max(20, max(Ship.ARIMA, Ship.LSTM))),
       main = "Baseline date 2018/12/18\n~2019/01/06", cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5)
  points(Ship.LSTM, type = "b", pch = 1, cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5, col = "red")
  axis(side = 1, at = 1:length(Ship.ARIMA), labels = rep("", length(Ship.ARIMA)), cex.axis = 0.75, srt = 45)
  text(x = 1:length(Ship.ARIMA), y = -2.75, labels = c("2019/05/06\n~2019/05/28", "2019/07/22\n~2019/08/08"), 
       cex = 1.2, xpd=TRUE, srt=90)
  abline(10, 0, lty = 2)
  abline(20, 0, lty = 2)
  # points(which(ShipMAPE > 10 & ShipMAPE <20), ShipMAPE[which(ShipMAPE > 10 & ShipMAPE <20)],
  #        col = "blue", pch = 16, cex = 3)
  # points(which(ShipMAPE > 20), ShipMAPE[which(ShipMAPE > 20)],
  #        col = "red", pch = 16, cex = 3)
  
  lines(x = rep(0.9, 2), y = c(-100, 100), lwd = 2, col = "red")
  text(1.3, 18, "Dry-docking\n(2018/10/24)", cex = 2, font = 100)
  legend("topright", c("Regression with ARIMA error", "LSTM-FCN"), col = c("black", "red"), lty = 1, lwd = 2, cex = 2)
  dev.off()
  
  # 9462720
  # E
  Ship.ARIMA <- c(6.76, 6.41, 8.62, 4.40, 4.13)
  Ship.LSTM <-  c(8.41, 13.08, 9.97, 7.28, 11.14)
  
  png(paste0("C:/Users/peterchen/Desktop/以Baseline date得MAPE折線圖/9462720_E_ENG.png"),width = 800,height = 600)
  par(mar = c(5.7, 4.1, 3.5, 2.1))
  plot(Ship.ARIMA, type = "b", xlab = "", ylab = "MAPE",
       xaxt = "n", pch = 1, xlim = c(0.85,length(Ship.ARIMA)), ylim = c(0, max(20, max(Ship.ARIMA, Ship.LSTM))),
       main = "Baseline date 2019/5/22\n~2019/6/3", cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5)
  points(Ship.LSTM, type = "b", pch = 1, cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5, col = "red")
  axis(side = 1, at = 1:length(Ship.ARIMA), labels = rep("", length(Ship.ARIMA)), cex.axis = 0.75, srt = 45)
  text(x = 1:length(Ship.ARIMA), y = -10, labels = c("2019/08/16\n~2019/08/26", "2019/09/29\n~2019/10/11", "2019/11/13\n~2019/11/22", 
                                                     "2020/01/28\n~2020/02/09", "2020/04/07\n~2020/04/20"), 
       cex = 1.2, xpd=TRUE, srt=90)
  abline(10, 0, lty = 2)
  abline(20, 0, lty = 2)
  # points(which(ShipMAPE > 10 & ShipMAPE <20), ShipMAPE[which(ShipMAPE > 10 & ShipMAPE <20)],
  #        col = "blue", pch = 16, cex = 3)
  # points(which(ShipMAPE > 20), ShipMAPE[which(ShipMAPE > 20)],
  #        col = "red", pch = 16, cex = 3)
  
  lines(x = rep(0.9, 2), y = c(-100, 100), lwd = 2, col = "red")
  text(1.35, 66, "Propeller\ncleaning\n(2019/2/20)", cex = 1.8, font = 100)
  legend("topright", c("Regression with ARIMA error", "LSTM-FCN"), col = c("black", "red"), lty = 1, lwd = 2, cex = 2)
  dev.off()
  
  # W
  Ship.ARIMA <- c(4.88, 5.70, 2.96, 5.66, 2.31, 7.56)
  Ship.LSTM <-  c(24.79, 22.97, 20.45, 5.98, 7.29, 10.39)
  
  png(paste0("C:/Users/peterchen/Desktop/以Baseline date得MAPE折線圖/9462720_W_ENG.png"),width = 800,height = 600)
  par(mar = c(5.7, 4.1, 3.5, 2.1))
  plot(Ship.ARIMA, type = "b", xlab = "", ylab = "MAPE",
       xaxt = "n", pch = 1, xlim = c(0.85,length(Ship.ARIMA)), ylim = c(0, max(20, max(Ship.ARIMA, Ship.LSTM))),
       main = "Baseline date 2019/03/12\n~2019/03/26", cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5)
  points(Ship.LSTM, type = "b", pch = 1, cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5, col = "red")
  axis(side = 1, at = 1:length(Ship.ARIMA), labels = rep("", length(Ship.ARIMA)), cex.axis = 0.75, srt = 45)
  text(x = 1:length(Ship.ARIMA), y = -2.75, labels = c("2019/04/30\n~2019/05/07", "2019/09/01\n~2019/09/11", "2019/10/13\n~2019/10/27", 
                                                       "2020/02/27\n~2020/03/13", "2020/04/29\n~2020/05/15", "2020/07/07\n~2020/07/20"), 
       cex = 1.2, xpd=TRUE, srt=90)
  abline(10, 0, lty = 2)
  abline(20, 0, lty = 2)
  # points(which(ShipMAPE > 10 & ShipMAPE <20), ShipMAPE[which(ShipMAPE > 10 & ShipMAPE <20)],
  #        col = "blue", pch = 16, cex = 3)
  # points(which(ShipMAPE > 20), ShipMAPE[which(ShipMAPE > 20)],
  #        col = "red", pch = 16, cex = 3)
  
  lines(x = rep(0.9, 2), y = c(-100, 100), lwd = 2, col = "red")
  text(1.05, 18, "Propeller\ncleaning\n(2018/2/14)", cex = 2, font = 100)
  legend("topright", c("Regression with ARIMA error", "LSTM-FCN"), col = c("black", "red"), lty = 1, lwd = 2, cex = 2)
  dev.off()
  
  # 9462732
  # E
  Ship.ARIMA <- c(18.86, 18.38, 18.79, 17.58, 8.91, 8.91)
  Ship.LSTM <-  c(19.13, 6.66, 6.22, 7.32, 8.29, 3.43)
  
  png(paste0("C:/Users/peterchen/Desktop/以Baseline date得MAPE折線圖/9462732_E_ENG.png"),width = 800,height = 600)
  par(mar = c(5.7, 4.1, 3.5, 2.1))
  plot(Ship.ARIMA, type = "b", xlab = "", ylab = "MAPE",
       xaxt = "n", pch = 1, xlim = c(0.85,length(Ship.ARIMA)), ylim = c(0, max(Ship.ARIMA, Ship.LSTM)+8),
       main = "Baseline date 2019/02/13\n~2019/02/24", cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5)
  points(Ship.LSTM, type = "b", pch = 1, cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5, col = "red")
  axis(side = 1, at = 1:length(Ship.ARIMA), labels = rep("", length(Ship.ARIMA)), cex.axis = 0.75, srt = 45)
  text(x = 1:length(Ship.ARIMA), y = -4.0, labels = c("2019/05/27\n~2019/06/11", "2019/07/11\n~2019/07/23", "2019/10/05\n~2019/10/18", 
                                                      "2019/11/21\n~2019/12/05", "2020/02/14\n~2020/02/28", "2020/06/10\n~2020/06/24"),
       cex = 1.2, xpd=TRUE, srt=90)
  abline(10, 0, lty = 2)
  abline(20, 0, lty = 2)
  # points(which(ShipMAPE > 10 & ShipMAPE <20), ShipMAPE[which(ShipMAPE > 10 & ShipMAPE <20)],
  #        col = "blue", pch = 16, cex = 3)
  # points(which(ShipMAPE > 20), ShipMAPE[which(ShipMAPE > 20)],
  #        col = "red", pch = 16, cex = 3)
  
  lines(x = rep(0.9, 2), y = c(-100, 100), lwd = 2, col = "red")
  text(1.8, 26, "Propeller\ncleaning\n(2019/2/13)", cex = 2, font = 100)
  legend("topright", c("Regression with ARIMA error", "LSTM-FCN"), col = c("black", "red"), lty = 1, lwd = 2, cex = 2)
  dev.off()
  
  # W
  Ship.ARIMA <- c(10.15, 8.57, 9.46, 4.34, 4.00, 6.82)
  Ship.LSTM <-  c(9.40, 11.47, 6.21, 3.86, 5.78, 4.27)
  
  png(paste0("C:/Users/peterchen/Desktop/以Baseline date得MAPE折線圖/9462732_W_ENG.png"),width = 800,height = 600)
  par(mar = c(5.7, 4.1, 3.5, 2.1))
  plot(Ship.ARIMA, type = "b", xlab = "", ylab = "MAPE",
       xaxt = "n", pch = 1, xlim = c(0.85,length(Ship.ARIMA)), ylim = c(0, max(Ship.ARIMA, Ship.LSTM)+6),
       main = "Baseline date 2019/06/17\n~2019/07/01", cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5)
  points(Ship.LSTM, type = "b", pch = 1, cex.axis = 1.8, las=1, cex=3, cex.main = 2, cex.lab = 1.5, col = "red")
  axis(side = 1, at = 1:length(Ship.ARIMA), labels = rep("", length(Ship.ARIMA)), cex.axis = 0.75, srt = 45)
  text(x = 1:length(Ship.ARIMA), y = -3.65, labels = c("2019/07/30\n~2019/08/15", "2019/09/10\n~2019/09/26", "2019/10/24\n~2019/11/07", 
                                                       "2020/01/24\n~2020/02/03", "2020/03/06\n~2020/03/17", "2020/04/17\n~2020/04/29"),
       cex = 1.2, xpd=TRUE, srt=90)
  abline(10, 0, lty = 2)
  abline(20, 0, lty = 2)
  # points(which(ShipMAPE > 10 & ShipMAPE <20), ShipMAPE[which(ShipMAPE > 10 & ShipMAPE <20)],
  #        col = "blue", pch = 16, cex = 3)
  # points(which(ShipMAPE > 20), ShipMAPE[which(ShipMAPE > 20)],
  #        col = "red", pch = 16, cex = 3)
  
  lines(x = rep(0.9, 2), y = c(-100, 100), lwd = 2, col = "red")
  text(1.8, 24, "Propeller\ncleaning\n(2019/2/13)", cex = 2, font = 100)
  legend("topright", c("Regression with ARIMA error", "LSTM-FCN"), col = c("black", "red"), lty = 1, lwd = 2, cex = 2)
  dev.off()
}