# 1.
set.seed(100)
old.par =par(mfrow=c(2,1) , mar=c(2,3,5,1) , mgp = c(3 , 0.5 , 0))
n.sim = 10000
lotter_num = 1:49
  lotter_times = rep(0 , 49)
  for(j in 1:n.sim){
    luck = sample(lotter_num , 6 , replace=F)
    lotter_times[luck] = lotter_times[luck]+1
  }
  for (k in 1:49){
    lotter_times[k]= lotter_times[k]/n.sim
  }
  barplot(lotter_times , xlab = "lotter number" , ylab = "lotter times" 
          , main = paste("累計搖獎", n.sim , "期的比例圖")  
          ,names.arg = c(1:49) , col = c("red" ,"orange" , "yellow" , 
                                         "green" , "lightblue" , "blue" , "purple"))

n.sim = 10000
lotter_num = 1:49
lotter_times = rep(0 , 49)
for(j in 1:n.sim){
  luck = sample(lotter_num , 6 , replace=F)
  lotter_times[luck] = lotter_times[luck]+1
}
# print(lotter_times)
max.v = which(lotter_times==max(lotter_times),arr.ind=TRUE)
barplot(lotter_times , xlab = "lotter number" , ylab = "lotter times" 
        , main = paste("累計搖獎", n.sim , "期的數量圖")  
        ,names.arg = c(1:49) , ylim = c(1200 , 1280) , col = c("green"))
# 2.
# max C = x1 + 3*x2 + 4*x3 + x4 , constraints : x1 - 2*x2<=9 , 3*x2 + x3 <=9 , x2+x4 <=10
library(lpSolve)
eg.lp <- lp(objective.in=c(1,3,4,1),
            const.mat=matrix(c(1,0,0,-2,3,1,0,1,0,0,0,1), nrow=3),
            const.rhs=c(9,9,10),
            const.dir=c("<=", "<=" , "<="), direction="max")
eg.lp # 55
eg.lp$solution # 9 0 9 10

