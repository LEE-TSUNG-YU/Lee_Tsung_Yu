install.packages("plot3D")
library("plot3D")
data(iris)
head(iris)
# x, y and z coordinates
x <- sep.l <- iris$Sepal.Length
y <- pet.l <- iris$Petal.Length
z <- sep.w <- iris$Sepal.Width

scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"))

scatter3D(x, y, z, colvar = NULL, col = "blue",
          pch = 19, cex = 0.5)

# full box
scatter3D(x, y, z, bty = "f", colkey = FALSE, main ="bty= 'f'")
# back panels and grid lines are visible
scatter3D(x, y, z, bty = "b2", colkey = FALSE, main ="bty= 'b2'" )

# grey background with white grid lines
scatter3D(x, y, z, bty = "g", colkey = FALSE, main ="bty= 'g'")
# User defined
scatter3D(x, y, z, pch = 18, bty = "u", colkey = FALSE, 
          main ="bty= 'u'", col.panel ="steelblue", expand =0.4, 
          col.grid = "darkblue")

# gg.col: ggplot2 like color
scatter3D(x, y, z, bty = "g", pch = 18, col = gg.col(100))
# ramp.col: custom palettes
scatter3D(x, y, z, bty = "g", pch = 18,
          col = ramp.col(c("blue", "yellow", "red")) )

scatter3D(x, y, z, bty = "g", pch = 18, 
          col.var = as.integer(iris$Species), 
          col = c("#1B9E77", "#D95F02", "#7570B3"),
          pch = 18, ticktype = "detailed",
          colkey = list(at = c(2, 3, 4), side = 1, 
                        addlines = TRUE, length = 0.5, width = 0.5,
                        labels = c("setosa", "versicolor", "virginica")))

# Bottom colkey
scatter3D(x, y, z, bty = "g",
          colkey = list(side = 1, length = 0.5))

scatter3D(x, y, z, theta = 15, phi = 20)

# type ="l" for lines only
scatter3D(x, y, z, phi = 0, bty = "g", type = "l", 
          ticktype = "detailed", lwd = 4)

# type ="b" for both points and lines
scatter3D(x, y, z, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5))
# type ="h" for vertical lines
scatter3D(x, y, z, phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed", pch = 19, cex = 0.5)

# Confidence interval
CI <- list(z = matrix(nrow = length(x),
                      data = rep(0.1, 2*length(x))))
head(CI$z)

# 3D Scatter plot with CI
scatter3D(x, y, z, phi = 0, bty = "g", col = gg.col(100), 
          pch = 18, CI = CI)

#==================================================================
hist3D_fancy<- function(x, y, break.func = c("Sturges", "scott", "FD"), breaks = NULL,
                        colvar = NULL, col="white", clab=NULL, phi = 5, theta = 25, ...){
  
  # Compute the number of classes for a histogram
  break.func <- break.func [1]
  if(is.null(breaks)){
    x.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(x),
                       scott = nclass.scott(x),
                       FD = nclass.FD(x))
    y.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(y),
                       scott = nclass.scott(y),
                       FD = nclass.FD(y))
  } else x.breaks <- y.breaks <- breaks
  
  # Cut x and y variables in bins for counting
  x.bin <- seq(min(x), max(x), length.out = x.breaks)
  y.bin <- seq(min(y), max(y), length.out = y.breaks)
  xy <- table(cut(x, x.bin), cut(y, y.bin))
  z <- xy
  
  xmid <- 0.5*(x.bin[-1] + x.bin[-length(x.bin)])
  ymid <- 0.5*(y.bin[-1] + y.bin[-length(y.bin)])
  
  oldmar <- par("mar")
  par (mar = par("mar") + c(0, 0, 0, 2))
  hist3D(x = xmid, y = ymid, z = xy, ...,
         zlim = c(-max(z)/2, max(z)), zlab = "counts", bty= "g", 
         phi = phi, theta = theta,
         shade = 0.2, col = col, border = "black",
         d = 1, ticktype = "detailed")
  
  scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = gg.col(100),
            add = TRUE, pch = 18, clab = clab,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)
}

# Create his3D using plot3D
hist3D_fancy(iris$Sepal.Length, iris$Petal.Width, colvar=as.numeric(iris$Species))
# Make the rgl version

install.packages("plot3Drgl")
library("plot3Drgl")

plotrgl()
