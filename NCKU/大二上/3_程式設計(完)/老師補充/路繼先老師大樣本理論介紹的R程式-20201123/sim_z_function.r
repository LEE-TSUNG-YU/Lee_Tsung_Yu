fourNum <- function (x) {
  mu <- mean(x)
  sigma <- sd(x)
  z <- scale(x, mu, sigma)
  c(mean = mu, variance = sigma^2, skewness = mean(z^3),
    kurtosis = mean(z^4) - 3)
}

simu.z <- function(dstn="norm", n=31, mu, sigma, ..., R=1000, plot.it=TRUE) {
  ### Simulation study of z-statistic, namely, CLT
  dist <- sapply(c("d", "r"), function(k) get(paste(k, dstn, sep="")))
  if(missing(mu))
    mu <- integrate(function(x) x*dist$d(x, ...), -Inf, Inf)$value
  if(missing(sigma))
    sigma <- sqrt(integrate(function(x) (x - mu)^2*dist$d(x, ...),
                            -Inf, Inf)$value)
  z <- sapply(1:R, function(dummy) {
    x <- dist$r(n, ...)
    (mean(x) - mu)/(sigma/sqrt(n))
  })
  digits <- round(log10(R))
  fournum <- fourNum(z)
  #fournum <- c(mean(z),var(z),skewness(z),kurtosis(z))
  if(plot.it) {
    parm <- c(...)
    if(!is.null(parm)) {
      n.parm <- length(parm)
      parm <- paste(names(parm), parm, sep=" = ", collapse=", ")
    }
    else n.parm <- 0
    FourNum <- c(0, 1, 0, 0)
    layout(matrix(rep(1:4, rep(2,4)), 2, 4, byrow=TRUE)) # par(mfrow=c(2,2))
    hist(z, 20, ylim=c(0, dnorm(0)), freq=FALSE, col="gray", border="white",
         xlab=paste(R, "simulated z-values"),
         main="\nHistogram with Density Curve of N(0, 1)")
    curve(dnorm(x), col=2, add=TRUE)
    plot(1:10, type="n", ann=F, xaxt="n", yaxt="n", bty="n")
    mtext(expression("Statistic:"~z == frac(bar(x)[n] - mu,
                                            sigma/sqrt(n))), side=1, line=2)
    title(paste("\n\n\nDistribution parameter",
                ifelse(n.parm > 1, "s\n\n", "\n\n"),
                ifelse(is.null(parm), "(standard version)", parm),
                sep=""))
    position <- seq(7, by=-1.5, length=4)
    text(6.75, 8.5, "Sample statistics", cex=1.25, pos=2)
    text(rep(5,4), position,
         paste(c("mean", "variance", "skewness", "kurtosis"),
               "="), pos=2)
    text(rep(6.75,4), position, format(round(fournum, digits),
                                       digits=digits), pos=2)
    text(7, 8.5, "N(0, 1)", pos=4, cex=1.25)
    text(rep(7.5, 4), position, FourNum, pos=4)
    mtext(substitute("Sample from '"*dn*"' Distribution of"~n == nn,
                     list(dn=dstn, nn=n)),
          outer=TRUE, line=-1.5, cex=1.2)
    plot(sort(z), (1:R)/R, type="s", ylab="Probability",
         xlab="Simulated z-values", main="\nEmpirical CDF Plot")
    curve(pnorm(x), col=2, add=TRUE)
    abline(h=c(0,1), lty=3)
    qqplot(qnorm(ppoints(R)), z,
           xlab=substitute("Quantiles of N(0, 1) Distribution", list(dd=df)),
           ylab="Simulated z-values",
           main="\nQ-Q Plot")
    abline(0, 1, col=2)
    layout(1) # par(mfrow=c(1,1))
  }
  return(round(fournum, digits))
}

#simu.z(dstn="norm", n=31, mu=1, sigma=1, R=1000, plot.it=TRUE)