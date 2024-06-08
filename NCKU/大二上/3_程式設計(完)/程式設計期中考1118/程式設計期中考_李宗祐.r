#1.
#(a)
f=function(x) x^4+3*x^3-2*x^2-7
curve(f , xlim = c(0.8,2) , ylim = c(-6,20))

#(b)
x=1
f=x^3+2*x^2-7
tolerance=0.000001
while(abs(f)>tolerance){
  f.prime=4*x^3+9*x^2-4*x
  x=x-f/f.prime
  f=x^3+2*x^2-7
}
x

#2.
set.seed(10)
x1 = runif(500000 , min = 12 , max = 13)
x2 = runif(500000 , min = 11 , max = 12)
x3 = runif(500000 , min = 10 , max = 11)
x4 = runif(500000 , min = 9  , max = 10)
x5 = runif(500000 , min = 8  , max = 9)
x6 = runif(500000 , min = 7  , max = 8)
x7 = runif(500000 , min = 6  , max = 7)
x8 = runif(500000 , min = 6  , max = 8)
x9 = runif(500000 , min = 6  , max = 9)
x10 = runif(500000 , min = 6 , max = 10)
x11 = runif(500000 , min = 6 , max = 11)
x12 = runif(500000 , min = 6 , max = 12)
mean((x1+x2+x3+x4+x5+x6)^(1/2)*(x7+x8+x9+x10+x11+x12)^(1/4))*720



