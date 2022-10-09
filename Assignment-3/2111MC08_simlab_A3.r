source("numerical_integration.r")

f1=function(x)
  return(2**x)

f2=function(x)
  return(x+1/x)

f3=function(x)
  return(1/(2+cos(x)))

f4=function(x)
  return(2000*log(140000/(140000-2100*x))-9.8*x)

f5=function(x)
  return(2*x)

#P1
print("P1")
trapezoidal(f1,0,10,10)
#P2
print("P2")
round(trapezoidal(f2,1.2,1.6,4),digits=2)
round(simpson13(f2,1.2,1.6,4),digits=2)
#P3
print("P3")
round(simpson13(f3,-1,1,10),digits=3)
#P4
print("P4")
simpson13(f4,8,30,4)
simpson38(f4,8,30,3)
#P5
print("P5")
trapezoidal(f5,0,1,10)
simpson13(f5,0,1,10)
#P6
print("P6")
simpson38_f6=function(fn,ll,ul,n){
  h=(ul-ll)/n
  x=seq(ll,ul,h)
  y=c(1,0.9975,0.99,0.9776,0.8604)
  y0=y[1]
  yn=y[length(y)]
  y3=0
  y2=0
  y=y[-length(y)][-1]
  for(i in 1:length(y))
    if(i%%3==0)
      y2=y2+y[i]
  else
    y3=y3+y[i]
  return((3*h/8)*(y0+yn+2*y2+3*y3)) 
}
simpson38_f6('',0,0.4,4)
