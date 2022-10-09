inv=function(x)
  return(1/x)

x=seq(from=0,to=20,by=0.1)
y=inv(x)
plot(x,y,type='l',col='red',main='y=1/x')
y1=x
y2=2*x
lines(x,y1,col='green')
lines(x,y2,col='blue')