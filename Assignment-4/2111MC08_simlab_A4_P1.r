f1=function(x)
  return(x^3-4*x-9)
  #return(x**3-x-4)


bisection_and_regular_falsi=function(f,method){
  x=0
  yp=0
  while(1){
    yc=f(x)
    if(yp*yc<0)
      break
    yp=yc
    x=x+1
  }
  b=x
  a=x-1
  print(a)
  print(b)
  midp=b
  while(1){
  if(method=="b")
    mid=(a+b)/2
  else if(method=="rf")
    mid=(a*f(b)-b*f(a))/(f(b)-f(a))
  if(abs(f(mid)-f(midp))<=1/exp(4))
    break
  if(f(a)*f(mid)<0)
    b=mid
  else if(f(mid)*f(b)<0)
    a=mid
  midp=mid
  }
  print(mid)
  print(f(mid))
}

bisection_and_regular_falsi(f1,"b")
bisection_and_regular_falsi(f1,"rf")