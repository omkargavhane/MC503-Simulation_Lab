f2=function(x)
  return(exp(x**2-1)+10*sin(2*x)-5)

bisection_and_regular_falsi=function(f,method){
  x=-1
  yp=0
  while(1){
    if(x==2)
      break
    yc=f(x)
    if(yp*yc<0)
      break
    yp=yc
    x=x+1
  }
  b=x
  a=x-1
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
bisection_and_regular_falsi(f2,"b")
bisection_and_regular_falsi(f2,"rf")