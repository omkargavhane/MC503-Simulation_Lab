trapezoidal=function(fn,ll,ul,n){
	h=(ul-ll)/n
	x=seq(ll,ul,h)
	y=c()
	for(xi in x)
		y=append(y,fn(xi))
	return ((h/2) * ( (y[1]+y[length(y)]) + 2*(sum(y)-(y[1]+y[length(y)]))  )) 

}
simpson13=function(fn,ll,ul,n){
	h=(ul-ll)/n
	x=seq(ll,ul,h)
	y=c()
	for(xi in x)
		y=append(y,fn(xi))
	y0=y[1]
	yn=y[length(y)]
	y4=0
	y2=0
	y=y[-length(y)][-1]
	for(i in 1:length(y))
		if(i%%2==0)
			y2=y2+y[i]
		else
			y4=y4+y[i]
  return((h/3)*(y0+yn+2*y2+4*y4)) 
}
simpson38=function(fn,ll,ul,n){
	h=(ul-ll)/n
	x=seq(ll,ul,h)
	y=c()
	for(xi in x)
		y=append(y,fn(xi))
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
