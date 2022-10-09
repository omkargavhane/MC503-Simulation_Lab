e=c(1.81, 1.24, 1.06, 1.80, 2.15, 1.93, 1.97, 1.59, 1.91, 1.86,
    1.08, 2.53, 1.12, 2.74, 1.69, 1.23, 1.24,1.80, 2.30, 1.54)

len_of_e=length(e)

uniform_dist=function(x,a,b){
  y=rep(0,each=len_of_e)
  for (i in 1:len_of_e)
    y[i]=(x[i]-a)/(b-a)
  return(y)
}

o=uniform_dist(e,1,3)

chi_square_val=function(o,e){
s=0
for(i in 1:length(e)){
  t=((o[i]-e[i])^2/e[i])
  print(t)
  s=s+t
}
return(s)
}
dof=len_of_e-1
alpha=0.05
s<30.14

#P2
expected=function(x){
  t=70*((exp(-1.185)*1.185^x)/factorial(x))
  return(t)
}
o=c(22,25,14,6,3)
e=rep(0,each=length(o))
x=c(0,1,2,3,4)
for(i in 1:length(x))
  e[i]=expected(x[i])
ret=chi_square_val(o,e)