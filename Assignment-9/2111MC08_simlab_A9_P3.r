#mue=2
#sigma=1
#x=mue-log(1-runif(30))*sigma


#cdf=function(m,s,x){
#  return(1-exp(-1*(x-m)/s))
#}
#cdf(mue,sigma,2)

#
mue=2
sigma=1
sample=mue+sigma*tan(pi*(runif(2000)-0.5))

cdf=function(m,s,x)
  return((1/pi)*atan((x-m)/s)+1/s)

min_sample=min(sample)
max_sample=max(sample)

step=(max_sample-min_sample)/10
observed=rep(0,each=10)
interval=seq(min_sample,max_sample,step)
start=min_sample
stop=min_sample+step
for(i in 1:10){
  for(e in sample){
    if(e>=start && e<=stop){
      observed[i]=observed[i]+1
    }
  }
  start=stop
  stop=stop+step
}
observed
expected=rep(0,each=10)
start=min_sample
stop=max_sample
for(i in 1:10){
  expected[i]=2000*(cdf(mue,sigma,interval[i+1])-cdf(mue,sigma,interval[i]))
}

chi_square_val=function(o,e){
  s=0
  for(i in 1:length(e)){
    t=((o[i]-e[i])^2/e[i])
    print(t)
    s=s+t
  }
  return(s)
}
chi_square_val(observed,expected)<14.067
sum(expected)

