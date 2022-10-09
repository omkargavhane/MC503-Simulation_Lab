dataset=c(1.013 ,1.034 ,1.109 ,1.169 ,1.266 ,1.509 ,1.533 ,1.563 ,1.716 ,1.929,
          1.965 ,2.061 ,2.344 ,2.546 ,2.626 ,2.778 ,2.951 ,3.413 ,4.118 ,5.136)

ordered_dataset=sort(dataset)
n=length(dataset)
n
emp_cdf=(0:n)/n
emp_cdf
a=2.093
s=1.013

new_pareto=function(x,a,s){
  y=rep(0, each=n)
  for (i in 1:n)
    y[i]=1-((2*s^a)/(s^a+x[i]^a))
  return(y)
}

emp_cdf_new=c(0,rep(emp_cdf[2:n], each=2),1)
emp_cdf_new

#for new pareto
theo_cdf_np=new_pareto(ordered_dataset,a,s)
theo_cdf_new_np=rep(theo_cdf_np, each=2)

abs_diff_np=abs(emp_cdf_new-theo_cdf_new_np)
ks_statistic_np=max(abs_diff_np)
ks_statistic_np
ks.test(dataset,new_pareto,a,s)

#for n=20 and alpha=0.05
d_value=0.192
ks_statistic_np<d_value
