dataset=c(.70, .84, .58, .50, .55, .82, .59, .71, .72, .61, .62, .49, .54,
          .36, .36, .71, .35, .64, .85, .55, .59, .29, .75, .46, .46, .60,
          .60, .36, .52, .68, .80, .55, .84, .34, .34, .70, .49, .56, .71,
          .61, .57, .73, .75, .44, .44, .81, .80, .87, .29, .50)
ordered_dataset=sort(dataset)
n=length(dataset)
emp_cdf=(0:n)/n
c=5
k=8.2680


burr_X=function(x,c,k)
{
  y=rep(0, each=n)
  for (i in 1:n)
    y[i]=(1-exp(-(c*x[i])**2))**k
  return(y)
}

burr_XII=function(x,c,k)
{
  y=rep(0, each=n)
  for (j in 1:n)
    y[j]=1-(1+(x[j])**c)**(-k)
  return (y)
}

emp_cdf_new=c(0,rep(emp_cdf[2:n], each=2),1)

#for burr X
theo_cdf_bx=burr_X(ordered_dataset,c,k)
theo_cdf_new_bx=rep(theo_cdf_bx, each=2)

abs_diff_bx=abs(emp_cdf_new-theo_cdf_new_bx)
ks_statistic_bx=max(abs_diff_bx)
ks_statistic_bx
ks.test(dataset,burr_X,c,k)

#for burr XII
theo_cdf_bxii=burr_XII(ordered_dataset,c,k)
theo_cdf_new_bxii=rep(theo_cdf_bxii, each=2)

abs_diff_bxii=abs(emp_cdf_new-theo_cdf_new_bxii)
ks_statistic_bxii=max(abs_diff_bxii)
ks_statistic_bxii
ks.test(dataset,burr_XII,c,k)

#for n=50 and alpha=0.05 
d_value=0.125
ks_statistic_bx<d_value
ks_statistic_bxii<d_value
