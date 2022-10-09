#P1
x=c(4,-5,2)
y=c(6,1,-3)
#P1.i
length(x)
length(y)
#P1.ii
x+y
x-y
#P1.iii
sum(x)
sum(y)
#P1.iv
print("finding covariance using inbuilt function")
cov(x,y)
print("finding covariance not using inbuilt function")
my_mean=function(vect){
  sum=0
  for(e in vect)
    sum=sum+e
  return(sum/length(vect))
}
my_cov=function(x,y){
mean_x=my_mean(x)
mean_y=my_mean(y)
sum=0
for(i in 1:length(x))
  sum=sum+(mean_x-x[i])*(mean_y-y[i])
return(sum/(length(x)-1))
}
my_cov(x,y)

#output :
#[omkar@gavhane Assignment]$ Rscript simlab_A1_P1.r 
#[1] 3
#[1] 3
#[1] 10 -4 -1
#[1] -2 -6  5
#[1] 1
#[1] 4
#[1] "finding covariance using inbuilt function"
#[1] 5.833333
#[1] "finding covariance not using inbuilt function"
#[1] 5.833333

