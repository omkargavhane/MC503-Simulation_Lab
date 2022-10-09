#X1= (2,4,6,10,4,7,12,20,5)
#X2= (10,5,5,20,4,70,40,12)
#X3= (2,4,2.5,34,1.6,9.5,6,2)

X1=c(2,4,6,10,4,7,12,20,5)
X2=c(10,5,5,20,4,70,40,12,NA)
X3=c(2,4,2.5,34,1.6,9.5,6,2,NA)
df=rbind(X1,X2,X3)
df
summary(df)

find_remove_outlier=function(X){
  ov=c(boxplot(X)$out)
  print(ov)
  nx=c()
  cnt=1
  for(x in X){
    if(!(x %in% ov))
      nx[cnt]=x
    cnt=cnt+1
  }
  #print(nx)
  return(nx)
}

n_df=data.frame(X1=find_remove_outlier(X1),
              X2=find_remove_outlier(X2),
              X3=find_remove_outlier(X3))
n_df