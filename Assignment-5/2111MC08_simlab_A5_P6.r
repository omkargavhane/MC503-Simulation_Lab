df=AirPassengers
#I
sum(df)
#II
ndf=matrix(df,nrow=12,ncol=12,byrow=TRUE)
total=c()
for(r in 1:12) 
  total[r]=sum(ndf[r,])  
plot(1949:1960,total,xlab='years',ylab='Number of passangers')
#III
for(c in 1:12){
  boxplot(ndf[,c],xlab=c)
  total[c]=sum(ndf[,c])
}
boxplot(total)