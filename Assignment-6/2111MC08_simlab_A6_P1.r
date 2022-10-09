X=c(1,3,4,2,6,7,5,8)
Y1=2*X
Y2=2.5*X
plot(X,
     Y1,
     main="Overlaying Graphs",
     ylab="",
     type="l",
     col="red")
lines(X,Y2, col="green")
legend("topleft",c("Y1","Y2"),fill=c("red","green"))