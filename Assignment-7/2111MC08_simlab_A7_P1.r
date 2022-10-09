#sample generation
options(max.print = 10000)
alpha=1
beta=1.5
#val=(runif(2000)^(-(1/(alpha*beta))))-1
val=(runif(2000)^(-1/alpha)-1)^(-1/beta)
val
mean(val)
var(val)
#plotting
x=seq(0.01,5,by=0.01)
alpha=1
beta=1
pdf_y=alpha*beta*x^-(beta+1)*(1+x^-beta)^-(alpha+1)
cdf_y=(1+x^-beta)^-alpha
plot(x,pdf_y,col='red',main="pdf & cdf",xlim = c(0,6),
     type="p",lwd=1,ylab = "y",xlab = "x",pch=1)
points(x,cdf_y,col="green")
legend("topright",legend = c("pdf_1_1","cdf_1_1"),
       col = c("red","green"),cex = 0.8,pch = 1)
alpha=1.5
beta=3
pdf_y=alpha*beta*x^-(beta+1)*(1+x^-beta)^-(alpha+1)
cdf_y=(1+x^-beta)^-alpha
plot(x,pdf_y,col='red',main="pdf & cdf",xlim = c(0,6),
     type="p",lwd=1,ylab = "y",xlab = "x",pch=1)
points(x,cdf_y,col="green")
legend("topright",legend = c("pdf_1.5_3","cdf_1.5_3"),
       col = c("red","green"),cex = 0.8,pch = 1)
