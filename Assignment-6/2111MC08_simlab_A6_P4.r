sigm=4
mue=3

#P4.I exponential
val_e=mue-sigm*log(1-runif(1000)) 
mean(val_e)
var(val_e)
y_e=(1/sigm)*exp(-(val_e-mue)/sigm)

#P4.II cauchy
val_c=sigm*tan(pi*runif(1000))
mean(val_c)
var(val_c)
y_c=sigm/(pi*(sigm^2+val_c^2))

#P4.III burr 
k=3
c=1
val_b=(-1+(1-runif(1000))**(-1/k))**(1/c)
mean(val_b)
var(val_b)
y_b=c*val_b^(c-1)*k*(1+val_b^c)^(-k-1)

#plotting
plot(val_e,y_e,col='red',main="sample generation",xlim = c(min(val_b),max(val_b)),
     type="p",lwd=1,ylab = "y",xlab = "x",pch=1)
points(val_c,y_c,col="green")
points(val_b,y_b,col="blue")

legend("topright",legend = c("exponential","cauchy","burr XII"),
       col = c("red","green","blue"),cex = 0.8,pch = 1)
abline(h=0,col='red')
abline(v=0,col= 'red')
