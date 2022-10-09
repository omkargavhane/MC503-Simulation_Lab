#P2
x1=rep(50,times=10)
x2=rep(1:2,times=15)
x3=rep(-1:3,times=10)
x4=rep(2,times=8)
x5=c(0,8)
v1=c(x1,x2,x3,x4,x5)
v1
length(v1)
#output : 
#[omkar@gavhane Assignment]$ Rscript simlab_A1_P2.r 
#[1] 50 50 50 50 50 50 50 50 50 50  1  2  1  2  1  2  1  2  1  2  1  2  1  2  1
#[26]  2  1  2  1  2  1  2  1  2  1  2  1  2  1  2 -1  0  1  2  3 -1  0  1  2  3
#[51] -1  0  1  2  3 -1  0  1  2  3 -1  0  1  2  3 -1  0  1  2  3 -1  0  1  2  3
#[76] -1  0  1  2  3 -1  0  1  2  3 -1  0  1  2  3  2  2  2  2  2  2  2  2  0  8