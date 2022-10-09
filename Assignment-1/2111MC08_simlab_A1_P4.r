#P4.i
A=rbind(c(3,-2,1),c(-1,4,-2))
A
A=cbind(c(3,-1),c(-2,4),c(1,-2))
A
B=rbind(c(-7,4),c(9,5),c(2,-1))
B
B=cbind(c(-7,9,2),c(4,5,-1))
B
#P4.ii
product=A%*%B
product
#P4.iii
transpose_product=t(product)
transpose_product
inverse_product=solve(product)
inverse_product
#function to calculate mean and standard deviation of given matrix wrt column or rows 
calculate_mean_sd=function(mat,by="row"){
  res_mean=c()
  res_sd=c()
  if(by=="row")
    end=nrow(mat)
  else if(by=="column")
    end=ncol(mat)
  for(i in 1:end){
    if(by=="row")
      r=mat[i,]
    else if(by=="column")
      r=mat[,i]
    res_mean=append(res_mean,mean(r))
    res_sd=append(res_sd,sd(r))
  }
return(list("mean"=res_mean,"sd"=res_sd))
}  
#P4.iv
mat=list(A,B,product,transpose_product,inverse_product)
for(e in mat){
  print("Matrix => ")
  print(e)
  res=calculate_mean_sd(e)
  print('Mean wrt Row => ')
  print(res$mean)
  print('SD wrt Row => ')
  print(res$sd)
  res=calculate_mean_sd(e,by="column")
  print('Mean wrt Column => ')
  print(res$mean)
  print('SD wrt Column => ')
  print(res$sd)
}

#output : 
#[omkar@gavhane Assignment]$ Rscript simlab_A1_P4.r 
#[,1] [,2] [,3]
#[1,]    3   -2    1
#[2,]   -1    4   -2
#[,1] [,2] [,3]
#[1,]    3   -2    1
#[2,]   -1    4   -2
#[,1] [,2]
#[1,]   -7    4
#[2,]    9    5
#[3,]    2   -1
#[,1] [,2]
#[1,]   -7    4
#[2,]    9    5
#[3,]    2   -1
#[,1] [,2]
#[1,]  -37    1
#[2,]   39   18
#[,1] [,2]
#[1,]  -37   39
#[2,]    1   18
#[,1]       [,2]
#[1,] -0.02553191 0.00141844
#[2,]  0.05531915 0.05248227
#[1] "Matrix => "
#[,1] [,2] [,3]
#[1,]    3   -2    1
#[2,]   -1    4   -2
#[1] "Mean wrt Row => "
#[1] 0.6666667 0.3333333
#[1] "SD wrt Row => "
#[1] 2.516611 3.214550
#[1] "Mean wrt Column => "
#[1]  1.0  1.0 -0.5
#[1] "SD wrt Column => "
#[1] 2.828427 4.242641 2.121320
#[1] "Matrix => "
#[,1] [,2]
#[1,]   -7    4
#[2,]    9    5
#[3,]    2   -1
#[1] "Mean wrt Row => "
#[1] -1.5  7.0  0.5
#[1] "SD wrt Row => "
#[1] 7.778175 2.828427 2.121320
#[1] "Mean wrt Column => "
#[1] 1.333333 2.666667
#[1] "SD wrt Column => "
#[1] 8.020806 3.214550
#[1] "Matrix => "
#[,1] [,2]
#[1,]  -37    1
#[2,]   39   18
#[1] "Mean wrt Row => "
#[1] -18.0  28.5
#[1] "SD wrt Row => "
#[1] 26.87006 14.84924
#[1] "Mean wrt Column => "
#[1] 1.0 9.5
#[1] "SD wrt Column => "
#[1] 53.74012 12.02082
#[1] "Matrix => "
#[,1] [,2]
#[1,]  -37   39
#[2,]    1   18
#[1] "Mean wrt Row => "
#[1] 1.0 9.5
#[1] "SD wrt Row => "
#[1] 53.74012 12.02082
#[1] "Mean wrt Column => "
#[1] -18.0  28.5
#[1] "SD wrt Column => "
#[1] 26.87006 14.84924
#[1] "Matrix => "
#[,1]       [,2]
#[1,] -0.02553191 0.00141844
#[2,]  0.05531915 0.05248227
#[1] "Mean wrt Row => "
#[1] -0.01205674  0.05390071
#[1] "SD wrt Row => "
#[1] 0.019056779 0.002005977
#[1] "Mean wrt Column => "
#[1] 0.01489362 0.02695035
#[1] "SD wrt Column => "
#[1] 0.05717034 0.03610758
