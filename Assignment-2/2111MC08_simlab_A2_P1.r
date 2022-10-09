#P1.i
A=rbind(c(3,-2,1),c(-1,4,-2))
A
A=cbind(c(3,-1),c(-2,4),c(1,-2))
A
B=rbind(c(-7,4),c(9,5),c(2,-1))
B
B=cbind(c(-7,9,2),c(4,5,-1))
B

#P1.ii
vector_product=function(v1,v2){
	sum=0
	for(i in 1:length(v1))
		sum=sum+v1[i]*v2[i]
	return(sum)
}
matrix_product=function(m1,m2){
	m1_nrow=nrow(m1)
	m1_ncol=ncol(m1)
	m2_nrow=nrow(m2)
	m2_ncol=ncol(m2)
	res=matrix(nrow=m1_nrow,ncol=m2_ncol)
	if(m1_ncol==m2_nrow){
		for(r in 1:m1_nrow)
			for(c in 1:m2_ncol)
				res[r,c]=vector_product(m1[r,],m2[,c])

		return(res)
	}
	return("Number of Rows & column mismatch!!")
}
matrix_transpose=function(mat){
  mat_transpose=matrix(nrow=ncol(mat),ncol=nrow(mat))
  for(i in 1:nrow(mat))
      for(j in 1:ncol(mat))
        mat_transpose[j,i]=mat[i,j]
    return(mat_transpose)
}

minor=function(A, i, j){ 
  return(det(matrix(A[-i,-j])))
  }
cofactor=function(A, i, j){ 
  return((-1)^(i+j) * minor(A,i,j))
}

mat_adjoint=function(A) {
  n=nrow(A)
  B=matrix(NA, n, n)
  for( i in 1:n )
    for( j in 1:n )
      B[j,i]=cofactor(A, i, j) 
  return(t(B))
}
#matrix_transpose(A)
product=matrix_product(A,B)
product
#product=A%*%B
#product
#P4.iii
transpose_product=matrix_transpose(product)
transpose_product
#inverse_product=solve(product)
#inverse_product
inverse_product=mat_adjoint(product)/det(product)
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
#P1.iv
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

