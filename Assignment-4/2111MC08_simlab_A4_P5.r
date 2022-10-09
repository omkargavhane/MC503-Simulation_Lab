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



mat_adjoint=function(A) {
  minor=function(A, i, j)
    return(det(A[-i,-j]))
  
  cofactor=function(A, i, j)
    return(((-1)^(i+j)) * minor(A,i,j))
  
  n=nrow(A)
  B=matrix(NA, n, n)
  print(B)
  for( i in 1:n )
    for( j in 1:n )
      B[i,j]=cofactor(A, i, j) 
  return(matrix_transpose(B))
}

A=matrix(c(1,-2,3,-1,3,-1,2,-5,5),nrow=3,ncol=3,byrow=TRUE)
print(A)
B=matrix(c(9,-6,17))
print(B)
inverse_A=mat_adjoint(A)/det(A)
x=matrix_product(inverse_A,B)
print(x)

#using builtin functions
inverse=solve(A)
result=inverse%*%B
print(result)

determinant <- function(mat, k){
  s <- 1
  det <- 0
  x <- c()
  xsum <- 1
  for (i in 1:k) {
    for (j in 1:k) {
      x[xsum] <- 0
      xsum <- xsum+1
    }
    
  }
  b <- matrix(x, nrow = k, byrow = TRUE)
  a <- matrix(mat, nrow = k, byrow = TRUE)
  m <- 0
  n <- 0
  c <- 0
  if(k == 1){
    return(a[1,1])
  }else{
    det <- 0
    for (c in 1:k) {
      
      m <- 1
      n <- 1
      for (i in 1:k) {
        
        for (j in 1:k) {
          b[i,j] <- 0
          if(i != 1 && j != c){
            b[m,n] <- a[i,j]
            if(n < (k-2))
              n <- n+1
          }else{
            n <- 1
            m <- m+1
          }
        }
        
      }
      det <- det + s*(a[1,c] * determinant(b, k-1))
      s <- -1 * s
    }
  }
  
  return(det)
}

