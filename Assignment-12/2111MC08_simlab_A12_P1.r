library(nleqslv)



c = 1
k = 1
n1 = 50

sample_gen1 <- function(n,c,k){
  
  u_vect = runif(n)
  x <- numeric(0)
  i = 1
  for(u in u_vect){
    x[i] <- (((1-u)^(-1/k))-1)^(1/c)
    i = i + 1
  }
  return(x)
}

mle_list1 <- matrix(0,nrow = 100, ncol = 2)

for (i in 1:100) {
  
  vec1 = sample_gen1(n1,c,k)
  
  est1 <- function(para){
    c <- para[1]
    k <- para[2]
    y <- numeric(0)
    
    y[1] = n1/c + sum(log(vec1)) - (k+1)*sum((vec1^c)*log(vec1)/(1+vec1^c))
    y[2] = n1/k - sum(log(1+vec1^c))
    return(y)
  }
  
  xstr1 <- c(c,k)
  mle1 <- nleqslv(xstr1, est1,method='Newton',jacobian=TRUE)
  mle_list1[i,] <- mle1$x
}

mle_c = round(mean(mle_list1[,1]),digits = 3)
mle_k = round(mean(mle_list1[,2]), digits = 3)

cat("Actual c and k are: ",c,k)
cat("MLE of c and k are: ",mle_c,mle_k)

var_mat1 <- solve(-mle1$jac)

z = 1.96
ci_c_l = mle_c - z*(var_mat1[1,1])^(1/2)
ci_c_u = mle_c + z*(var_mat1[1,1])^(1/2)

ci_k_l = mle_k - z*(var_mat1[2,2])^(1/2)
ci_k_u = mle_k + z*(var_mat1[2,2])^(1/2)

cat("\nMLE of c and k: ",mle_c,mle_k,
    "\nCI of c & k:\n",ci_c_l,ci_c_u,"\t",ci_k_l,ci_k_u)

