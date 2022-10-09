library(nleqslv)

#************************************** Problem 1 ***************************************

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


#************************************** Problem 2 ***************************************

sample2 = c(.70, .84, .58, .50, .55, .82, .59, .71, .72, .61, .62, .49,
            .54, .36, .36, .71, .35, .64, .85, .55, .59, .29, .75, .46,
            .46, .60, .60, .36, .52, .68, .80, .55, .84, .34, .34, .70,
            .49, .56, .71, .61, .57, .73, .75, .44, .44, .81, .80, .87,
            .29, .50)
n2 = length(sample2)

est_XII <- function(para){
  c <- para[1]
  k <- para[2]
  y <- numeric(0)
  
  y[1] = n1/c + sum(log(sample2_XII)) - (k+1)*sum((sample2_XII^c)*log(sample2_XII)/(1+sample2_XII^c))
  y[2] = n2/k - sum(log(1+sample2_XII^c))
  return(y)
}

est_X <- function(para){
  c <- para[1]
  k <- para[2]
  y <- numeric(0)
  
  e = exp(-(c*sample2_X)^2)
  y[1] = 2*(n2/c - c*sum((sample2_X)^2)) + 2*c*(k-1)*sum((sample2_X^2)*e/(1-e))
  y[2] = n2/k + sum(log(1-e))
  return(y)
}

c = 1
k = 1
xstr2 <- c(c,k)
sample2_X = sample2
sample2_XII = sample2

mle_X <- nleqslv(xstr2, est_X,method='Newton',jacobian=TRUE)
mle_XII <- nleqslv(xstr2, est_XII,method='Newton',jacobian=TRUE)

var_mat_X <- solve(-mle_X$jac)
var_mat_XII <- solve(-mle_XII$jac)
z = 1.96

ci_X_c_l = mle_X$x[1] - z*(var_mat_X[1,1])^(1/2)
ci_X_c_u = mle_X$x[1] + z*(var_mat_X[1,1])^(1/2)

ci_X_k_l = mle_X$x[2] - z*(var_mat_X[2,2])^(1/2)
ci_X_k_u = mle_X$x[2] + z*(var_mat_X[2,2])^(1/2)

ci_XII_c_l = mle_XII$x[1] - z*(var_mat_XII[1,1])^(1/2)
ci_XII_c_u = mle_XII$x[1] + z*(var_mat_XII[1,1])^(1/2)

ci_XII_k_l = mle_XII$x[2] - z*(var_mat_XII[2,2])^(1/2)
ci_XII_k_u = mle_XII$x[2] + z*(var_mat_XII[2,2])^(1/2)

cat("\nMLE of c and k: ",mle_X$x,
    "\nCI for c & k are:\n",c(ci_X_c_l,ci_X_c_u),"\t",c(ci_X_k_l,ci_X_k_u))

cat("\nMLE of c and k: ",mle_XII$x,
    "\nCI for c & k are:\n",c(ci_XII_c_l,ci_XII_c_u),"\t",c(ci_XII_k_l,ci_XII_k_u))


#************************************** Problem 3 ***************************************

sample3 = c(0.080, 0.084, 0.102, 0.124, 0.326, 0.358, 0.412, 0.444, 0.456, 0.504, 0.498, 0.564, 0.648,
            0.666, 0.682, 0.732, 0.770, 0.814, 0.840, 0.862, 0.882, 0.922, 0.924, 0.964, 1.034, 1.034,
            1.048, 1.128, 1.134, 1.172, 1.238, 1.240, 1.242, 1.244, 1.294, 1.302, 1.372, 1.522, 1.526)
n3 = length(sample3)

est3 <- function(para){
  beta <- para[1]
  eta <- para[2]
  y <- numeric(0)
  y[1] = n3/beta + sum(sample3) - eta*sum(exp(beta*sample3-1)*(sample3))
  y[2] = n3/eta - sum(exp(beta*sample3-1))
  return(y)
}

xstr3 <- c(1,1)
mle3 <- nleqslv(xstr3, est3,method='Newton',jacobian=TRUE)

var_mat3 <- solve(-mle3$jac)
z = 1.96

beta_l = mle3$x[1] - z*(var_mat3[1,1])^(1/2)
beta_u = mle3$x[1] + z*(var_mat3[1,1])^(1/2)

eta_l = mle3$x[2] - z*(var_mat3[2,2])^(1/2)
eta_u = mle3$x[2] + z*(var_mat3[2,2])^(1/2)

cat("\nMLE's of beta & eta: ",mle3$x,
    "\nCI for beta & eta are:\n",c(beta_l,beta_u),"\t",c(eta_l,eta_u))