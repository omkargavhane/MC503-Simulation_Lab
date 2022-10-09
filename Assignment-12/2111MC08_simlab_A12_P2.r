library(nleqslv)

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



