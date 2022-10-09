library(nleqslv)

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
