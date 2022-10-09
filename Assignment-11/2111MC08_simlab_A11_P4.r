sample_gen4 <- function(u_vect,l,k){
  x <- numeric(0)
  i = 1
  for(u in u_vect){
    x[i] <- l*(-log(1-u))^(1/k)
    i = i + 1
  }
  return(x)
}

mle_list <- matrix(0,nrow = 1000, ncol = 2)
lmd=2
k=1.5
for (i in 1:1000) {
  
  u_vec4 = runif(50)
  vec4 = sample_gen4(u_vec4,2,1.5)
  n4 = length(vec4)
  
  est4 <- function(para){
    l <- para[1]
    k <- para[2]
    y <- numeric(0)
    
    y[1] = n4/k - n4*log(l) + sum(log(vec4)) - sum(((vec4/l)^k)*log(vec4/l))
    y[2] = (k/l)*sum((vec4/l)^k) - (n4*k)/l 
    return(y)
  }
  
  xstr4 <- c(lmd,k)
  mle4 <- nleqslv(xstr4, est4,method='Newton',jacobian=TRUE)
  
  mle_list[i,] <- mle4$x
}

mle_l = mean(mle_list[,1])
mle_k = mean(mle_list[,2])


mle_l
mle_k

bias_l = abs(mle_l - lmd)
mse_l = mean(((mle_list[,1])-lmd)^2)
bias_k = abs(mle_k - k)
mse_k = mean(((mle_list[,2])-k)^2)

bias_l
mse_l
bias_k
mse_k
