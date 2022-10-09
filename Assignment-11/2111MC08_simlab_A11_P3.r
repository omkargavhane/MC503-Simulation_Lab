library(nleqslv)

sample_gen3 <- function(u_vect,a,b){
  x <- numeric(0)
  i = 1
  for(u in u_vect){
    x[i] <- (1-(1-u)^(1/b))^(1/a)
    i = i + 1
  }
  return(x)
}
mle_list <- matrix(0,nrow = 100, ncol = 2)

for (i in 1:1000) {
  
  u_vec = runif(50)
  vec3 = sample_gen3(u_vec,2,3)
  #print(vec3)
  n = length(vec3)
  
  est <- function(para){
    a <- para[1]
    b <- para[2]
    y <- numeric(0)
    
    y[1] = n/a + sum(log(vec3)) + (1-b)*sum(((vec3^a)*log(vec3))/(1-vec3^a))
    y[2] = n/b + sum(log(1-vec3^a))
    return(y)
  }
  
  xstr <- c(1,2)
  mle <- nleqslv(xstr, est,method='Newton',jacobian=TRUE)
  mle_list[i,] <- mle$x
}

mle_a = mean(mle_list[,1])
mle_b = mean(mle_list[,2])
mle_a
mle_b

