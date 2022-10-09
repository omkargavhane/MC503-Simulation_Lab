chi_test <- function(expected,observed,alpha,d){
  cat("Observed values:",observed,"\n")
  cat("Expected values:",expected,"\n\n")
  expected_mod = numeric(0)
  observed_mod = numeric(0)
  n = length(expected)
  k = 0
  for (i in 1:n) {
    key = expected[i]
    if(key < 5 && k!=0){
      expected_mod[k] = expected_mod[k] + key
      observed_mod[k] = observed_mod[k] + observed[i]
      next
    }
    if(key < 5 && k==0){
      k=k+1
      expected_mod[k] = key
      observed_mod[k] = observed[i]
      next
    }
    if(k==1 && expected_mod[k]<5){
      expected_mod[k] = expected_mod[k] + key
      observed_mod[k] = observed_mod[k] + observed[i]
      next
    }
    k = k+1
    expected_mod[k] = key
    observed_mod[k] = observed[i]
  }
  cat("Modified Observed values:",observed_mod,"\n")
  cat("Modified Expected values:",expected_mod,"\n\n")
  
  s = sum(((observed_mod-expected_mod)^2)/(expected_mod))
  
  N = length(expected_mod)-d
  c = qchisq(1-alpha,N)
  
  cat("Chi-test value(W):",s,"\nChi-square table value:",c,"\n")
  
  if(s<=c)
    cat("\nNull Hypothesis is accepted")
  else
    cat("\nNull Hypothesis rejected")
}

alpha = 0.05
get_result <- function(x,test,alpha){
  
  a = min(min(x),min(test))
  b = max(max(x),max(test))
  h=(b-a)/10
  int_pts <- seq(a,b,h)
  
  k = length(int_pts)-1
  observed=rep(0,k)
  expected=rep(0,k)
  
  for(i in 1:k){
    observed[i]=observed[i]+ length(x[x>=(int_pts[i])])-length(x[x>(int_pts[i+1])])
    expected[i]=expected[i]+ length(test[test>=(int_pts[i])])-length(test[test>(int_pts[i+1])])
}
  
  chi_test(expected,observed,alpha,3)
}

n = 1000
test = rnorm(2000)
#summary(test)

#Method 1
method1=function(){
  u1 = runif(n)
  u2 = runif(n)
  x1 = (-2*log(u1))^(1/2)*cos(2*pi*u2) 
  x2 = (-2*log(u1))^(1/2)*sin(2*pi*u2)
  return(c(x1,x2))
}
rt1 = method1()
get_result(rt1,test,alpha)

#Method 2
method2=function(){
  x1 = numeric(0)
  x2 = numeric(0)
  for (i in 1:n) {
    w = 0
    v1 = 0
    v2 = 0
    repeat{
      u1 = runif(1)
      u2 = runif(1)
      v1 = 2*u1-1
      v2 = 2*u2-1
      w = v1^2+v2^2
      if(w<=1) break
    }
    y = ((-2*log(w)/w)^0.5)
    x1[i] = v1*y
    x2[i] = v2*y
  }
  return(c(x1,x2))
}

rt2 = method2()
get_result(rt2,test,alpha)

