library(nleqslv)

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

ks_test <- function(c,s){
  
  cat("Critical D value:",c,"\nStatistic D value:",s)
  
  if(s<=c)
    cat("\nNull Hypothesis is accepted")
  else
    cat("\nNull Hypothesis rejected")
}


burrXII_cdf=function(x,c,k){
  theo_cdf = 1-(1+x^c)^(-k)
  return(theo_cdf)
}

burrXII_pdf=function(X,c,k){
  y = c*k*(X^(c-1))/((1+X^c)^(k+1))
  return(y)
}

n = 1000
c = 5
k = 8.268

sample_gen=function(u_vect,c,k){
  
  x <- numeric(0)
  i = 1
  for(u in u_vect){
    x[i] <- ((1-u)^(-1/k)-1)^(1/c)
    i = i + 1
  }
  return(x)
}

u_vect = runif(n)
vec=sample_gen(u_vect,c,k)
#summary(vec1)

a = min(vec)
b = max(vec)
h = (b-a)/10
int_pts = seq(a,b,h)

observed=numeric(0)
expected=numeric(0)

for(i in 1:10){
  observed[i]=length(vec[vec>=(int_pts[i])])-length(vec[vec>(int_pts[i+1])])
  expected[i]=n*(burrXII_cdf(int_pts[i+1],c,k) - burrXII_cdf(int_pts[i],c,k))
}

chi_test(expected,observed,alpha,1)


#KS test
emp_cdf=numeric(0)
theo_cdf=numeric(0)
for(i in 1:11){
  emp_cdf[i] <- length(vec[vec<=(int_pts[i])])/n
  theo_cdf[i] <- burrXII_cdf(int_pts[i],c,k)
}

critical_D = 0.262
emp_cdf=c(0,rep(emp_cdf[1:10], each=2),1)
theo_cdf=rep(theo_cdf, each=2)
statistic_D=max(abs(emp_cdf-theo_cdf))
ks_test(critical_D,statistic_D)



