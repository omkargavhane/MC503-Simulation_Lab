chi_test=function(expected,observed){
  expected_freq = numeric(0)
  observed_freq = numeric(0)
  n = length(expected)
  k = 0
  for (i in 1:n) {
    key = expected[i]
    if(key < 5 && k!=0){
      expected_freq[k] = expected_freq[k] + key
      observed_freq[k] = observed_freq[k] + observed[i]
      next
    }
    k = k+1
    expected_freq[k] = key
    observed_freq[k] = observed[i]
  }
  cat("Observed values:",observed_freq,"\n")
  cat("Expected values:",expected_freq,"\n")
  return(sum(((observed_freq-expected_freq)^2)/(expected_freq)))
}

n = 1000
int_pts=seq(0,1,0.1)
observed=c(112, 101, 94, 99, 108, 93, 94, 100, 104, 95)

cdf_func=function(x,a,b)
  return((x-a)/(b-a))

cdf_int_pts=cdf_func(int_pts,0,1)

expected=numeric(0)
for (i in 1:10) 
  expected[i] = n*(cdf_int_pts[i+1]-cdf_int_pts[i])

W=chi_test(expected,observed)
chi_table=16.919
W<chi_table


