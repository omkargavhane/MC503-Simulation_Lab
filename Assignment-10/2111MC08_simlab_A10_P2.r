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

hours = c(0,1,2,3,4,5,6,7)
observed = c(22, 53, 58, 39, 20, 5, 2, 1)

lambda = sum((hours*observed))/ sum(observed)

pdf_func=function(x,lambda)
  return((exp(-1*lambda)*(lambda^x))/factorial(x))

expected = sum(observed)*pdf_func(hours,lambda)

W=chi_test(expected,observed)
chi_table=9.488
W<chi_table