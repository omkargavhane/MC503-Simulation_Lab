n <- 20
sample <- runif(n, 2, 5)
ord_samp <- sort(sample)
emp_cdf <- (0:n)/n
uniform_cdf <- function(x){
	cdf <- ((x-2)/3)
	cdf
}
theo_cdf <- uniform_cdf(ord_samp)
emp_cdf_new <- c(0,rep(emp_cdf[2:n], each=2),1)
theo_cdf_new <- rep(theo_cdf, each=2)
abs_diff <- abs(emp_cdf_new-theo_cdf_new)
ks_statistic <- max(abs_diff)
ks_statistic
ks.test(sample, uniform_cdf)
# For 5% level of significance, we can not reject the null hypthesis

#P(Dn > dcal) = 
