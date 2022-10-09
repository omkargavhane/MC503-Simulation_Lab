n=2000
sigma=1;
muu=0;
######## sample generation for gexponential distribution
x <- numeric();
generated_sample <- function(sig,mu){
	for(i in 1:n){
		u=runif(1);
		x[i] <- (mu-sig*(log(1-u)));
	}
	return(x);
}
Generated_data <- generated_sample(1,0);
Generated_data;

### CDF
cdf_function <- function(x,sig1,mu1){
	1-exp(-(x-mu1)/sig1)
}

## Interval
a=min(Generated_data);
b = max(Generated_data);
h=(b-a)/10;
interval <- seq(a,b,h);
interval;

## Observed Value
observed <- rep(0,10)
for(i in 1:10){
	for(j in 1:n){
		if(Generated_data[j] >= interval[i] && Generated_data[j]<= interval[i+1]){
			observed[i] <- observed[i] + 1
		}
	}
}
observed;
sum(observed);

## Expected Value
 expexted <- numeric();
 for(ii in 1:10){
	expexted[ii] <- n*((cdf_function(interval[ii+1],sigma,muu))-(cdf_function(interval[ii],sigma,muu))); 
 }
 expexted;
 sum(expexted);
 
 Chi_test <- sum(((observed-expexted)^2)/(expexted));
 Chi_test;
 
 #chi_value <- 14.067
 chi_value <- qchisq(1-0.05,7)
 
 if(Chi_test-chi_value>0){
 print("rejected")
 }else{
	print("accepted");
 }
 
 
 
 

