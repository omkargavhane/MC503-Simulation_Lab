isprime=function(no){
  if(no==2)
    return(TRUE)
	if(no%%2==0)
		return(FALSE)
	i=3
	while(i<=ceiling(no**0.5)){
		if(no%%i==0)
			return(FALSE)
		i=i+2
	}
	return(TRUE)
}
isprime(10)
