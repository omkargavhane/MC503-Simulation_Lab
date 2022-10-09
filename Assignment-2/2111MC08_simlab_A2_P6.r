fibonacci_number=function(no){
  fno=0
  sno=1
	#fib_no=c(0,1)
  fib_no=c()
	for(i in 1:no)
	  if(i==1)fib_no=append(fib_no,fno)
    else if(i==2)fib_no=append(fib_no,sno)
    else
		  fib_no=append(fib_no,fib_no[length(fib_no)]+fib_no[length(fib_no)-1])
	return(fib_no)
}
fibonacci_number(10)
