belongs_to=function(element,vector){
	for(e in vector)
		if(element==e)
			return(TRUE)
	return(FALSE)

}
element=8
vector=c(4,8,10,5,6,12)
belongs_to(element,vector)
