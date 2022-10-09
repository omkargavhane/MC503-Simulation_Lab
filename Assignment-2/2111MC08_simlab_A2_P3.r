find_max_min=function(set){
	#Finding min max using tournament approach which calculates min and max simultanously
	#check whether the no of elements is odd or even if odd then take out the last element
	last_element=''
	if(length(set)%%2!=0){
		last_element=set[length(set)]
		set=set[-length(set)]
	}
	#for first two elements compare them and set min and max accordingly
	if(set[1]<set[2]){
		min=set[1]
		max=set[2]
	}
	else{
		min=set[2]
		max=set[2]
	}
	#remove them from set
	set=set[-1][-1]
	#now for rest elements match will be played between successive two elements and set min max accordingly
	#here match means who is bigger or smaller
	for(i in 1:(length(set)/2)){
		opponent_1=((i-1)*2)+1
		opponent_2=opponent_1+1
		if(set[opponent_1]<set[opponent_2]){
			cur_min=set[opponent_1]
			cur_max=set[opponent_2]
		}
		else{
			cur_min=set[opponent_2]
			cur_max=set[opponent_1]
		}
		if(cur_min<min)
			min=cur_min
		if(cur_max>max)
			max=cur_max
	}
	#if no of elements is odd then there is last element which we have taken out earlier
	#now compare it with min and max and set them accordingly
	if(last_element!=''){
		if(last_element<min)
			min=last_element
		if(last_element>max)
			max=last_element
	}
	return(list('min'=min,'max'=max))
}
#***no of comparisons***
#if no of elements(n) is even
#3*(n/2)-2
#if no of elements(n) is odd
#3*[(n-1)/2]-2+2
set=c(-4,44.7, -2, 40, 54, 1, -3, 4)
res=find_max_min(set)
print(res$min)
print(res$max)
