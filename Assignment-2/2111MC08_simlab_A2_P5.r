bubble_sort=function(set)
{
  n = length(set)
  for (i in 1 : (n - 1)) {
    for (j in 1 : (n - i)) {
      if (set[j] > set[j + 1]) {
        temp = set[j]
        set[j] = set[j + 1]
        set[j + 1] = temp
      }
    }
  }
  return(set)
}

find_mean=function(vector){
  sum=0
	for(e in vector)	
	  sum=sum+e
	return(sum/length(vector))
}

find_median=function(vector){
  vector=bubble_sort(vector)
  lov=length(vector)
  if(lov%%2==0)
    return(sum(c(vector[lov/2]+vector[lov/2+1]))/2)
  else
    return(vector[ceiling(lov/2)])
}

find_mode=function(vector){
  freq=table(vector)
  return(names(freq)[which(freq==max(freq))])
  #return(3*find_median(vector)-2*find_mean(vector))
}
vector=c(8,7,6,3,3)
find_mean(vector)
find_median(vector)
find_mode(vector)
