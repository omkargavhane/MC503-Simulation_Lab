p=0.5
u=runif(1000)
s=c()
for(e in u){
  if(e<=p)
    s=append(s,1)
  else
    s=append(s,0)
}
s
length(s[s==0])
length(s[s==1])