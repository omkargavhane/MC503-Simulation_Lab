#Marks in Statistics 20-25 25-30 30-35 35-40 40-45 45-50
#Number of student    5     4      3     4     2     1

marks=c(20,22,21,25,24,26,27,26,28,31,35,33,36,37,37,40,43,45,49)
hist(marks, 
     xlab="Marks in Statistics",
     ylab="Number of Students",
     xlim=c(20,50),
     breaks=6
)

summary(marks)
v=table(marks)
mode=c()
for(i in 1:length(v))
  if(v[i]==max(v))
    mode=append(mode,names(v[i]))
mode