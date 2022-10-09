x=1:15
y=matrix(x,nrow=5,ncol=3)
z=letters[1:5]
df=data.frame(y,z)
df
write.csv(df, file = "df.csv")
