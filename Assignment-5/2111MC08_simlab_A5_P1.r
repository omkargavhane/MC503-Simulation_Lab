#I
nrow(iris)
ncol(iris)
#II
summary(iris$Sepal.Length)
summary(iris$Sepal.Width)
#III
unique_species=unique(iris$Species)
for(i in 1:length(unique_species)){
  print(unique_species[i])
  print(nrow(subset(iris,Species==unique_species[i])))
}
#IV
df_Petal_length_gt2=subset(iris,Petal.Length>2)
df_Petal_length_gt2