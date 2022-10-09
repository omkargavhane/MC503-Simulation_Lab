df=read.csv("medals_total.csv")
#I
ind=subset(df,Country.Code=="IND")
usa=subset(df,Country.Code=="USA")
chn=subset(df,Country.Code=="CHN")
dft=rbind(ind,usa,chn)
sum(dft$Total)
#II
uk=subset(df,Country.Code=="GBR")
uk_medals=c(uk$Gold.Medal,uk$Silver.Medal,uk$Bronze.Medal)
barplot(uk_medals)
chn_medals=c(chn$Gold.Medal,chn$Silver.Medal,chn$Bronze.Medal)
barplot(chn_medals,xlab="Medals type",ylab="No of Medals")
#III
jpn=subset(df,Country.Code=="JPN")
brazil=subset(df,Country.Code=="BRA")
#IV
#Filter the dataset, only for 5 counties as India, USA, Japan, China and Brazil.
a=c(ind$Total,usa$Total,jpn$Total,chn$Total,brazil$Total)
label=c("India","usa","japan","brazil","chaina")
pie(a,label,main="medals respective contries")