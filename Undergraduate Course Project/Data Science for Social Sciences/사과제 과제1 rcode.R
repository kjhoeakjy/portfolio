#(b)
data=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\사회과학을위한데이터과학\\student.dat",header=T)
head(data,20)

#(c)
freqtable1=table(data$studytime)
freqtable1
barplot(freqtable1)
pie(freqtable1)

#(d)
freqtable2=table(data$schoolsup)
freqtable2
barplot(freqtable2)
pie(freqtable2)

#(e)
hist(data$G3)
stem(data$G3)
boxplot(data$G3)

#(f)
contin1=table(data$schoolsup,data$studytime)
contin1
barplot(contin1,col=c('red','blue'), beside=T,legend=c('no','yes'))

#(g)
contin2=table(data$schoolsup,data$G3)
contin2
boxplot(G3~schoolsup,data=data)

#(h)
plot(data$G1,data$G2)

#(i)
summary(data$G3)
sd(data$G3)
range(data$G3)
IQR(data$G3)

#(j)
data$G3_adj=ifelse(data$G3==0,NA,data$G3)
head(data,20)
summary(data$G3_adj,na.rm=T)
sd(data$G3_adj,na.rm=T)
range(data$G3_adj,na.rm=T)
IQR(data$G3_adj,na.rm=T)
