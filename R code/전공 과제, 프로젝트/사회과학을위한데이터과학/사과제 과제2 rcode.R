#2

a=c(41.5,50.7,36.6,37.3,34.2,45.0,48.0,43.2,47.7,42.2,43.2,44.6,48.4,46.4,46.8,39.2,37.3,43.5,44.3,43.3)

mean(a)
sd(a)
t.test(a)$conf.int
hist(a)
stem(a)
boxplot(a)

#4
data=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\사회과학을위한데이터과학\\student.dat",header=T)
hw=cbind(data$schoolsup,data$G3)
schoolsup1=ifelse(data$schoolsup=='yes',1,0)
schoolsup1
prob=mean(schoolsup1)
prob

t.test(schoolsup1)$conf.int
