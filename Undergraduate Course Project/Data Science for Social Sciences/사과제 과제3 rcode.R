#5
student=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\사회과학을위한데이터과학\\student.dat",header=T)
student.1=student[,c(6,16)]
student.1$sup=ifelse(student.1$schoolsup=='yes',1,0)
n=382
#(a)
freq = sum(student.1$sup)
prop.test(freq, n, p=0.15, alternative="two.sided", correct=FALSE)

#(b)
z.test(student.1$sup)$conf.int

#(c)
t.test(student.1$G3, mu=10,alternative='greater')

#(d)
t.test(student.1$G3)$conf.int
