#6
student=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\사회과학을위한데이터과학\\student.dat",header=T)
student.1=student[,c(2,6,14,16)]
head(student.1)

#(a)
schoolsup_yes = subset(student.1, schoolsup=="yes")
schoolsup_no = subset(student.1, schoolsup=="no")

t.test(schoolsup_yes$G3,schoolsup_no$G3, alternative="greater")

#(b)
t.test(student.1$G1,student.1$G3, alternative="greater")

#(c)
student_male = subset(student.1, sex=="M")
student_female = subset(student.1, sex=="F")
student_male$sup=ifelse(student_male$schoolsup=='yes',1,0)
student_female$sup=ifelse(student_female$schoolsup=='yes',1,0)
length(student_male$sup)
sum(student_male$sup)
length(student_female$sup)
sum(student_female$sup)

prop.test(x = c(15,36), n = c(184,198), correct=FALSE)
