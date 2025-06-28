#1

library(dplyr)

n1=276
n2=468
n3=87

ybar1=10.4
ybar2=7.4
ybar3=8.3

sd1=17.8
sd2=13.6
sd3=15.6

set.seed(123) 

data=data.frame(
  happiness = factor(rep(c("very", "pretty", "not very"), times=c(n1, n2, n3))),
  good_friends = c(rnorm(n1, ybar1,sd1), rnorm(n2, ybar2,sd2), rnorm(n3, ybar3, sd3)))


anova_results= aov(good_friends ~ happiness, data = data)
summary(anova_results)


#(d)
confint(anova_results)

#(e)
confint(anova_results,level=1-0.05/3)

#2
student=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\사회과학을위한데이터과학\\student.dat",header=T)
student.1=student[,c(4,15)]
head(student.1)

traveltime_1 = subset(student.1, traveltime==1)
traveltime_2 = subset(student.1, traveltime==2)
traveltime_3 = subset(student.1, traveltime==3)
traveltime_4 = subset(student.1, traveltime==4)

n1 = length(traveltime_1)
n2 = length(traveltime_2)
n3 = length(traveltime_3)
n4 = length(traveltime_4)

ybar1 = mean(traveltime_1$G2)
ybar2 = mean(traveltime_2$G2)
ybar3 = mean(traveltime_3$G2)
ybar4 = mean(traveltime_4$G2)

s1 = sd(traveltime_1$G2)
s2 = sd(traveltime_2$G2)
s3 = sd(traveltime_3$G2)
s4 = sd(traveltime_4$G2)


data=data.frame(
  traveltime = factor(rep(c(1,2,3,4), times=c(n1,n2,n3,n4))),
  grade = c(rnorm(n1, ybar1,s1), rnorm(n2, ybar2,s2), rnorm(n3, ybar3, s3),rnorm(n4, ybar4, s4)))

anova_results= aov(grade~ traveltime, data = data)
summary(anova_results)

#(d)
confint(anova_results)

#(e)
confint(anova_results,level=1-0.05/4)

#(f)
TukeyHSD(anova_results)




#과제6
#1

age_groups <- c("18-34", "35-69", "70+")
total_counts <- c(105, 221, 54)
support_counts <- c(71, 113, 16)

support_percentages <- (support_counts / total_counts) * 100

data <- data.frame(
  AgeGroup = age_groups,
  SupportPercentage = support_percentages
)

library(ggplot2)

ggplot(data, aes(x = AgeGroup, y = SupportPercentage, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(SupportPercentage, 1), "%")), vjust = -0.5) +
  ylim(0, 100) +
  labs(title = "Support for Legalizing Marijuana by Age Group",
       x = "Age Group",
       y = "Percentage Supporting Legalization") +
  theme_minimal()


#3
student=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\사회과학을위한데이터과학\\student.dat",header=T)
student.1=student[,c(2,7)]
head(student.1)
contin=table(student.1)
chisq.test(contin, correct = FALSE)

rowsum=rowSums(contin)
tot=sum(contin)
pi1=contin[1,1]/rowsum[1]
pi2=contin[2,1]/rowsum[2]
pihat=(contin[1,1] + contin[2,1])/tot
se0=sqrt(pihat*(1-pihat)*(1/(contin[1,1]+ contin[1,2]) + 1/(contin[2,1] + contin[2,2])))
z=(pi1 - pi2)/se0
pvalue=2*(1-pnorm(abs(z)))
z
pvalue


chisq.test(contin)$stdres


odds_ratio=contin[1,1]*contin[2,2]/(contin[1,2]*contin[2,1])
