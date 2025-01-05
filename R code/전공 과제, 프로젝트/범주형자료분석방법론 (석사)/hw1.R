#범주형자료분석방법론 hw1
#김연주
#2021250461
#
#
#Table 3.1
T3.1= matrix(c(54,10325,25,51790), nrow = 2, byrow = TRUE)
rownames(T3.1)=c("No", "Yes")
colnames(T3.1)=c("Fatal", "Nonfatal")
T3.1

library(epitools)
epitools::oddsratio(T3.1,conf=0.95,correct=FALSE,method="wald")

#Table 3.2
T3.2=matrix(c(9,23,28,8,39,48,27,88,89,8,49,19,47,179,104,236,706,293),nrow=3)
Degree=c("Less","HS","College")
God=c("1","2","3","4","5","6")
dimnames(T3.2)=list('Degree'= Degree, 'God'=God)
T3.2

chisq.test(T3.2)$expected
chisq.test(T3.2)
library(MASS)
lrt=loglm(~ Degree + God, data = T3.2)
summary(lrt)


#Table 3.7
T3.7=matrix(c(13,23,14,29,59,67,15,47,54),nrow=3)
PI=c("Liberal","Moderate","Conservative")
Happiness=c("Not","Pretty","Very")
dimnames(T3.7)=list('PI'= PI, 'Happiness'=Happiness)
T3.7

library(vcdExtra)
CMHtest(T3.7,rscores = c(1,2,3),csores=c(1,2,3))
1-pchisq(5.8517,df=1)

GKgamma(T3.7)
z.value=0.185/0.078
(1-pnorm(z.value))*2


#Table 3.9
T3.9=matrix(c(3,1,1,3),nrow=2)
Poured=c("Milk","Tea")
Guess=c("Milk","Tea")
dimnames(T3.9)=list('Poured'= Poured, 'Guess'=Guess)
T3.9

chisq.test(T3.9)$expected
fisher.test(T3.9)
fisher.test(T3.9,alternative="greater")
