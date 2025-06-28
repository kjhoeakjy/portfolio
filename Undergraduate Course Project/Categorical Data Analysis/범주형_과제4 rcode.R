#1
1-pchisq(0.82,df=1)


#2
#(a)
death=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\범주형자료분석\\DeathPenalty(1).dat",header=T)
death      
death$P=factor(death$P, levels = c("yes", "no"))
fit=glm(count~D*V+D*P+P*V,family=poisson,data=death)
summary(fit)

#(b)
exp(0.8678)

#(c)
yes=death$count[c(1,3,5,7)]
no=death$count[c(2,4,6,8)]
n=yes+no
death.1=cbind(death[c(1,3,5,7),c(1,2)],yes,no,n)
death.1
fit2=glm(no/n~D+V,family=binomial,weight=n,data=death.1)
summary(fit2)

#3
#(a)
S=c("yes","yes","no","no","yes","yes","no","no")
E=c("yes","no","yes","no","yes","no","yes","no")
K=c("no","no","no","no","yes","yes","yes","yes")
count=c(1105,411111,4624,157342,14,483,497,1008)
data=data.frame(S=S,E=E,K=K,count=count)
data

#(b)
fit=glm(count~S*E+E*K+S*K,family=poisson,data=data)
summary(fit)

#(c)
sum(abs(count-fitted(fit)))/(2*sum(count))

#(d)
fitted(fit)

#4
#(b)
data=matrix(c(159,8,22,14),nrow=2)
data
mcnemar.test(data,correct=FALSE)
