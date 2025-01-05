#table10.1
t10.1=data.frame(expand.grid(cig=c("Yes","No"),alc=c("Yes","No"),mar=c("Yes","No"),sex=c("female","male"),race=c("white","other")), count=c(405,13,1,1,268,218,17,117,453,28,1,1,228,201,17,133,23,2,0, 0,23, 19,1,12,30,1,1,0,19,18,8,17))
t10.1


fit1=glm(count ~ cig + alc + mar + sex + race +sex*race, family=poisson,data=t10.1)
fit2=glm(count ~ .^2, family=poisson, data=t10.1)
fit3=glm(count~.^3,data=t10.1,family=poisson)
fit4=glm(count~cig*mar+cig*sex+cig*race+alc*mar+alc*sex+alc*race+mar*sex+mar*race+sex*race,data=t10.1,family=poisson)
fit5=glm(count~cig*alc+cig*mar+cig*sex+cig*race+alc*sex+alc*race+mar*sex+mar*race+sex*race,data=t10.1,family=poisson)
fit6=glm(count~cig*alc+cig*sex+cig*race+alc*mar+alc*sex+alc*race+mar*sex+mar*race+sex*race,data=t10.1,family=poisson)
fit7=glm(count~cig*alc+cig*mar+cig*sex+cig*race+alc*mar+alc*race+mar*sex+mar*race+sex*race,data=t10.1,family=poisson)
fit8=glm(count~cig*alc+cig*mar+cig*sex+cig*race+alc*mar+alc*sex+mar*sex+mar*race+sex*race,data=t10.1,family=poisson)
fit9=glm(count~cig*alc+cig*mar+cig*race+alc*mar+alc*sex+alc*race+mar*sex+mar*race+sex*race,data=t10.1,family=poisson)
fit10=glm(count~cig*alc+cig*mar+cig*sex+alc*mar+alc*sex+alc*race+mar*sex+mar*race+sex*race,data=t10.1,family=poisson)
fit11=glm(count~cig*alc+cig*mar+cig*sex+cig*race+alc*mar+alc*sex+alc*race+mar*race+sex*race,data=t10.1,family=poisson)
fit12=glm(count~cig*alc+cig*mar+cig*sex+cig*race+alc*mar+alc*sex+alc*race+mar*sex+sex*race,data=t10.1,family=poisson)
fit13=glm(count~alc*cig+alc*mar+cig*mar+alc*sex+alc*race+sex*mar+sex*race+mar*race,data=t10.1,family=poisson)
fit14=glm(count~alc*cig+alc*mar+cig*mar+alc*sex+alc*race+sex*mar+sex*race,data=t10.1,family=poisson)
fit15=glm(count~alc*cig+alc*mar+cig*mar+alc*sex+alc*race+sex*race,data=t10.1,family=poisson)

model=c(1:15)
Gsq=c(fit1$deviance,fit2$deviance,fit3$deviance,fit4$deviance,fit5$deviance,fit6$deviance,fit7$deviance,fit8$deviance,fit9$deviance,fit10$deviance,fit11$deviance,fit12$deviance,fit13$deviance,fit14$deviance,fit15$deviance)
Xsq=c(sum(resid(fit1, type = "pearson")^2),
      sum(resid(fit2, type = "pearson")^2),
      sum(resid(fit3, type = "pearson")^2),
      sum(resid(fit4, type = "pearson")^2),
      sum(resid(fit5, type = "pearson")^2),
      sum(resid(fit6, type = "pearson")^2),
      sum(resid(fit7, type = "pearson")^2),
      sum(resid(fit8, type = "pearson")^2),
      sum(resid(fit9, type = "pearson")^2),
      sum(resid(fit10, type = "pearson")^2),
      sum(resid(fit11, type = "pearson")^2),
      sum(resid(fit12, type = "pearson")^2),
      sum(resid(fit13, type = "pearson")^2),
      sum(resid(fit14, type = "pearson")^2),
      sum(resid(fit15, type = "pearson")^2))
DF=c(25,16,6,17,17,17,17,17,17,17,17,17,18,19,20)
comp=data.frame(model,Gsq, Xsq, DF)
comp

#fit1,2,3
qchisq(0.95,df=25)
qchisq(0.95,df=16)
qchisq(0.95,df=6)

#fit4-12
qchisq(0.95,df=1)
Gsq[4]-Gsq[2]
Xsq[4]-Xsq[2]
Gsq[5]-Gsq[2]
Xsq[5]-Xsq[2]
Gsq[6]-Gsq[2]
Xsq[6]-Xsq[2]
Gsq[7]-Gsq[2]
Xsq[7]-Xsq[2]
Gsq[8]-Gsq[2]
Xsq[8]-Xsq[2]
Gsq[9]-Gsq[2]
Xsq[9]-Xsq[2]
Gsq[10]-Gsq[2]
Xsq[10]-Xsq[2]
Gsq[11]-Gsq[2]
Xsq[11]-Xsq[2]
Gsq[12]-Gsq[2]
Xsq[12]-Xsq[2]

#fit13,14,15
Gsq[13]-Gsq[10]
Xsq[13]-Xsq[10]
Gsq[14]-Gsq[13]
Xsq[14]-Xsq[13]
Gsq[15]-Gsq[14]
Xsq[15]-Xsq[14]

#fit14 선택
qchisq(0.95,df=19)
res14=resid(fit14, type = "pearson")/sqrt(1 - lm.influence(fit14)$hat)
res14


#table10.3
u1=c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
v1=c(rep(c(1,2,3,4),4))
count=c(81,68,60,38,24,26,29,14,18,41,74,42,36,57,161,157)
pre=factor(u1,levels=4:1)
teen=factor(v1,levels=4:1)

t10.3=data.frame(pre=pre,teen=teen,u1=u1,v1=v1,count=count)
t10.3


model1=glm(count ~ pre + teen + u1:v1, data=t10.3,family=poisson)
model2=glm(count ~ pre + teen, data=t10.3,family=poisson)
summary(model1)
qchisq(0.95,df=9)
summary(model2)
qchisq(0.95,df=8)

model2$deviance-model1$deviance
qchisq(0.95,model2$df.residual-model1$df.residual)

#local OR, exp(beta) 비교
model1$fitted.values
(80.9*23.1)/(67.6*20.8)
exp(0.28584)

(65.7*155.5)/(48.8*157.4)
exp(0.28584)
