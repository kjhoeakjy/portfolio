#5.6
t5.6=expand.grid(AZT=factor(c("Yes","No"),levels=c("No","Yes")),Race=factor(c("White","Black"),levels=c("Black","White")))
t5.6=data.frame(t5.6,Yes=c(14,32,11,12), No=c(93,81,52,43))
print(t5.6)

fit1=glm(cbind(Yes,No)~AZT+Race, family=binomial, data=t5.6)
summary(fit1)

exp(-0.71946)

qchisq(df=1,0.95)

fit2=glm(cbind(Yes,No)~AZT*Race,family=binomial, data=t5.6)
summary(fit2)

fit3=glm(cbind(Yes,No)~AZT, family=binomial, data=t5.6)
summary(fit3)

fit4=glm(cbind(Yes,No)~Race, family=binomial, data=t5.6)
summary(fit4)

fit5=glm(cbind(Yes,No)~1, family=binomial, data=t5.6)
summary(fit5)

#표로 만들어서 비교
model=c("AZT*Race","AZT+Race","AZT","Race","null")
df=c(0,1,2,2,3)
deviance=c(fit1$deviance,fit2$deviance,fit3$deviance,fit4$deviance,fit5$deviance)
AIC=c(fit1$aic,fit2$aic,fit3$aic,fit4$aic,fit5$aic)
compare_model=c("_","2-1","3-2","4-3","5-3")
compare_dev=c("_",c(fit2$deviance-fit1$deviance, fit3$deviance-fit2$deviance, fit4$deviance-fit3$deviance, fit5$deviance-fit3$deviance))
compare_df=c(1,1,1,1,1)
model_diag=data.frame(model,df,deviance,AIC,compare_model,compare_dev,compare_df)
print(model_diag)

#fit3 선택
residuals(fit3,type='pearson')
residuals(fit3, type = "pearson")/sqrt(1 - lm.influence(fit3)$hat)



#6.5, 6.7
sp=factor(c("<117","117-126","127-136","137-146","147-156","157-166","167-186",">186"))
n=c(156,252,284,271,139,85,99,43)
obs=c(3,17,12,16,12,8,16,8)
bp=c(111.5,121.5,131.5,141.5,151.5,161.6,176.5,191.5)
t6.5=data.frame(sp,bp,n,obs)
t6.5

fit6=glm(obs/n~bp,family=binomial,weights=n,data=t6.5)
summary(fit6)
qchisq(0.95,df=6)

#standard residual
s_res=resid(fit6, type = "pearson")/sqrt(1 - lm.influence(fit6)$hat)

r1=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-1,])
r2=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-2,])
r3=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-3,])
r4=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-4,])
r5=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-5,])
r6=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-6,])
r7=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-7,])
r8=glm(obs/n~bp,family=binomial,weights=n,data=t6.5[-8,])

G=c(fit6$deviance-r1$deviance,fit6$deviance-r2$deviance,fit6$deviance-r3$deviance,fit6$deviance-r4$deviance,fit6$deviance-r5$deviance,fit6$deviance-r6$deviance,fit6$deviance-r7$deviance,fit6$deviance-r8$deviance)
G

#pearson residual
p_res=resid(fit6, type = "pearson")
p_diff=c(p_res[1]^2,p_res[2]^2,p_res[3]^2,p_res[4]^2,p_res[5]^2,p_res[6]^2,p_res[7]^2,p_res[8]^2)
p_diff
#standardized residual (table 6.7)
s_diff=c(s_res[1]^2,s_res[2]^2,s_res[3]^2,s_res[4]^2,s_res[5]^2,s_res[6]^2,s_res[7]^2,s_res[8]^2)
s_diff
#df
df = c((fit6$coefficients[2] - r1$coefficients[2]) / sqrt(vcov(fit6)[2, 2]),(fit6$coefficients[2] - r2$coefficients[2]) / sqrt(vcov(fit6)[2, 2]),(fit6$coefficients[2] - r3$coefficients[2]) / sqrt(vcov(fit6)[2, 2]),(fit6$coefficients[2] - r4$coefficients[2]) / sqrt(vcov(fit6)[2, 2]),(fit6$coefficients[2] - r5$coefficients[2]) / sqrt(vcov(fit6)[2, 2]),(fit6$coefficients[2] - r6$coefficients[2]) / sqrt(vcov(fit6)[2, 2]),(fit6$coefficients[2] - r7$coefficients[2]) / sqrt(vcov(fit6)[2, 2]),(fit6$coefficients[2] - r8$coefficients[2]) / sqrt(vcov(fit6)[2, 2]))
df

#표로 만들어서 비교
diag2=data.frame(bp,df,p_diff,s_diff,G)
diag2

fit7=glm(obs/n~1,family=binomial,weights=n,data=t6.5)
summary(fit7)

#6.7
s_res.ind=resid(fit7, type = "pearson")/sqrt(1 - lm.influence(fit7)$hat)

r1.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-1,])
r2.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-2,])
r3.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-3,])
r4.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-4,])
r5.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-5,])
r6.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-6,])
r7.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-7,])
r8.ind=glm(obs/n~1,family=binomial,weights=n,data=t6.5[-8,])

G.ind=c(fit7$deviance-r1.ind$deviance,fit7$deviance-r2.ind$deviance,fit7$deviance-r3.ind$deviance,fit7$deviance-r4.ind$deviance,fit7$deviance-r5.ind$deviance,fit7$deviance-r6.ind$deviance,fit7$deviance-r7.ind$deviance,fit7$deviance-r8.ind$deviance)

#pearson residual
p_res.ind=resid(fit7, type = "pearson")
p_diff.ind=c(p_res.ind[1]^2,p_res.ind[2]^2,p_res.ind[3]^2,p_res.ind[4]^2,p_res.ind[5]^2,p_res.ind[6]^2,p_res.ind[7]^2,p_res.ind[8]^2)
#standardized residual (table 6.7)
s_diff.ind=c(s_res.ind[1]^2,s_res.ind[2]^2,s_res.ind[3]^2,s_res.ind[4]^2,s_res.ind[5]^2,s_res.ind[6]^2,s_res.ind[7]^2,s_res.ind[8]^2)

diag2.ind=data.frame(p_diff.ind,s_diff.ind,G.ind)
diag2.ind

exp(-2.5987)/(1+exp(-2.5987))



#7.1
t7.1=data.frame(LogDose=c(1.6907,1.7242,1.7552,1.7842,1.8113,1.8369,1.8610,1.8839),n=c(59,60,62,56,63,59,62,60),y=c(6,13,18,28,52,53,61,60))
fit8=glm(y/n ~ LogDose, weights=n, family=binomial,data=t7.1)
summary(fit8)

fit9=glm(y/n ~ LogDose, weights=n,family=binomial(link=probit),data=t7.1)
summary(fit9)

fit10=glm(y/n ~ LogDose, weights=n,family=binomial(link=cloglog),data=t7.1)
summary(fit10)

plot(t7.1$LogDose,t7.1$y/t7.1$n,ylim=c(0,1), xlab="LogDose",ylab="Proportion",bty="L")
axis(side=1, at=seq(from=1.5,to=2,by=0.05))
lines(t7.1$LogDose,predict(fit8, type="response"),lty=1)
lines(t7.1$LogDose,predict(fit9, type="response"),lty=2)
lines(t7.1$LogDose,predict(fit10, type="response"),lty=3)

