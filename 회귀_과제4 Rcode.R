#7.3
P198=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P198.txt",header=T)

#회귀모형
fit1=lm(Y~X1+X2+X3,data=P198)
summary(fit1)

#표준화잔차
Fitted = fitted(fit1)
Rstandard = as.vector(rstandard(fit1))
Rstandard
plot(Fitted,Rstandard,pch=19,cex=1) 

#지렛값
Hatvalues = as.vector(hatvalues(fit1))
Hatvalues
plot(Hatvalues,pch=19,cex=1) 

#Cook's distance
Cooks.distance = as.vector(cooks.distance(fit1))
Cooks.distance
plot(Cooks.distance,pch=19,cex=1) 

#DFITs
Dffits = as.vector(dffits(fit1))
Dffits
plot(Dffits,pch=19,cex=1) 



#7.4

#회귀모형
fit2=lm(Y~X1+X2+X3+factor(Region),data=P198)
summary(fit2)


#가중치 계산

residuals = residuals(fit2)
residuals.1 = residuals[P198$Region == 1]
residuals.2 = residuals[P198$Region == 2]
residuals.3 = residuals[P198$Region == 3]
residuals.4 = residuals[P198$Region == 4]
sig1=sum(residuals.1^2)/8
sig2=sum(residuals.2^2)/11
sig3=sum(residuals.3^2)/15
sig4=sum(residuals.4^2)/12

wgt.1=9*sig1/sum(residuals.1^2)
wgt.2=12*sig1/sum(residuals.2^2)
wgt.3=16*sig1/sum(residuals.3^2)
wgt.4=13*sig1/sum(residuals.4^2)


P198$Wgt = 0
P198$Wgt[P198$Region==1] = 1/(wgt.1^2)
P198$Wgt[P198$Region==2] = 1/(wgt.2^2)
P198$Wgt[P198$Region==3] = 1/(wgt.3^2)
P198$Wgt[P198$Region==4] = 1/(wgt.4^2)
fit3 = lm(Y~X1+X2+X3+factor(Region),data=P198,weights=Wgt)
summary(fit3)

Fitted.3 = fitted(fit3)
Rstandard.3 = as.vector(rstandard(fit3))
plot(Fitted,Rstandard.3,pch=19,cex=1) 
plot(P198$Region,Rstandard.3,pch=19,cex=1)

library(car)
anova(fit3)




#8.4
#(a)
P229=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P229-30.txt",header=T)
P229$Day=P229$Time

#선형모델
fit4=lm(DJIA~Day,data=P229)
summary(fit4)

#선형성,잔차
plot(DJIA~Day,data=P229)
Rstandard4 = as.vector(rstandard(fit4))
plot(P229$Day,Rstandard4)

#(b)
library(dplyr)
P229.1 = P229 %>%
  mutate(T_1 = lag(DJIA))
head(P229.1)

fit5=lm(DJIA~T_1,data=P229.1)
summary(fit5)


Rstandard5 = as.vector(rstandard(fit5))
plot(Rstandard5)

library(lmtest)

dw_result=dwtest(fit5)
print(dw_result)
#유의한 양의 자기상관


#(c)
fit6=lm(log(DJIA)~T_1,data=P229.1)
summary(fit6)

Rstandard6 = as.vector(rstandard(fit6))
plot(Rstandard6)

dw_result.1=dwtest(fit6)
print(dw_result.1)
#유의한 양의 자기상관, p-value 감소


#8.5
#(a)
P229.2=P229.1[P229.1$Day<=130, ]
head(P229.2)
fit7=lm(DJIA~T_1,data=P229.2)
summary(fit7)

residuals.2 = residuals(fit7)
rms=sum(residuals.2^2)/130
rms

#(b)
P229.3=P229.1[c(131:145), ]
fit8=lm(DJIA~T_1,data=P229.3)
summary(fit8)
residuals.3 = residuals(fit8)
residuals.3

#(c)
sum(residuals.3^2)/15
#예측오차와 잔차는 동일

#(d)
P229.4=P229.1[P229.1$Day>130, ]
fit9=lm(DJIA~T_1,data=P229.4)
summary(fit9)
residuals.4 = residuals(fit9)
sum(residuals.4^2)/132

#(e)
plot(P229.1$DJIA~P229.1$Day)
plot(P229.2$DJIA~P229.2$Day)
plot(P229.3$DJIA~P229.3$Day)
plot(P229.4$DJIA~P229.4$Day)


#8.6
#(a) 
library(car)
#개별검정
linearHypothesis(fit5, "(Intercept) = 0")
linearHypothesis(fit5, "T_1 = 1")
#동시검정
linearHypothesis(fit5, c("(Intercept) = 0", "T_1 = 1"))

#(b)
DJIA_diff=diff(P229.1$DJIA)
P229.1$logDJIA=log(P229.1$DJIA)
log_DJIA_diff=diff(P229.1$logDJIA)
#정규성
shapiro.test(DJIA_diff)
shapiro.test(log_DJIA_diff)
#평균0
t.test(DJIA_diff, mu = 0)
t.test(log_DJIA_diff, mu = 0)
#상수분산
library(lmtest)
model=lm(DJIA_diff~seq_along(DJIA_diff))
gqtest(model)
logmodel=lm(log_DJIA_diff~seq_along(log_DJIA_diff))
gqtest(logmodel)


#(c)
recent=read.csv("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\Download Data - INDEX_US_DOW JONES GLOBAL_DJIA (1).csv", header=T)
recent$day=c(1:250)
recent$Open=as.numeric(gsub(",", "", recent$Open))
head(recent)

#선형모델
fit10=lm(Open~day,data=recent)
summary(fit10)

#선형성,잔차
plot(Open~day,data=recent)
Rstandard10 = as.vector(rstandard(fit10))
plot(recent$day,Rstandard10)


#전날의 데이터 적합합
library(dplyr)
recent.1 = recent %>%
  mutate(T_1 = lag(Open))
head(recent.1)

fit11=lm(Open~T_1,data=recent.1)
summary(fit11)
Rstandard11 = as.vector(rstandard(fit11))
plot(Rstandard11)

library(lmtest)
dw_result.2=dwtest(fit11)
print(dw_result.2)
#유의한 양의 자기상관


fit12=lm(log(Open)~T_1,data=recent.1)
summary(fit12)
Rstandard12 = as.vector(rstandard(fit12))
plot(Rstandard12)

dw_result.3=dwtest(fit12)
print(dw_result.3)
#유의한 양의 자기상관, p-value 감소


library(car)
#개별검정
linearHypothesis(fit11, "(Intercept) = 0")
linearHypothesis(fit11, "T_1 = 1")
#동시검정
linearHypothesis(fit11, c("(Intercept) = 0", "T_1 = 1"))

#차분값
recent_diff=diff(recent.1$Open)
recent.1$logOpen=log(recent.1$Open)
log_recent_diff=diff(recent.1$logOpen)
#정규성
shapiro.test(recent_diff)
shapiro.test(log_recent_diff)
#평균0
t.test(recent_diff, mu = 0)
t.test(log_recent_diff, mu = 0)
#상수분산
library(lmtest)
model=lm(recent_diff~seq_along(recent_diff))
gqtest(model)
logmodel=lm(log_recent_diff~seq_along(log_recent_diff))
gqtest(logmodel)

