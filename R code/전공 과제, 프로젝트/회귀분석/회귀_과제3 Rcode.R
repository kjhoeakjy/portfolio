#5.2
P140=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P140.txt",header=T)
P140$RaceTest = P140$RACE*P140$TEST

#RM
P140.fit1 = lm(JPERF~TEST,data=P140)
summary(P140.fit1)

#FM, t검정
P140.fit4 = lm(JPERF~TEST+RaceTest,data=P140)
summary(P140.fit4)

Fitted1 = as.vector(fitted(P140.fit4))
Residual1 = as.vector(residuals(P140.fit4))
Rstandard1 = as.vector(rstandard(P140.fit4))
P140.1 = data.frame(P140,Fitted1,Residual1,Rstandard1)

plot(Rstandard1~RaceTest,data=P140.1,pch=19,cex=1)

#F검정
anova(P140.fit1,P140.fit4)


#5.9
P160=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P160.txt",header=T)
P160$GI = P160$G*P160$I
fit1=lm(V~I+D+W+GI+P+N,data=P160)
summary(fit1)

fit2=lm(V~D+GI+I:P,data=P160)
summary(fit2)

#5.10
P160$D1=ifelse(P160$D==1,1,0)
P160$D2=ifelse(P160$D==-1,1,0)

fit=lm(V~I+D1+D2+W+GI+P+N,data=P160)
summary(fit)


#6.5
P160$Y=log(P160$V/(1-P160$V))
fit1=lm(V~I+D+W+GI+P+N,data=P160)
summary(fit1)
#6.3
P187=read.delim("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P187.txt",header=T)
fit1=lm(R~P,data=P187)
summary(fit1)

plot(R~P,data=P187,pch=19,cex=1)

Fitted1 = as.vector(fitted(fit1))
Rstandard1 = as.vector(rstandard(fit1))
plot(Rstandard1~Fitted1,pch=19,cex=1)

P187$R1=P187$R**.5
P187$P1=P187$P**.5
P187$R2=log(P187$R)
P187$P2=log(P187$P)
P187$R3=P187$R**-.5
P187$P3=P187$P**-.5
P187$R4=P187$R**-1
P187$P4=P187$P**-1

plot(R1~P,data=P187,pch=19,cex=1)
plot(R1~P1,data=P187,pch=19,cex=1)
plot(R2~P,data=P187,pch=19,cex=1)
plot(R2~P2,data=P187,pch=19,cex=1)
plot(R3~P,data=P187,pch=19,cex=1)
plot(R3~P3,data=P187,pch=19,cex=1)
plot(R4~P,data=P187,pch=19,cex=1)
plot(R4~P4,data=P187,pch=19,cex=1)

fit2=lm(R2~P2,data=P187)
summary(fit2)

Fitted2 = as.vector(fitted(fit2))
Rstandard2 = as.vector(rstandard(fit2))
plot(Rstandard2~Fitted2,pch=19,cex=1)

P187_1 = P187[-c(15,22,23),]
plot(R2~P2,data=P187_1,pch=19,cex=1)

fit3=lm(R2~P2,data=P187_1)
summary(fit3)

Fitted3 = as.vector(fitted(fit3))
Rstandard3 = as.vector(rstandard(fit3))
plot(Rstandard3~Fitted3,pch=19,cex=1)

fit2=lm(Y~I+D+W+GI+P+N,data=P160)
summary(fit2)

Fitted1 = as.vector(fitted(fit1))
Residual1 = as.vector(residuals(fit1))
Rstandard1 = as.vector(rstandard(fit1))
P160.1 = data.frame(P160,Fitted1,Residual1,Rstandard1)
plot(Rstandard1~Fitted1,data=P160.1,pch=19,cex=1)
plot(V~Fitted1,data=P160.1)

Fitted2 = as.vector(fitted(fit2))
Residual2 = as.vector(residuals(fit2))
Rstandard2 = as.vector(rstandard(fit2))
P160.2 = data.frame(P160,Fitted2,Residual2,Rstandard2)
plot(Rstandard2~Fitted2,data=P160.2,pch=19,cex=1)
plot(Y~Fitted2,data=P160.2)

plot(fit1,which=2,col=c("red"))  
plot(fit2,which=2,col=c("red")) 

#6.8
P190=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P190.txt",header=T)
P190$t=P190$Year-1987
plot(Price~t,data=P190)
P190$lnprice=log(P190$Price)
fit1=lm(lnprice~t,data=P190)
summary(fit1)

P190$t1=ifelse(P190$t<=4,0,1)
fit2=lm(lnprice~t+t1+t:t1,data=P190)
summary(fit2)

