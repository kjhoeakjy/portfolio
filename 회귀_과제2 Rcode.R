#3.3.(a)
F=c(68,75,85,94,86,90,86,68,55,69,91,75,81,91,80,94,94,97,79,84,65,83)
P1=c(78,74,82,90,87,90,83,72,68,69,91,79,89,93,87,91,86,91,81,80,70,79)
P2=c(73,76,79,96,90,92,95,69,67,70,89,75,84,97,77,96,94,92,82,83,66,81)
model1=lm(F~P1)
summary(model1)
#
model2=lm(F~P2)
summary(model2)
#
model3=lm(F~P1+P2)
summary(model3)

#3.15.(a)
data=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P088.txt",header=T)
Age=data$Age
HS=data$HS
Income=data$Income
Black=data$Black
Female=data$Female
Price=data$Price
Sales=data$Sales
model.a=lm(Sales~Age+HS+Income+Black+Female+Price)
summary(model.a)

#3.15.(b)
model.b=lm(Sales~Age+Income+Black+Price)
anova(model.a)
anova(model.b)

#3.15.(C)
confint(model.a,level=0.95)

#3.15.(d)
model.d=lm(Sales~Age+HS+Black+Female+Price)
summary(model.d)

#3.15.(e)
model.e=lm(Sales~Age+Income+Price)
summary(model.e)

#3.15.(f)
model.f=lm(Sales~Income)
summary(model.f)

#3.16.(b)
n = 10  
X1 = rnorm(n, mean = 0.1, sd = 1) 
Y = rnorm(n, mean = 0, sd = 1)  
FM=lm(Y ~ X1)
RM=lm(Y ~ 0)
anova(FM,RM)
X1
Y
summary(FM)
summary(RM)

#4.7.(a)
data['State']=NULL
cor(data)

#4.7.(b)
cor(data)
plot(data)

#4.7.(c)
summary(lm(Sales~Age))
summary(lm(Sales~HS))
summary(lm(Sales~Income))
summary(lm(Sales~Black))
summary(lm(Sales~Female))
summary(lm(Sales~Price))

#4.8.(a)

P083=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P083.txt")
P083.fit1=model1
P083.fit2=model2
P083.fit3=model3

e = as.vector(residuals(P083.fit1))
d = e/sqrt(516.34)
p = as.vector(hatvalues(P083.fit1))
po = p/(1-p);
re = ((p+1)/(1-p))*(d**2/(1-d**2));
plot(re,po,pch=19,cex=1, main="Model 1(P1)")
text(re,po,labels=rownames(P083),pos=4,col="red",cex=1,font=1)

e = as.vector(residuals(P083.fit2))
d = e/sqrt(365.46)
p = as.vector(hatvalues(P083.fit2))
po = p/(1-p);
re = ((p+1)/(1-p))*(d**2/(1-d**2));
plot(re,po,pch=19,cex=1, main="Model 2(P2)")
text(re,po,labels=rownames(P083),pos=4,col="red",cex=1,font=1)

e = as.vector(residuals(P083.fit3))
d = e/sqrt(296.83)
p = as.vector(hatvalues(P083.fit3))
po = p/(1-p);
re = ((p+1)/(1-p))*(d**2/(1-d**2));
plot(re,po,pch=19,cex=1, main="Model 1(P1 & P2)")
text(re,po,labels=rownames(P083),pos=4,col="red",cex=1,font=1)

P083.a = P083[which((rownames(P083) != "15") &(rownames(P083) != "7") & (rownames(P083) != "9")),]
P083.fit.a = lm(F~P1+P2,data=P083.a)

summary(P083.fit.a)


#4.12.(a)
P128=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P128.txt",header=T)
P128.fit = lm(Y~X1+X2+X3+X4+X5+X6,data=P128)
summary(P128.fit)
plot(P128.fit)

durbinwatsonTest(P128.fit)
library(corrplot)
P128.cor = cor(P128[c("Y","X1","X2","X3","X4","X5","X6")])
corrplot(P128.cor,method="number",diag=TRUE,type="full")

#4.12.(b)
Fitted = as.vector(fitted(P128.fit))
Residual = as.vector(residuals(P128.fit))
Rstandard = as.vector(rstandard(P128.fit))
Rstudent = as.vector(rstudent(P128.fit))
P128.pred = data.frame(P128,Fitted,Residual,Rstandard,Rstudent)
P128.pred

Rstandard

Cooks.distance = as.vector(cooks.distance(P128.fit))
Cooks.distance

Dffits = as.vector(dffits(P128.fit))
Dffits

X <- as.matrix(cbind(c(rep(1,40)),P128.fit$model[,2:7]))
p <- diag(X%*%solve(t(X)%*%X)%*%t(X))
d <- P128.fit$residuals/sqrt(sum(P128.fit$residuals^2))
po = p/(1-p)
re = ((6+1)/(1-p))*(d**2/(1-d**2))
H  = po + re
H

#4.12.(c)
plot(Rstandard,pch=19,cex=1) 
plot(Cooks.distance,pch=19,cex=1) 
plot(Dffits,pch=19,cex=1) 
plot(H,pch=19,cex=1) 
plot(re,po,pch=19,cex=1)
text(re,po,labels=rownames(P128),pos=4,col="red",cex=1,font=1)

