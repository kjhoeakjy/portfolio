#9.1
eigenvalue=c(4.603,1.175,0.203,0.015,0.003,0.001)

sqrt(4.603/0.001)
sqrt(4.603/0.003)
sqrt(4.603/0.015)



#9.3
#(a)
P256=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P256.txt",header=T)
head(P256)

P256.fit = lm(Y~X_1+X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_.10.+X_.11.,data=P256)
summary(P256.fit)

pairs(P256)
cor=cor(P256[,-1])
cor

#(b)
library(olsrr)
ols_vif_tol(P256.fit)
ols_eigen_cindex(P256.fit)

lambda=eigen(cor)
lambda

CI <- sort(sqrt(max(lambda$values)/(lambda$values)))
CI
#다중공선성 존재


#10.4
P290=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P290.txt",header=T)
head(P290)

#(a)
P290_std = as.data.frame(scale(P290))
fit2=lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data=P290_std)
summary(fit2)

#(b)
Sy=sd(P290$Y)
S1=sd(P290$X1)
S2=sd(P290$X2)
S3=sd(P290$X3)
S4=sd(P290$X4)
S5=sd(P290$X5)
S6=sd(P290$X6)
b1=Sy*0.004628/S1
b2=Sy*(-1.014)/S2
b3=Sy*(-0.5375)/S3
b4=Sy*(-0.2047)/S4
b5=Sy*(-0.1012)/S5
b6=Sy*2.48/S6
mean(P290$Y)-mean(P290$X1)*b1-mean(P290$X2)*b2-mean(P290$X3)*b3-mean(P290$X4)*b4-mean(P290$X5)*b5-mean(P290$X6)*b6
b1
b2
b3
b4
b5
b6


#(c)
fit1=lm(Y~X1+X2+X3+X4+X5+X6, data=P290)
summary(fit1)

#(d)
cor1=cor(P290_std[,-1])
plot(P290_std[,-1])

#(e)
#11.5
P290.PC=prcomp(P290_std[,-1], center=TRUE, scale.=TRUE)
summary(P290.PC)
P290.PC

lambda1=eigen(cor1)
lambda1

CI <- sort(sqrt(max(lambda1$values)/(lambda1$values)))
CI


#(f)
#3개 pc 선택
P290.PC$rotation
P290.PC.DAT=data.frame(cbind(P290_std$Y,P290.PC$x))
fit2=lm(scale(V1)~PC1+PC2+PC3,P290.PC.DAT)
summary(fit2)

pc_coefficients=coef(summary(fit2))[-1, "Estimate"]
rotation_matrix=P290.PC$rotation[, 1:3]

standardized_coefficients=rotation_matrix %*% pc_coefficients
standardized_coefficients

original_sds=apply(P290[,-1], 2, sd)
original_coefficients=standardized_coefficients*Sy / original_sds
original_coefficients
mean(P290$Y)-mean(P290$X1)*original_coefficients[1,1]-mean(P290$X2)*original_coefficients[2,1]-mean(P290$X3)*original_coefficients[3,1]-mean(P290$X4)*original_coefficients[4,1]-mean(P290$X5)*original_coefficients[5,1]-mean(P290$X6)*original_coefficients[6,1]

#(g)
library(MASS)

P290.ridge.fit = lm.ridge(Y ~ X1 + X2 + X3 + X4 + X5 + X6 -1, P290_std, lambda = seq(0, 1, 1e-3))
coef(P290.ridge.fit)

par(mar = c(4, 4, 4, 4), las = 2)
matplot(P290.ridge.fit$lambda, coef(P290.ridge.fit), main="Ridge Trace", type = "l", ylim = c(-0.5, 1.5),
        xlab = "k", ylab = expression(hat(theta)))
legend(0.8,1.5,c("Theta_1" , "Theta_2", "Theta_3","Theta_4" , "Theta_5", "Theta_6"),lty=c(1,2,3),col=c("black","red","blue","green","lightblue","pink"),cex=1)

fit3 = lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,P290)
summary(fit3)

fit4 = lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6,P290_std)
summary(fit4)


X <- as.matrix(P290_std[,2:7])
Y  <- as.matrix(P290_std[,1])

D <- diag(1/sqrt(diag(t(X)%*%X)))
Z <- X%*%D

theta.ridge.0.1 <- D%*%solve(t(Z)%*%Z+0.1*diag(rep(1,6)))%*%t(Z)%*%Y 
theta.ridge.0.1 

s <- sqrt(apply(P290,2,var))[1:7]
beta.ridge=(s[1]/s[2:7])*theta.ridge.0.1
s

m <- apply(P290,2,mean)[1:7]
beta.ridge.0 <- m[1] - sum(m[2:7]*beta.ridge)
m


#11.5
#(a)
P329=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\회귀분석\\P329.txt",header=T)
head(P329)
fit1=lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6+X7+X8+X9, data=P329)
summary(fit1)

library(olsrr)
ols_vif_tol(fit1)
cor3=cor(P329[,-10])
cor3
lambda=eigen(cor3)
lambda
CI <- sort(sqrt(max(lambda$values)/(lambda$values)))
CI
#다중공선성 문제 없음

# Forward selection.
ols_step_forward_p(fit1)

# Backward elimination.
ols_step_backward_p(fit1)


#(b)
fit10=lm(Y ~ X1 + X9, data=P329)
summary(fit10)


#(c)
fit11=lm(Y ~ X1, data=P329)
summary(fit11)

#11.6
P256.fit = lm(Y~X_1+X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_.10.+X_.11.,data=P256)
summary(P256.fit)

ols_vif_tol(P256.fit)
cor4=cor(P256[,-1])
cor4
lambda=eigen(cor4)
lambda
CI <- sort(sqrt(max(lambda$values)/(lambda$values)))
CI

# Forward selection.
ols_step_forward_p(P256.fit)

# Backward elimination.
ols_step_backward_p(P256.fit)


#(b)
fit.1 = lm(Y~X_1,data=P256)
summary(fit.1)
fit.2 = lm(Y~X_.10.,data=P256)
summary(fit.2)
fit.3 = lm(Y~X_1+X_.10.,data=P256)
summary(fit.3)
fit.4 = lm(Y~X_2+X_.10.,data=P256)
summary(fit.4)
fit.5 = lm(Y~X_8+X_.10.,data=P256)
summary(fit.5)
fit.6 = lm(Y~X_5+X_8+X_.10.,data=P256)
summary(fit.6)

AIC(fit.1)
AIC(fit.2)
AIC(fit.3)
AIC(fit.4)
AIC(fit.5)
AIC(fit.6)
#(c)
pairs(P256[,c(1,2,3,9,11)])

#(d)
P256$W=100/P256$Y
head(P256)
pairs(P256[,c(2,3,9,11,13)])

#(e)
fit.1 = lm(W~X_1,data=P256)
summary(fit.1)
fit.2 = lm(W~X_.10.,data=P256)
summary(fit.2)
fit.3 = lm(W~X_1+X_.10.,data=P256)
summary(fit.3)
fit.4 = lm(W~X_2+X_.10.,data=P256)
summary(fit.4)
fit.5 = lm(W~X_8+X_.10.,data=P256)
summary(fit.5)
fit.6 = lm(W~X_5+X_8+X_.10.,data=P256)
summary(fit.6)

P256.fit.1 = lm(W~X_1+X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_.10.+X_.11.,data=P256)
summary(P256.fit.1)

# Forward selection.
ols_step_forward_p(P256.fit.1)

# Backward elimination.
ols_step_backward_p(P256.fit.1)

AIC(fit.1)
AIC(fit.2)
AIC(fit.3)
AIC(fit.4)
AIC(fit.5)
AIC(fit.6)

#(f)
P256$X_.13.=P256$X_8/P256$X_.10.
head(P256)
fit.7=lm(Y~X_.13.,data=P256)
summary(fit.7)

#(g)
P256.fit.2 = lm(W~X_1+X_2+X_3+X_5+X_6+X_7+X_9+X_.10.+X_.11.+X_.13.,data=P256)
summary(P256.fit.2)

# Forward selection.
ols_step_forward_p(P256.fit.2)

# Backward elimination.
ols_step_backward_p(P256.fit.2)

P256.fit.3 = lm(Y~X_2+X_5+X_7+X_9+X_.11.+X_.13.,data=P256)

ols_vif_tol(P256.fit.3)
cor4=cor(P256[,c(11,12,14)])
cor4
lambda=eigen(cor4)
lambda
CI <- sort(sqrt(max(lambda$values)/(lambda$values)))
CI
