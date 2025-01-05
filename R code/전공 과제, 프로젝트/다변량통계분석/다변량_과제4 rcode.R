#Q1

library(psych)


cor = matrix(c(1.00,-0.04,0.61,0.45,0.03,-0.29,-0.3,0.45,0.3,
               -0.04,1.00,-0.07,-0.12,0.49,0.43,0.3,-0.31,-0.17,
               0.61,-0.07,1.00,0.59,0.03,-0.13,-0.24,0.59,0.32,
               0.45,-0.12,0.59,1.00,-0.08,-0.21,-0.19,0.63,0.37,
               0.03,0.49,0.03,-0.08,1.00,0.47,0.41,-0.14,-0.24,
               -0.29,0.43,-0.13,-0.21,0.47,1.00,0.63,-0.13,-0.15,
               -0.3,0.3,-0.24,-0.19,0.41,0.63,1.00,-0.26,-0.29,
               0.45,-0.31,0.59,0.63,-0.14,-0.13,-0.26,1.00,0.4,
               0.3,-0.17,0.32,0.37,-0.24,-0.15,-0.29,0.4,1.00),
             nrow = 9, ncol = 9,
             dimnames = list(c("x1", "x2", "x3", "x4", "x5", "x6",'x7','x8','x9'),
                             c("x1", "x2", "x3", "x4", "x5", "x6",'x7','x8','x9')))

# pca for choosing the number of factors in the principal factor method.
pain.pca=princomp(covmat=cor,cor=T)
summary(pain.pca)
pain.pca$sdev^2
screeplot(pain.pca, npcs=9,type="l") #choose 3 factors

# Conduct factor analysis using the principal factor method without rotations.
fac.pf1 = fa(cor, nfactors=3, fm="pa", rotate="none")
fac.pf1
fac.pf1$Phi

#residual matrix
fac.pf1$residual

# Conduct factor analysis using the principal factor method with the Varimax rotation.
fac.pf2 = fa(cor, nfactors=3, fm="pa", rotate="varimax")
fac.pf2

#residual matrix
fac.pf2$residual



#Q2
#(a)
# 주어진 요인 적재 값
F1 <- c(0.789, 0.834, 0.74, 0.586, 0.676, 0.654, 0.641, 0.629, 0.564, 0.808)
F2 <- c(-0.403, -0.234, -0.134, -0.185, -0.248, 0.44, 0.534, 0.651, 0.354, 0.714)

# 요인 적재 행렬 생성
loadings=cbind(F1, F2)

# 회전 함수 정의
rotate_matrix=function(loadings, angle) {
  rotation_matrix=matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2)
  return(loadings %*% rotation_matrix)
}

# 음수 값을 최소화하는 각도 찾기
angles=seq(0, pi, length.out = 360)
neg_counts=sapply(angles, function(angle) {
  rotated=rotate_matrix(loadings, angle)
  return(sum(rotated < 0))
})

# 최소 음수 값을 갖는 각도
min_neg_angle=angles[which.min(neg_counts)]
min_neg_angle

# 음수 값의 수 시각화
plot(angles, neg_counts, type = "l", xlab = "Angle (radians)", ylab = "Number of negative loadings",
     main = "Number of negative loadings vs. Rotation angle")
abline(v = min_neg_angle, col = "red", lty = 2)


#(b)
# Combine into a loading matrix
loadings=matrix(c(F1, F2), nrow = 10, ncol = 2, byrow = FALSE)
rownames(loadings)=paste0("x", 1:10)
colnames(loadings)=c("F1", "F2")

# Perform varimax rotation
rotated=varimax(loadings)
rotated_loadings=rotated$loadings

# Print the rotated loadings
rotated_loadings

# Group variables based on the rotated loadings
factor1=rownames(rotated_loadings)[apply(rotated_loadings, 1, which.max) == 1]
factor2=rownames(rotated_loadings)[apply(rotated_loadings, 1, which.max) == 2]

factor1
factor2


#Q3
sales=read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\다변량통계분석\\sales.dat",header=T)

#(a)
f1=fa(sales, nfactors = 2, fm = "ml", rotate = "varimax")
f1

#(b)
f2=fa(sales, nfactors = 3, fm = "ml", rotate = "varimax")
f2

#(c)
cor1=cor(sales)
pca=princomp(covmat=cor1,cor=T)
summary(pca)
pca$sdev^2
screeplot(pca, npcs=7,type="l") #choose 2 factors

#(d)
f1$scores

#(e)
factor1_vars=rownames(f1$loadings)[apply(f1$loadings, 1, function(x) which.max(abs(x)) == 1)]
factor2_vars=rownames(f1$loadings)[apply(f1$loadings, 1, function(x) which.max(abs(x)) == 2)]
factor1_vars
factor2_vars

factor1_scores=rowSums(sales[, factor1_vars])
factor2_scores=rowSums(sales[, factor2_vars])

factor_scores=data.frame(Factor1 = factor1_scores, Factor2 = factor2_scores)
factor_scores

