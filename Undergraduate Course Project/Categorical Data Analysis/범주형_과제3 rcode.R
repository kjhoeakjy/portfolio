Gators = read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\범주형자료분석\\Alligators2.dat",header = TRUE)
head(Gators, 5)
library(VGAM)
Gators$lake=factor(Gators$lake, levels = c(4, 1, 2, 3))
fit1=vglm(cbind(y2,y3,y4,y5,y1)~lake+size, family = multinomial, data = Gators)
coef(fit1, matrix = TRUE)
summary(fit1)

fitted(fit1)
