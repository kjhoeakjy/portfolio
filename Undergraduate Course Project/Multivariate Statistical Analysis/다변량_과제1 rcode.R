#1-(e)
n=50
p=3
mu=matrix(c(-1,2,-5),nrow=3,ncol=1)
vmat=matrix(c(9,4,1,4,16,-1,1,-1,4),nrow=3,ncol=3)
library(MASS)
mvdata=mvrnorm(n,mu,vmat)
mvdata
colMeans(mvdata)
cov(mvdata)

#1-(f)
par(mfrow=c(3,2))
for(i in 1:p){qqnorm(mvdata[,i],xlab="Quantiles of Standard Normal",ylab=paste("Var",i))}
mvcov = cov(mvdata)
dis = mahalanobis(mvdata, colMeans(mvdata), mvcov)
qqmath(dis,distribution = function(p) qchisq(p,df=3))



#2
paper= read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\다변량통계분석\\Paper.dat", col.names = c("Density", "machine_strength", "cross_strength"),header=T)

#2-(a)
hist(paper$Density)
stem(paper$Density)
boxplot(paper$Density)
#
hist(paper$machine_strength)
stem(paper$machine_strength)
boxplot(paper$machine_strength)
#
hist(paper$cross_strength)
stem(paper$cross_strength)
boxplot(paper$cross_strength)


#2-(b)
pairs(paper)

#2-(c)
library(rgl)
plot3d(paper$Density,paper$machine_strength,paper$cross_strength) 
identify3d(paper$Density,paper$machine_strength,paper$cross_strength)

#2-(d)
colmean=colMeans(paper)
matrix(colmean)
cov=cov(paper)
matrix(cov,nrow=3,ncol=3)

#2-(e)
p=3
for(i in 1:p){qqnorm(paper[,i],xlab="Quantiles of Standard Normal",ylab=paste("Var",i))}

#2-(f)
distance = mahalanobis(paper, colMeans(paper), cov)
distance

#2-(g)
library(lattice) 
qqmath(distance,distribution = function(p) qchisq(p,df=3))
