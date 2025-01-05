#Q2
mean=matrix(c(95.5,164.4,55.7,93.4,18.0,31.1),ncol=1)
covmat=matrix(c(3266,1344,732,1176,163,238,1344,722,324,537,80,118,732,324,179,281,39,57,1176,537,281,475,64,95,163,80,39,64,10,14,238,118,57,95,14,21),ncol=6,nrow=6)
mean
covmat

#(a)
bear.pca.1 = princomp(covmat=covmat,cor=F)
bear.pca.1$loadings
bear.pca.1$sdev^2
screeplot(bear.pca.1, npcs=6,type="l")

#(b)
bear.pca.2 = princomp(covmat=covmat,cor=T)
bear.pca.2$loadings
bear.pca.2$sdev^2
screeplot(bear.pca.2, npcs=6,type="l")


#Q3
radio= read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\다변량통계분석\\radiotherapy.dat",header=T)
attach(radio)
radiocov=cov(radio)
radio.pca = princomp(covmat=radiocov,cor=T)
radio.pca$loadings
radio.pca$sdev^2
screeplot(radio.pca, npcs=6,type="l")
