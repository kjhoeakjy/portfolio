cereal = read.table("C:\\Users\\kjhoe\\OneDrive\\바탕화~1-LAPTOP-VGOF9FJ7-25904429\\다변량통계분석\\cereal_r.dat",header=T)
head(cereal)

par(mfrow=c(1,1))

# Single Linkage
plot(hclust(dist(cereal[,c(-1,-2)]),method="single"),labels=cereal[,1],xlab=NA,ylab="Distance",main="Single linkage dendrogram")

# Complete Linkage
plot(hclust(dist(cereal[,c(-1,-2)]),method="complete"),labels=cereal[,1],xlab=NA,ylab="Distance",main="Complete linkage dendrogram")

# Group Average
plot(hclust(dist(cereal[,c(-1,-2)]),method="average"),labels=cereal[,1],xlab=NA,ylab="Distance",main="Group Average dendrogram")


# K-means method (k=2)
k1=kmeans(cereal[,c(-1,-2)],2)
k2=kmeans(cereal[,c(-1,-2)],3)
k1$cluster
k2$cluster

pca=princomp(cereal[,c(-1,-2)],cor=T)
plot(pca$scores[,1], pca$scores[,2], xlab="PC1",ylab="PC2",type="n",lwd=2)
text(pca$scores[,1], pca$scores[,2],labels=k1$cluster,col="blue",cex=.8,lwd=2)

pca=princomp(cereal[,c(-1,-2)],cor=T)
plot(pca$scores[,1], pca$scores[,2], xlab="PC1",ylab="PC2",type="n",lwd=2)
text(pca$scores[,1], pca$scores[,2],labels=k2$cluster,col="blue",cex=.8,lwd=2)

