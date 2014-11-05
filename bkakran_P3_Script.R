library(kernlab)
GetKMeansData = function(radius,numberOfPoints,x,y){
coordinates <- matrix(nrow=numberOfPoints,ncol = 2)
for (i in 1:numberOfPoints) {
angle <- runif(1)*2*pi
coordinates[i,] <- c(x-runif(1)+sin(angle)*radius, y-runif(1)+cos(angle)*radius)
}
return(coordinates)
}


KmeansCluster1 = GetKMeansData(2,200,2,3)
KmeansClusterlabel1 =matrix(1,nrow=200,ncol=1)
KmeansCluster1 = cbind(KmeansCluster1,KmeansClusterlabel1)
KmeansCluster2 = GetKMeansData(4,200,2,3)
KmeansClusterlabel2 =matrix(2,nrow=200,ncol=1)
KmeansCluster2 = cbind(KmeansCluster2,KmeansClusterlabel2)
kmeansPoints = rbind(KmeansCluster1,KmeansCluster2)
plot(kmeansPoints[,-3], xlab="x",ylab="y", main="Data For K-means",col=kmeansPoints[,3],pch=16)

getPCAPoints = function(radius,numberOfPoints,x,y){
coordinates <- matrix(nrow=numberOfPoints,ncol = 2)
for (i in 1:numberOfPoints) {
angle <- runif(1)*2*pi
coordinates[i,] <- c(x+sin(angle)*radius, y+cos(angle)*radius)
}
return(coordinates)
}

PCACluster1 = getPCAPoints(2,200,1,1)
PCAClusterlabel1 =matrix(1,nrow=200,ncol=1)
PCACluster1 = cbind(PCACluster1,PCAClusterlabel1)
PCACluster2 = getPCAPoints(4,200,1,1)
PCAClusterlabel2 =matrix(2,nrow=200,ncol=1)
PCACluster2 = cbind(PCACluster2,PCAClusterlabel2)
PCAPoints = rbind(PCACluster1,PCACluster2)
plot(PCAPoints[,-3], xlab="x",ylab="y", main="Data For PCA",col=PCAPoints[,3],pch=19)


getSVMPoints = function(radius,numberOfPoints,x,y){

coordinates <- matrix(nrow=numberOfPoints,ncol = 2)
for (i in 1:numberOfPoints) {
angle <- runif(1)*pi
coordinates[i,] <- c(x+sin(angle)*radius, y+cos(angle)*radius)
}
return(coordinates)
}



SVMCluster1 = getSVMPoints(2,200,0,0)
SVMClusterlabel1 =matrix(1,nrow=200,ncol=1)
SVMCluster1 = cbind(SVMCluster1,SVMClusterlabel1)
SVMCluster2 = getSVMPoints(4,200,0,0)
SVMClusterlabel2 =matrix(2,nrow=200,ncol=1)
SVMCluster2 = cbind(SVMCluster2,SVMClusterlabel2)
SVMPoints = rbind(SVMCluster1,SVMCluster2)
plot(SVMPoints[,-3], xlab="x",ylab="y", main="Data For SVM",col=SVMPoints[,3],pch=19)

getPerformance <- function(groundLabel, classLablel) {
TP=0.0
FN=0.0
FP=0.0
TN=0.0
groundLabelMat = as.matrix(groundLabel)
classLablelMat = as.matrix(classLablel)
for (i in 1:nrow(groundLabelMat)) {
	for (j in 1:nrow(classLablelMat)) {
			if(i == j) {
		    ##do nothin
		    } else if((groundLabelMat[i]==groundLabelMat[j]) && (classLablelMat[i]==classLablelMat[j])){
		   		TP = TP +1
		    } else if((groundLabelMat[i]!=groundLabelMat[j]) && (classLablelMat[i]!=classLablelMat[j])) {
		    	TN = TN +1
		    } else if((groundLabelMat[i]==groundLabelMat[j]) && (classLablelMat[i]!=classLablelMat[j])) {
		    	FN =FN +1
		    } else if((groundLabelMat[i]!=groundLabelMat[j]) && (classLablelMat[i]==classLablelMat[j])) {
		    	FP =FP +1
		    }
		}
	}
   TP = TP/2
   TN = TN/2
   FN = FN/2
   FP = FP/2
   accuracy = (TP + TN) / (TP + TN + FP + FN)
   precision = TP/(TP + FP)
   recall = TP/(TP + FN)
   fscore = 2*precision*recall/(precision + recall)
   return(list(accuracy=accuracy,precision=precision,recall=recall,fscore=fscore))
}

##--------------------------------------------------------------------------------------------------------------------------------------------------------
kmeansOut <- kmeans(kmeansPoints,2)
kemansCluster1 = kmeansPoints[which(kmeansOut$cluster==1),]
kemansCluster2 = kmeansPoints[which(kmeansOut$cluster==2),]

x1 = min(kmeansPoints[,1]) - 1
x2 = max(kmeansPoints[,1]) + 1
y1 = min(kmeansPoints[,2]) - 1
y2 = max(kmeansPoints[,2]) + 1

plot(-100,-100, col=c(1), main = "Cluster Distributions for k-means clustering", xlim=c(x1,x2), ylim=c(y1,y2), pch=c(19),xlab="x",ylab="y")
points(kemansCluster1, col=2, pch=2)
points(kemansCluster2, col=3, pch=3)
kemansperf = getPerformance(kmeansPoints[,3],kmeansOut$cluster)
##--------------------------------------------------------------------------------------------------------------------------------------------------------

pca = princomp (PCAPoints[,-3], center=TRUE);
pca
plot (pca); # screeplot
pca_loadings=loadings(pca);      # matrix of eigenvectors
pca_summary=summary (pca); # check proportion of variance
P=pca$scores;     # projection of X onto eigenvectors
proj_data_c1 <- P[, 1];
proj_data_c2 <- P[, 2];
plot(P)
plot (proj_data_c1, xlab="Point Index",ylab="Projection on Component 1");
plot (proj_data_c2, xlab="Point Index",ylab="Projection on Component 2");
barplot(proj_data_c1,main="BarPlot of projection on Component1")
barplot(proj_data_c2,main="BarPlot of projection on Component2")
hist(proj_data_c1,main="Histogram of points on Component1")
hist(proj_data_c2,main="Histogram of points on Component2")
##--------------------------------------------------------------------------------------------------------------------------------------------------------
PCAPointsTest = PCAPoints[,3]
PCAPointsTest = as.matrix(PCAPointsTest)
PCAPointsTest[which(PCAPointsTest[,1]==2),]=0
model <- glm(PCAPointsTest ~ proj_data_c1, family=binomial)
##--------------------------------------------------------------------------------------------------------------------------------------------------------

library("e1071")
dat=data.frame(x=SVMPoints[,-3], y=as.factor(SVMPoints[,3]))
svmfit=svm(y~., data=dat , kernel ="linear", cost=10,scale=FALSE)
plot(svmfit,dat)
svmclusterLabel = as.matrix(svmfit$fitted)
storage.mode(svmclusterLabel) <- "integer" 
svmperf = getPerformance(SVMPoints[,3],svmclusterLabel)
plot(SVMPoints[,-3], xlab="x",ylab="y", main="Data For SVM after Clustering",col=svmclusterLabel,pch=19)
##-------------------------------------------------------------------------------------------------------------------------------------------------------

dat=kmeansPoints[,-3]
kkmeansRbf = kkmeans(dat,2,kernel="rbfdot")
kkmeansRbf = specc(dat,2,kernel="rbfdot")
kkmeansLabel=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
kemansperf = getPerformance(kmeansPoints[,3],kkmeansLabel)
plot(dat, col=kkmeansRbf)
plot(dat, col=kkmeansRbf,xlab="X axis",ylab="y axis",main="Clustering after Radial Basis kernel function Gaussian")



dat=kmeansPoints[,-3]
kkmeansPoly = kkmeans(dat,2,kernel="polydot")
kkmeansLabe=c(2,2,2,2,2,1,1,1,2,1,1,1,1,2,1,1,2,1,2,1,2,1,1,2,1,2,2,2,1,2,2,1,1,2,2,2,1,1,2,1,2,1,2,1,2,2,2,2,2,1,2,2,2,2,2,2,1,2,1,1,1,2,2,2,1,1,2,1,2,2,1,2,2,2,1,2,1,1,2,1,1,2,2,1,2,2,2,2,1,1,2,1,1,2,2,2,2,1,1,2,1,2,1,2,1,2,2,2,1,1,1,1,2,2,2,2,1,2,2,2,1,1,1,1,2,2,2,2,1,1,1,2,2,1,2,2,2,1,1,2,2,2,1,1,2,1,1,1,2,1,2,2,1,1,1,2,1,1,2,2,1,1,1,1,2,2,1,2,1,2,1,1,2,1,1,1,1,1,2,1,2,1,2,2,1,2,2,1,1,1,1,1,1,2,2,2,2,2,1,1,1,1,1,1,1,2,2,2,2,1,2,2,1,1,2,2,1,2,1,1,1,1,2,1,1,2,2,1,2,1,1,1,2,1,1,2,1,2,2,1,1,2,2,1,2,2,2,1,2,1,2,1,1,2,1,1,1,1,1,1,1,1,1,2,1,2,2,2,1,2,1,1,2,2,2,2,1,1,1,2,1,2,1,2,2,1,1,2,1,2,1,2,2,2,1,1,1,1,1,1,1,1,1,2,2,2,1,1,1,1,1,1,1,2,1,2,1,1,1,1,2,2,2,2,2,1,1,2,2,1,2,2,2,1,2,2,1,2,1,1,1,2,2,2,1,1,1,2,1,2,1,1,2,1,1,1,2,2,2,1,2,1,2,1,1,2,1,1,2,2,2,1,1,2,1,1,1,2,2,2,1,2,2,2,1,2,1,1,1,2,2,1,1,2,1,1,2,1,2,1)
kemansperf = getPerformance(kmeansPoints[,3],kkmeansLabel)
plot(dat, col=kkmeansPoly,xlab="X axis",ylab="y axis",main="Clustering after Polynomial kernel function")

##-------------------------------------------------------------------------------------------------------------------------------------------------------

pca_kernel <- kpca(PCAPoints[,-3], kernel="rbfdot",kpar=list(sigma=1),features=2)
pca_loadings_kernel <- pcv(pca_kernel);
pca_eigne_vales <- eig(pca_kernel)
projected_data <- rotated(pca_kernel);
plot (projected_data[,1], xlab="Point Index",ylab="Projection on Component 1",main="Projection on Component 1 using Radial Basis kernel function");
plot (projected_data[,2], xlab="Point Index",ylab="Projection on Component 2",main="Projection on Component 2 using Radial Basis kernel function");
pca_variance = pca_eigne_vales/sum(pca_eigne_vales)
plot(rotated(pca_kernel),col=as.integer(PCAPoints[,3]),xlab="1st Principal Component",ylab="2nd Principal Component",main="PCA on guassianl kernel function")
hist(projected_data[,1],main="Histogram of points on Component1")
hist(projected_data[,2],main="Histogram of points on Component2")


pca_kernel <- kpca(PCAPoints[,-3], kernel="polydot",kpar=list(degree=2,scale=1,offset=0),features=2)
pca_loadings_kernel <- pcv(pca_kernel);
pca_eigne_vales <- eig(pca_kernel)
projected_data <- rotated(pca_kernel);
plot (projected_data[,1], xlab="Point Index",ylab="Projection on Component 1",main="Projection on Component 1 using Polynomial kernel function");
plot (projected_data[,2], xlab="Point Index",ylab="Projection on Component 2",main="Projection on Component 2 using Polynomial kernel function");
pca_variance = pca_eigne_vales/sum(pca_eigne_vales)
plot(rotated(pca_kernel),col=as.integer(PCAPoints[,3]),xlab="1st Principal Component",ylab="2nd Principal Component",main="PCA on Polynomial kernel function")
hist(projected_data[,1],main="Histogram of points on Component1")
hist(projected_data[,2],main="Histogram of points on Component2")


##-------------------------------------------------------------------------------------------------------------------------------------------------------



library("e1071")
dat=data.frame(x=SVMPoints[,-3], y=as.factor(SVMPoints[,3]))
svmfit=svm(y~., data=dat , kernel ="radial", cost=10,scale=FALSE)
plot(svmfit,dat,main="SVM classification plot after applying radial kernel")
svmclusterLabel = as.matrix(svmfit$fitted)
storage.mode(svmclusterLabel) <- "integer" 
svmperf = getPerformance(SVMPoints[,3],svmclusterLabel)
plot(SVMPoints[,-3], xlab="x",ylab="y", main="SVM classification plot after applying radial kernel",col=svmclusterLabel,pch=19)



library("e1071")
dat=data.frame(x=SVMPoints[,-3], y=as.factor(SVMPoints[,3]))
svmfit=svm(y~., data=dat , kernel ="polynomial", cost=10,scale=FALSE)
plot(svmfit,dat,main="SVM classification plot after applying radial kernel")
svmclusterLabel = as.matrix(svmfit$fitted)
storage.mode(svmclusterLabel) <- "integer" 
svmperf = getPerformance(SVMPoints[,3],svmclusterLabel)
plot(SVMPoints[,-3], xlab="x",ylab="y", main="SVM classification plot after applying polynomial kernel",col=svmclusterLabel,pch=19)

##-------------------------------------------------------------------------------------------------------------------------------------------------------

##The logic of n spehere is used to derive n dimensional data.For a n spehere with centre as origin points will be such that
##	x1^2+x2^2+x3^2+x4^2+x5^2+......+xn^2 = r^2 where r is the radius.
##The idea used here is to first generate set of random points of n dimension and then convert them to points in a unit radius
## n spehere and then scale it by multiplying with the radius.
GetKNDimMeansData = function(radius,numberOfPoints,numberOfDims){
library("mnormt")
points = matrix(0,nrow=numberOfPoints,ncol=numberOfDims)
mean=rep(0,numberOfDims)
Sigma = diag(length(mean))	
#Generate n dimenisonal points
randomPoints = rmnorm(n = numberOfPoints, mean = rep(0, nrow(Sigma)), Sigma);
randomPointsTemp = randomPoints^2
#Get the square root of sume of sqaures
randomPointsRowSum = sqrt(rowSums(randomPointsTemp))
randomPointsRowSum = as.matrix(randomPointsRowSum);
#Convert to nspehere of unit radius and multiply by radius to scale it up
for (i in 1:numberOfPoints) {
	points[i,] = radius*(randomPoints[i,]/randomPointsRowSum[i])
}
return(points)
}
	
	
#Example
library(rgl)
open3d()
plot3d(GetKNDimMeansData(2,100,10),col=5,type="p", radius=5)


##-------------------------------------------------------------------------------------------------------------------------------------------------------
#generate 200 points of  n-spehere of of 50 dimension and radius 2
NdimKmeansCluster1 = GetKNDimMeansData(2,200,50)
NdimKmeansClusterlabel1 =matrix(1,nrow=200,ncol=1)
NdimKmeansCluster1 = cbind(NdimKmeansCluster1,NdimKmeansClusterlabel1)
#generate 200 points of n-spehere of 50 dimension and radius 10
NdimKmeansCluster2 = GetKNDimMeansData(10,200,50)
NdimKmeansClusterlabel2 =matrix(2,nrow=200,ncol=1)
NdimKmeansCluster2 = cbind(NdimKmeansCluster2,NdimKmeansClusterlabel2)
NdimkmeansPoints = rbind(NdimKmeansCluster1,NdimKmeansCluster2)
NdimkmeansOut <- kmeans(NdimkmeansPoints[,-51],2)
kemansperf = getPerformance(NdimkmeansPoints[,51],NdimkmeansOut$cluster)


##-------------------------------------------------------------------------------------------------------------------------------------------------------


pca = princomp (NdimkmeansPoints[,-51], center=TRUE);
pca
plot (pca); # screeplot
pca_loadings=loadings(pca);      # matrix of eigenvectors
pca_summary=summary (pca); # check proportion of variance
P=pca$scores;     # projection of X onto eigenvectors

##-------------------------------------------------------------------------------------------------------------------------------------------------------

pca_kernel <- kpca(NdimkmeansPoints[,-51], kernel="polydot",kpar=list(degree=2,scale=1,offset=0))
pca_loadings_kernel <- pcv(pca_kernel);
pca_eigne_vales <- eig(pca_kernel)
projected_data <- rotated(pca_kernel);
pca_variance = pca_eigne_vales/sum(pca_eigne_vales)
pca_variance_cum<-cumsum(pca_variance)
plot(pca_variance_cum)
variablity = sum(pca_eigne_vales[1:25])/sum(pca_eigne_vales)

for (i in 1:25) {
title = paste("Projection onto component", i, sep = "")
plot(projected_data[,i],main=title)
}

NdimkmeansOut <- kmeans(projected_data[,1:25],2)
kemansperf = getPerformance(NdimkmeansPoints[,51],NdimkmeansOut$cluster)
pca_kernel <- kpca(NdimkmeansPoints[,-51], kernel="rbfdot",kpar=list(sigma=1),features=2)

##-------------------------------------------------------------------------------------------------------------------------------------------------------
dat=NdimkmeansPoints[,-51]
kkmeansPoly = kkmeans(dat,2,kernel="rbfdot")
kkmeansPoly = specc(dat,2,kernel="rbfdot")
kkmeansLabel=c(1,2,2,2,2,1,2,1,2,1,1,1,2,1,2,1,2,1,1,2,1,1,1,2,1,2,2,2,1,1,1,2,2,1,2,2,1,1,1,2,1,2,1,1,2,1,1,1,2,1,2,2,2,1,1,2,2,1,1,1,1,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,1,1,2,1,1,1,1,1,2,1,1,2,2,2,2,1,2,2,1,1,2,2,2,2,1,2,1,1,1,1,2,2,1,1,2,1,2,2,1,2,1,1,1,1,2,1,2,1,2,2,1,2,1,1,1,1,1,2,1,1,2,2,1,2,2,1,2,1,1,2,2,1,2,1,2,2,2,2,1,2,1,2,1,1,2,2,2,1,1,1,1,2,2,1,2,2,1,2,2,2,2,2,1,2,1,2,1,2,2,2,2,1,1,2,2,1,1,2,1,2,2,2,1,2,2,1,1,1,1,1,1,2,1,1,1,2,1,2,1,2,1,1,1,2,1,2,1,1,1,1,1,2,1,1,2,2,1,2,2,2,1,1,2,2,2,1,2,1,2,1,1,1,1,1,1,1,2,1,1,2,2,1,1,2,1,1,2,1,2,2,2,2,1,1,2,1,1,1,2,1,1,2,2,2,2,1,2,2,1,2,1,2,2,1,1,2,2,2,2,2,2,1,1,1,2,1,1,2,1,1,1,2,1,2,1,1,1,2,1,1,2,1,2,1,1,2,2,1,2,1,1,1,2,2,1,2,1,1,2,1,2,1,1,2,2,2,1,1,1,1,1,2,1,1,2,1,1,2,2,2,2,1,2,2,2,2,2,1,2,2,1,1,2,1,1,2,1,2,2,1,1,2,2,2,2,1,2,1,1,1,2,1,1,1,2,1,2,2,2,2,2,2,1,1)
kemansperf = getPerformance(NdimkmeansPoints[,51],kkmeansLabel)
#----------------------------------------------------------------------------------------------------------------------------