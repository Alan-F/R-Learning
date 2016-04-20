setwd("E:\\Multivariate Statistics WEEK 8")
load("food.rda")

food.pca=prcomp(x=food)
summary(food.pca)
food.pca$sdev

food.stand=apply(food,MARGIN = 2,FUN = scale,center=T,scale=T)
food.stand.pca=prcomp(x=food.stand)
summary(food.stand.pca)
food.stand.pca$sdev
############################

load("sad.rda")
library(EBImage)
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

display(sad)
sad.mean=apply(sad,2,scale,scale=F)
display(sad.mean)
sad.1=sad-sad.mean
display(sad.1)

source("rrr.R")
sad.pca=rrr(x=sad,y=sad,k=0.000000001,type="PCA",rrr.plot=F,only.C=T ) $? 
#f=function(x){sad.mean+sad.l%*%x}
plot.sad=lapply(sad.pca,function(x){sad.mean+sad.1%*%x} )  #?

#display the rank=i plot
display(plot.sad[[i]])

#######################

load("combo.rda")
library(CCA)
combo.x=as.matrix(combo[,-(1:6)])
combo.y=as.matrix(combo[,(1:6)])
combo.cca=cc(X=combo.x,Y=combo.y)
barplot(combo.cca$cor,xlab="Dimension",ylab="Canonical Correlation",ylim=c(0,1))

x2=as.vector(combo.cca$scores$xscores[,2])
y2=as.vector(combo.cca$scores$yscores[,2])
plot(x2,y2)
