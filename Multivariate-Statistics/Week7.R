setwd("E:\\Multivariate Statistics WEEK 7\\data")
getwd()
################################################
load("npq.rda")
npq.lr=lm(cbind(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y13)~.,data=npq)
(coef.multivar.coef=t(npq.lr$coefficients))
###############################################
coef.mat=data.frame(matrix(nrow = 13,ncol=10))
colnames(coef.mat)=c("intercept",paste0("X",1:9)) 
#past0()/paste()
coef.mat
for(i in 1:13)
  {
  formu=formula(paste0("Y",i,"~",paste("X",1:9,sep="",collapse = "+")))
  coef.mat[i,]=unlist(coef(lm(formu,data=npq)))
}
coef.mat
###############################################

#Pretest: Profile analysis
load("pretest.rda")
library(profileR) # can not download
pbg(data=pretest[,2:6],group =pretest$treatment)
##############################################

#MANOVA
load("npk.rda")
npk.lr=lm(cbind(yield,foo)~.,npk)
library(car)
linearHypothesis(npk.lr,verbose = T,
                 hypothesis.matrix = c("N1=1","block2=K1"),
                 P=diag(x=1,nrow=2))
##############################################
#classical multivariate regression
load("pah.rda")
set.seed(1234)
train=sample(1:25,size=22)
pah.lr=lm(cbind(Py,Ace,Anth,Acy,Chry,Benz,Fluora,Fluore,Nap,Phen)~.,
            data = pah[train,])
View(pah.lr$coefficients)
predict(pah.lr,newdata=pah[-train,])

##############################################
#Multivariate PCR
library(pls)
pah.pcr=pcr(cbind(Py,Ace,Anth,Acy,Chry,Benz,Fluora,Fluore,Nap,Phen)~.,
            data=pah)
summary(pah.pcr)
#same as step-by-step PCR
yname=names(pah)[1:10]
xfor=paste(names(pah)[-(1:10)],sep="",collapse = "+")
for(i in 1:10){
  formu=formula(paste0(yname[i],"~",xfor))
  print(summary(pcr(formu,data=pah,validation="none")))
}

################################################
#Minimizing a weighted sum of squares
load("cct.rda")
source("rrr.R")
x=cct[,4:9]
y=cct[,1:3]
cct.rrr=rrr(x=x,y=y)
cct.rrr
###############################################
#CV for t
set.seed(1234)
train=sample(1:25,size=22)
pah.cct=rrr(x=pah[train,11:37],y=pah[train,1:10],k=0.001)
pred=t((pah.cct$C[[5]]) %*% as.matrix(t(pah[-train,11:37])))
colnames(pred)=names(pah[1:10])
round(pred,digits = 3)
pah[-train,1:10]
