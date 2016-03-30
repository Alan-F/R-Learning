library(leaps)
setwd("e://")
load('bdfat.rda')

#All Possible Subsets
all.bdfat=regsubsets(x=as.matrix(bdfat[,1:3]),
                     y=bdfat$bodyfat , 
                     nvmax=3 ,
                     method="exhaustive",
                     really.big = F) 
#really.big=T if variables >50
#exhaustive search/forward selection/backward selection/sequential replacement
plot(all.bdfat,scale = "Cp")


#Greedy Path
library(MASS)
null.bdfat=lm(bodyfat~1,data=bdfat)
full.bdfat=lm(bodyfat~.,data=bdfat)
#Forward
step(object = null.bdfat,
     scope=list(lower=null.bdfat,upper=full.bdfat),
     direction = "forward")
#Both
step(object = null.bdfat,
scope=list(lower=null.bdfat,upper=full.bdfat),
direction = "both")
#Backward
step(object = full.bdfat,
     scope=list(lower=null.bdfat,upper=full.bdfat),
     direction = "backward")


##Ridge Reg
load('pettrain.rda')
View(pet.train)
library(glmnet)
pet.cv.rr=cv.glmnet(x=as.matrix(pet.train),
                    y=pet.train$y,nfolds=5,
                    type.measure='deviance', #loss function
                    alpha=0)# 0 means ridge reg
plot.cv.glmnet(pet.cv.rr)
rr.coef=as.vector(coef.cv.glmnet(pet.cv.rr,s="lambda.1se"))
names(rr.coef)=row.names((coef.cv.glmnet(pet.cv.rr,s="lambda.1se")))
rr.coef


##LASSO
pet.cv.lasso=cv.glmnet(x=as.matrix(pet.train[,1:268]),
                       y=pet.train$y,
                       nfolds = 5,
                       alpha=1,
                       type.measure = "deviance")
plot.cv.glmnet(pet.cv.lasso)
lass0.coef=as.vector(coef.cv.glmnet(pet.cv.lasso,s="lambda.1se"))
names(lass0.coef)=row.names(coef.cv.glmnet(pet.cv.lasso,s="lambda.1se"))
lass0.coef
#PREDICTION  
load('pettest.rda')
pred.lasso=predict.cv.glmnet(pet.cv.lasso,
                              newx=as.matrix(pet.test[,1:268]),
                              s="lambda.1se")
err.lasso=pred.lasso-pet.test$y
sum((err.lasso)^2)/8

##LARs
library(lars)
lar.bdfat=lars(x=as.matrix(bdfat[,1:3]),
               y=bdfat$bodyfat,type="lar")
lar.bdfat
cv.lar=cv.lars(x=as.matrix(pet.train[,1:268]),
                y=pet.train$y,
                type='lar')
pred.lar=predict.lars(cv.lar,type='fit',s=6,
                      mode='step',
                      newx=as.matrix(pet.test[,1:268]))
