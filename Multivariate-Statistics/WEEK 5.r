setwd("e://")
getwd()
library(glmnet)
pet=read.table("PET.txt",header=T)
pet_train=pet[,-ncol(pet)]

##LASSO
pet.cv.lasso=cv.glmnet(x=as.matrix(pet[,1:268]),
                       y=pet$y,
                       nfolds = 5,
                       alpha=1,
                       type.measure = "deviance")
plot.cv.glmnet(pet.cv.lasso)#plot(pet.cv.lasso)
lass0.coef=as.vector(coef.cv.glmnet(pet.cv.lasso,s="lambda.1se"))
names(lass0.coef)=row.names(coef.cv.glmnet(pet.cv.lasso,s="lambda.1se"))
lass0.coef
#PREDICTION  
pred.lasso=predict.cv.glmnet(pet.cv.lasso,
                             newx=as.matrix(pet[,1:268]),
                             s="lambda.1se")
err.lasso=pred.lasso-pet$y
sum((err.lasso)^2)/8
#LASSO Path
#by package(lars)



##LARs
library(lars)
lar.pet=lars(x=as.matrix(pet[,1:268]),
               y=pet$y,type="lar") #lasso
lar.pet
plot(lar.pet)#plot.lars(lar.pet)
#Cross-validation MSE Plot 
cv.lar=cv.lars(x=as.matrix(pet[,1:268]),
               y=pet$y,
               type='lar')
pred.lar=predict.lars(cv.lar,type='fit',s=6,
                      mode='step',
                      newx=as.matrix(pet[,1:268]))

##PCR=Principle Component Regression
pet.pcr=pcr(y~.,data=pet[,1:269],validation="CV")
pet.plsr=plsr(y~.,data=pet[,1:269],validation="CV")
pcr.cvplot(pet.plsr,err.stand="MSEP")
