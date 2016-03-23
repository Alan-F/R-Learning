setwd("H://")
pima=read.csv(file='pima.csv',header=T)
pima=pima[,-1];View(pima)
ind=sample(2,nrow(pima),replace=T,prob=c(0.8,0.2))

pima_train=pima[ind==1,]
pima_test=pima[ind==2,]


library(ada)
pima.ada=ada(class~.,loss="logistic",data=pima_train,iter=50,type="discrete")
summary(pima.ada)
plot(pima.ada)
pima.pred=predict(pima.ada,newdata=pima_test,type='vector')
prop.table(table(pima.pred==pima_test$class))




library(rpart)
library(rpart.plot)
pima.tree=rpart(class~.,data=pima_train,method='class',control=rpart.control(minsplit=0,cp=0.01),parms=list(split='information'))
prp(pima.tree,extra=2)
pima.tree.pred=predict(pima.tree,newdata=pima_test,type='class')
mean(pima.tree.pred!=pima_test$class)
varplot(model, max.var.show=)

#2
spam=read.csv("spambase.csv",header=T)
library(randomForest)
dim(spam)
ind2=sample(2,nrow(spam),replace=T,prob=c(0.8,0.2))
spam_train=spam[ind==1,]
spam_test=spam[ind==2,]
spam.rf=randomForest(class~.,data=spam_train,ntrees=10,mtry=5)
plot(spam.rf)
summary(spam.rf)
spam.rf
rf.pred=predict(spam.rf,newdata=spam_test)
prop.table(table(rf.pred==spam_test$class))

round(importance(spam.rf),2)
spam.rf$err.rate

varImpPlot(rfmodel, type=2, main=" biaoti",n.var=10)



library(gbm)
model=
  gbl(bodyfat~., data= , interaction.depth=2)
summary(model)


library(ipred)
bag.tree=bagging(class~., data= , nbagg=100, control=rpart.control(minisolit=2,cp=0,xval=0))
predict(model,newdata=, aggregation='majoraty')
mean(  != data$class)
