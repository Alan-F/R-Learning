setwd("E:\\Multivariate Statistics WEEK 9")
load("spamtrain.rda")
ncol(spam.train)

mod=glm(class~.,data=spam.train,family = "binomial")
summary(mod)
coef(mod)

library(glmnet)
mod2=cv.glmnet(x=as.matrix(spam.train[,1:57]),
               y=spam.train$class,family="binomial",
               alpha=1)
summary(mod2)
plot.cv.glmnet(mod2)
coef(mod2,s="lambda.1se")

mod3=step(mod)
#################
library(MASS)
mod4=qda(class~.,data=spam.train,method="mle",CV=T)
