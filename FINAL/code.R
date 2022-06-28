setwd("/Users/Username/RCode/FINAL")

install.packages("dplyr")
install.packages("factoextra")
library(factoextra)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(class)
library(caret)
library(scales)
library(randomForest)
library (e1071)
library(ggplot2)

#1.1
slp <- read.csv("sleep.csv",stringsAsFactors = TRUE);
attach(slp)
slp[,'Gender'] <-as.factor(slp[,'Gender'])
is.factor(slp$Age)
is.factor(slp$Gender)
is.factor(slp$Poverty)
is.factor(slp$Depressed)
is.factor(slp$BMI)
summary(slp)

#1.2
set.seed(1000)
n <- nrow(slp)
tr.idx <- sample.int(n, size = n*7/10, replace=FALSE)
qslp.train<-slp[tr.idx,] #target variable인 gender를 포함한 train set
qslp.test<-slp[-tr.idx,] #target variable인 gender를 포함한 test set
slp.train<-slp[tr.idx,-10] #target variable인 gender를 제외한 train set
slp.test<-slp[-tr.idx,-10] #target variable인 gender를 제외한 test set
trainLabels<-slp[tr.idx,10]  #target variable인 gender의 train set
testLabels<-slp[-tr.idx,10]  #target variable인 gender의 test set

LR_out0<-glm(SleepTrouble~Age+Gender+Poverty+Depressed+BMI+PhysActive+SmokeNow+AlcoholYear+SleepHrsNight,data=qslp.train,family=binomial(logit))
summary(LR_out0)  
LR_probability = predict(LR_out0, newdata = qslp.test, type = 'response')
LR_pred = ifelse(LR_probability>0.5, 1, 0)
confusionMatrix(as.factor(LR_pred),qslp.test$SleepTrouble)


#1.3
ggplot(slp, aes(x=SleepTrouble, y=Gender, color=Gender))+geom_point(size=1.5) 
             
             
#1.4
m1<-svm(SleepTrouble~., data = qslp.train)
summary(m1)
pred1<-predict(m1,qslp.test) # radial basis
confusionMatrix(pred1, qslp.test$SleepTrouble)

#1.5
RF_out<-randomForest(SleepTrouble~.,data=qslp.train,importance=T,ntree=20, mtry=3)
RF_out
round(importance(RF_out), 2)  #분류의 정확도에 기여도가 높은 변수를 (소수점 2자리로 나타냄)
varImpPlot(RF_out)
RF_pred<-predict(RF_out,qslp.test)
confusionMatrix(RF_pred,qslp.test$SleepTrouble)

#2.1
wine <- read.csv("wine_binary.csv", stringsAsFactors = TRUE)
wine[,'quality'] <-as.factor(wine[,'quality'])
attach(wine)
table(quality)
str(wine)
set.seed(1000)
dat1<-wine[ , -12]
#km <- kmeans(dat1, 5, nstart = 25)
km <- kmeans(dat1, 3, nstart = 25)
km
km <- kmeans(dat1, 3, nstart=10)
km
km <- kmeans(dat1, 3)
km
pam_out <- pam(dat1, 3)
fviz_cluster(km, data = dat1, ellipse.type="convex", repel = TRUE)
fviz_cluster(pam_out, data = dat1, ellipse.type="convex", repel = TRUE)