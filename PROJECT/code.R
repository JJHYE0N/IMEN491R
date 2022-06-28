##################################
#0. 패키지 포함 및 데이터 준비작업
##################################
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")   #for confusion matrix
install.packages("scales")  #for graph
install.packages("randomForest")  #for randomForest
install.packages("e1071")   #SVM

library(dplyr)
library(ggplot2)
library(class)
library(caret)
library(scales)
library(randomForest)
library (e1071)

setwd("/Users/username/RCode/project")
wine <- read.csv("wine_binary.csv", stringsAsFactors = TRUE)
wine[,'quality'] <-as.factor(wine[,'quality'])
attach(wine)
table(quality)
str(wine)

###########################################################################
#1.기술 통계치 (빈도 평균 등) + 데이터 시각화 히스토그램 산점도 등 2개 이상 
###########################################################################
#1.1 기술통계치 (빈도 및 평균 등)
#################################
#평균
a1 <- select(wine, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar"
                     , "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")) %>% summarize_all(mean)
a2 <- select(wine, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", 
                     "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")) %>% summarize_all(sd)
a3 <- select(wine, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", 
                     "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")) %>% summarize_all(min)
a4 <- select(wine, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", 
                     "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")) %>% summarize_all(max)
table1 <- rbind(a1,a2,a3,a4)
rownames(table1) <- c("mean","sd","min","max")
table1
#빈도 
table(quality)
#기술통계치
summary(wine)


#1.2 데이터시각화 (히스토그램, 산점도 등 2개 이상) 
##################################################
hist(alcohol, col = quality, main="Histogram of Alcohol") #알코올의 히스토그램 (색깔=품질) 
ggplot(wine, aes(x=alcohol, y=density, color=quality))+geom_point(size=1.5) #알코올과 밀도의 산점도 (색깔=품질)


#####################################################
#2. 주어진 데이터를 7:3의 비율로 Train/Test set (1점)
#####################################################
set.seed(1000)
n <- nrow(wine)
tr.idx <- sample.int(n, size = n*7/10, replace=FALSE)
qwine.train<-wine[tr.idx,] #target variable인 quality를 포함한 train set
qwine.test<-wine[-tr.idx,] #target variable인 quality를 포함한 test set
wine.train<-wine[tr.idx,-12] #target variable인 quality를 제외한 train set
wine.test<-wine[-tr.idx,-12] #target variable인 quality를 제외한 test set
trainLabels<-wine[tr.idx,12]  #target variable인 quality의 train set
testLabels<-wine[-tr.idx,12]  #target variable인 quality의 test set


###################################################################
#3. K-NN, Decision Tree, Random Forest, Support vector machine,
#Logisitic Regression 중 3개이상의 기법을 적용하여 비교 분석  (5점)
###################################################################
#Random Forest
##############
RF_out<-randomForest(quality~.,data=qwine.train,importance=T)
RF_out
round(importance(RF_out), 2)  #분류의 정확도에 기여도가 높은 변수를 (소수점 2자리로 나타냄)
varImpPlot(RF_out)
RF_pred<-predict(RF_out,qwine.test)
confusionMatrix(RF_pred,qwine.test$quality)

#Support vector machine (SVM using kernel)
#######################
m1<-svm(quality~., data = qwine.train)
summary(m1)
m2<-svm(quality~., data = qwine.train,kernel="polynomial")
summary(m2)
m3<-svm(quality~., data = qwine.train,kernel="sigmoid")
summary(m3)
m4<-svm(quality~., data = qwine.train,kernel="linear")
summary(m4)

pred1<-predict(m1,qwine.test) # radial basis
confusionMatrix(pred1, qwine.test$quality)  #False =17
pred2<-predict(m2,qwine.test) # polynomial
confusionMatrix(pred2, qwine.test$quality)  #False = 26
pred3<-predict(m3,qwine.test) # sigmoid
confusionMatrix(pred3, qwine.test$quality)  #False = 36
pred4<-predict(m4,qwine.test) # linear
confusionMatrix(pred4, qwine.test$quality)  #False = 28


#Logistic Regression
####################
LR_out0<-glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides
             +free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=qwine.train,family=binomial(logit))
summary(LR_out0)  #AIC = 333.44
backwards = step(LR_out0) #최적의 AIC를 만드는 그룹을 찾는다.

LR_out<-glm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar 
            + total.sulfur.dioxide + density + pH + sulphates + alcohol, data=qwine.train,family=binomial(logit))
summary(LR_out)#AIC = 331.05

LR_probability = predict(LR_out, newdata = qwine.test, type = 'response')
LR_pred = ifelse(LR_probability>0.5, 1, 0)
confusionMatrix(as.factor(LR_pred),qwine.test$quality)

#########################################################
#4. 3의 결과를 기반으로 전체적 분류모형에 대한 평가 (2점)
#########################################################