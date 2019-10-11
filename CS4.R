library(mlbench)
library(e1071)
library(dplyr)
data("PimaIndiansDiabetes")
#take a look
head(PimaIndiansDiabetes)
tail(PimaIndiansDiabetes)
#dimension
dim(PimaIndiansDiabetes)
#structure
glimpse(PimaIndiansDiabetes)
#class
sapply(PimaIndiansDiabetes,class)
#standard deviation
sapply(PimaIndiansDiabetes[,1:8],sd)
#skewness
apply(PimaIndiansDiabetes[,1:8],2,skewness)
#class distribution
levels(PimaIndiansDiabetes$diabetes)
y<-PimaIndiansDiabetes$diabetes
table(y)
cbind(freq=table(y),perc=round(prop.table(table(y)*100),2))
#Summary
summary(PimaIndiansDiabetes)
#Correlation
cor(PimaIndiansDiabetes[,1:8])

#Understand the data using data Visualisation
#Univariate analysis
data("iris")
#Histogram
par(mfrow=c(1,4))
for(i in 1:4){
  hist(iris[,i], main = names(iris)[i])}

#Density plot
par(mfrow=c(1,4))
for(i in 1:4){
  plot(density(iris[,i]), main = names(iris)[i])}

#Box and Whisker plot
par(mfrow=c(1,4))
for(i in 1:4){
  boxplot(iris[,i], main = names(iris)[i])}

#Missing plot
#Currently unavilable
#Checking for an alternative solution
library("Amelia")
data("Soybean")
missmap(Soybean, col=c("red", "yellow"), legend=FALSE)

#Multivariate visualization
#Correlation plot
#Currently unavailable
#Checking for an alternative
#install.packages("corrplot")
library(corrplot)
install.packages("corrplot")
x<-cor(iris[,1:4])
corrplot(x, method='circle')

#Scatter plot
par(mfrow=c(1,4))
pairs(iris[,1:4])
#Scatter plot matrix by class
pairs(Species~., data=iris, color=iris$Species)

#Box and Whisker plot by class
library(caret)
x<-iris[,1:4]
y<-iris[,5]
featurePlot(x,y,plot="box")

#Pre-processing
summary(iris[,1:4])
#Standardize the data(with mean value of 0 and standard deviation of 1)
preProcessParams<-preProcess(iris[,1:4],method = c("center","scale"))
print(preProcessParams)
#Normalize the data
preProcessParams2<- preProcess(iris[,1:4], method = "range")
print(preProcessParams2)
#standalone transformation
transformed<-predict(preProcessParams,iris[,1:4])
summary(transformed)

#Box-Cox
summary(PimaIndiansDiabetes[,7:8])
preProcessParams3<-preProcess(PimaIndiansDiabetes[,7:8],method = "BoxCox")
print(preProcessParams3)
transformed<-predict(preProcessParams3, PimaIndiansDiabetes[,7:8])
summary(transformed)

#PCA
summary(iris)
preProcessParams4<-preProcess(iris, method = c("center","scale","pca"))
print(preProcessParams4)
transformed<-predict(preProcessParams4, iris)
summary(iris)

#ICA
summary(PimaIndiansDiabetes[,1:8])
preProcessParams5<-preProcess(PimaIndiansDiabetes[,1:8],method = c("center","scale","ica"), n.comp=5)
print(preProcessParams5)
transformed<-predict(preProcessParams5,PimaIndiansDiabetes[,1:8])
summary(transformed)

#Resampling to estimate the model accuracy
#train-test split
library(klaR)
index<-createDataPartition(iris$Species,p=0.8,list = FALSE)
trainData<-iris[index,]
testData<-iris[-index,]
fit.nb<-NaiveBayes(Species~.,data=trainData)
print(fit.nb)
pred<-predict(fit.nb,testData)
confusionMatrix(pred$class,testData$Species)
#Bootstrap sampling
control<-trainControl(method = "boot", number = 100)
mod1<-train(Species~., trainData, trControl = control, method = "nb")
print(mod1)
#cross-validation
control2<-trainControl(method = "cv", number = 10)
mod2<-train(Species~., trainData, trControl = control2, method = "nb")
print(mod2)
#repeated cv
control3<-trainControl(method = "repeatedcv",number = 10, repeats = 3)
mod3<-train(Species~., trainData, trControl = control3, method = "nb")
print(mod3)
#Leave one out cross validation
control4 <- trainControl(method="LOOCV")
mod4<-train(Species~., trainData, trControl = control4, method = "nb")
print(mod4)

#Model Evaluation Metrics
#1.accuracy on binary classification
data("PimaIndiansDiabetes")
control<-trainControl(method = "cv", number = 10)
modelFit<-train(diabetes~.,PimaIndiansDiabetes,method="glm",metric="Accuracy",trControl=control)
print(modelFit)
#2.RMSE/R^2 on regression problem
data(longley)
control<-trainControl(method = "cv", number = 10)
modelFit<-train(Employed~.,longley,method="lm",metric="RMSE",trControl=control)
print(modelFit)
#3.ROC
control<-trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)
modelFit<-train(diabetes~.,PimaIndiansDiabetes,method="glm",metric="ROC",trControl=control)
print(modelFit)
#4.Log-loss
control<-trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = mnLogLoss)
modelFit<-train(Species~.,iris,method="rpart",metric="logLoss",trControl=control)
print(modelFit)