#probleme 1
#Explore the data
library("e1071")

setwd("C:/SVMnNN")
data <- read.csv("data.csv",sep=";",header = TRUE,stringsAsFactors = T,na.strings = c(' '))
#Perform 10 fold cross validation

# get the data from somewhere and specify number of folds 5
nrFolds <- 5

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(data))
pred.v <- rep(NA, nrFolds)
# actual cross validation
for(k in 1:nrFolds) {
  # actual split of the data
  #Segement your data by fold using the which() function 
  
  fold <- which(folds == k)
  trainData <- data[-fold,]
  testData <- data[fold,]
  x_train<-trainData[,1:2]
  y_train<-as.factor(trainData[,3])
  x_test<-testData[,1:2]
  y_test<-as.factor(testData[,3])
  
  svm_model_linear<- svm(x_train, y_train,method="C-classification", kernel="linear", cost=1, gamma=0.5)
  svm_model=svm_model_linear
  pred <- predict(svm_model,x_test)
  print(mean(pred==y_test))
  # train and test your model with data.train and data.test
}
pred.v
# get the data from somewhere and specify number of folds 10
nrFolds <- 10

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(data))
pred.v <- rep(NA, nrFolds)
# actual cross validation
for(k in 1:nrFolds) {
  # actual split of the data
  #Segement your data by fold using the which() function 
  
  fold <- which(folds == k)
  trainData <- data[-fold,]
  testData <- data[fold,]
  x_train<-trainData[,1:2]
  y_train<-as.factor(trainData[,3])
  x_test<-testData[,1:2]
  y_test<-as.factor(testData[,3])
  
  svm_model_linear<- svm(x_train, y_train,method="C-classification", kernel="linear", cost=1, gamma=0.5)
  svm_model=svm_model_linear
  pred <- predict(svm_model,x_test)
  pred.v[k]<-mean(pred==y_test)
  # train and test your model with data.train and data.test
}
pred.v
