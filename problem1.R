
setwd("C:/SVMnNN")
data <- read.csv("data.csv",sep=";",header = TRUE,stringsAsFactors = T,na.strings = c(' '))

dim(data)
summary(data)
table(data$nature.of.driver)
pie(table(data$nature.of.driver))

x <- subset(data, select=-nature.of.driver)
y <- data$nature.of.driver
index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/2))
testset <- data[testindex,]
trainset <- data[-testindex,]
x_test <- x[testindex,]
x_train <- x[-testindex,]
y_test <- y[testindex]
y_train <- y[-testindex]

y_test <- as.factor(y_test)
y_train <- as.factor(y_train)

library("e1071")


#1 linear SVM:
svm_model_linear<- svm(x_train, y_train,method="C-classification", kernel="linear", cost=1, gamma=0.5)
summary(svm_model_linear)
svm_model=svm_model_linear
pred <- predict(svm_model,x_test)
table(pred,y_test)
mean(pred==y_test)


#2 Gaussian kernel SVM:
svm_model_radial<- svm(x_train, y_train,method="C-classification",kernel="radial", cost=1, gamma=0.5)
summary(svm_model_radial)
svm_model=svm_model_radial
pred <- predict(svm_model,x_test)
table(pred,y_test)
mean(pred==y_test)

#polynomial kernel SVM:
svm_model_polynomial<- svm(x_train, y_train ,method="C-classification",kernel="polynomial", cost=0.5, gamma=0.5)
summary(svm_model_polynomial)
svm_model=svm_model_polynomial
pred <- predict(svm_model_polynomial,x_test)
table(pred,y_test)
mean(pred==y_test)

#sigmoid kernel SVM:
svm_model_polynomial<- svm(x_train, y_train ,method="C-classification",kernel="sigmoid", cost=0.5, gamma=0.5)
summary(svm_model_polynomial)
svm_model=svm_model_polynomial
pred <- predict(svm_model_polynomial,x_test)
table(pred,y_test)
mean(pred==y_test)

  
  
  