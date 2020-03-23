install.packages("kohonen")
#probleme 1
#Explore the data
setwd("C:/SVMnNNnSOM")
data <- read.csv("data.csv",sep=";",header = TRUE,stringsAsFactors = T,na.strings = c(' '))
dim(data)
summary(data)
table(data$nature.of.driver)
pie(table(data$nature.of.driver))

x <- subset(data, select=-nature.of.driver)
y <- data$nature.of.driver
## split data into a train and test set
index <- 1:nrow(data)
testindex <- sample(index, trunc(length(index)/2))
testset <- data[testindex,]
trainset <- data[-testindex,]

x_test <- scale(x[testindex,])
x_train <- scale(x[-testindex,])
y_test <- y[testindex]
y_train <- y[-testindex]

y_test <- as.factor(y_test)
y_train <- as.factor(y_train)

#What library should be used?
library("kohonen")

set.seed(7)
# train model

#create SOM grid
som.model <- som(x_train, grid = somgrid(5, 5, "hexagonal"))

## use hierarchical clustering to cluster the codebook vectors
groups<-2# number of groupes or classes
som.hc <- cutree(hclust(dist(som.model$codes[[1]])), groups)

#plot
plot(som.model, type="codes", bgcol=rainbow(groups)[som.hc])

#cluster boundaries
add.cluster.boundaries(som.model, som.hc)


# prediction
som.prediction <- predict(som.model, newdata = x_test,trainX = x_train,trainY = y_train)
pred=som.hc[som.prediction$unit.classif]
table(y_test, pred)

#accuracy
mean(pred==y_test)
