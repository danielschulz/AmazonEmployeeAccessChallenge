
library(e1071)
library(randomForest)

rm(list=ls())

head(iris)
test = iris[ c(1:10, 51:60, 101:110), ]
train = iris[ c(11:50, 61:100, 111:150), ]
r = randomForest(Species ~., data=train, importance=TRUE, do.trace=100)
print(r)

iris.predict = predict(r, test)
iris.predict

t = table(observed=test[,'Species'], predict=iris.predict)
t

prop.table(t, 1)
