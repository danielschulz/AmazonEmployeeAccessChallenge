# install.packages("klaR")
# install.packages("caret")

library("klaR")
library("caret")

x = iris[,-5]
y = iris$Species

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

model

pr = predict(model$finalModel,x)

t = table(pr$class, y)

# -----

# install.packages('ElemStatLearn')
library('ElemStatLearn')
library("klaR")
library("caret")

sub = sample(nrow(spam), floor(nrow(spam) * 0.9))
train = spam[sub,]
test = spam[-sub,]

xTrain = train[,-58]
yTrain = train$spam

xTest = test[,-58]
yTest = test$spam

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

prop.table(table(predict(model$finalModel,xTest)$class,yTest))
