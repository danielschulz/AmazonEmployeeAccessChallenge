
# SETUP WORKSPACE

library(e1071)
library(randomForest)
library(kernlab)
library(rpart)
library(klaR)
library(caret)


# INIT

# clean
rm(list = ls()[!(ls() %in% PERSISTENT_CONSTANTS)])

setwd("C:/Users/schulzda/Desktop/Work/AEAC/AEAC/AEAC/")

# constants
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS", "WD")
WD = getwd()

dataLocation = paste0(WD, "/input/data/train.csv")
dataLocationTargets = paste0(WD, "/input/data/test.csv")
sampleSubmission = paste0(WD, "/input/data/sampleSubmission.csv")


# LOAD DATA
rawData = read.csv(dataLocation, header=TRUE, sep=",")
rawDataTargets = read.csv(dataLocationTargets, header=TRUE, sep=",")
sampleSubmission = read.csv(sampleSubmission, header=TRUE, sep=",")


# TRANSFORM
rawData$ACTION = as.factor(rawData$ACTION)
rawData$RESOURCE = as.factor(rawData$RESOURCE)
rawData$MGR_ID = as.factor(rawData$MGR_ID)
rawData$ROLE_ROLLUP_1 = as.factor(rawData$ROLE_ROLLUP_1)
rawData$ROLE_ROLLUP_2 = as.factor(rawData$ROLE_ROLLUP_2)
rawData$ROLE_DEPTNAME = as.factor(rawData$ROLE_DEPTNAME)
rawData$ROLE_TITLE = as.factor(rawData$ROLE_TITLE)
rawData$ROLE_FAMILY_DESC = as.factor(rawData$ROLE_FAMILY_DESC)
rawData$ROLE_FAMILY = as.factor(rawData$ROLE_FAMILY)
rawData$ROLE_CODE = as.factor(rawData$ROLE_CODE)


rawDataTargets$id = as.factor(rawDataTargets$id)
rawDataTargets$ACTION = as.factor(0)
rawDataTargets$RESOURCE = as.factor(rawDataTargets$RESOURCE)
rawDataTargets$MGR_ID = as.factor(rawDataTargets$MGR_ID)
rawDataTargets$ROLE_ROLLUP_1 = as.factor(rawDataTargets$ROLE_ROLLUP_1)
rawDataTargets$ROLE_ROLLUP_2 = as.factor(rawDataTargets$ROLE_ROLLUP_2)
rawDataTargets$ROLE_DEPTNAME = as.factor(rawDataTargets$ROLE_DEPTNAME)
rawDataTargets$ROLE_TITLE = as.factor(rawDataTargets$ROLE_TITLE)
rawDataTargets$ROLE_FAMILY_DESC = as.factor(rawDataTargets$ROLE_FAMILY_DESC)
rawDataTargets$ROLE_FAMILY = as.factor(rawDataTargets$ROLE_FAMILY)
rawDataTargets$ROLE_CODE = as.factor(rawDataTargets$ROLE_CODE)


data = rawData
targets = rawDataTargets

rm(list=c("rawData", "rawDataTargets", "dataLocation", "dataLocationTargets"))

# CLASSIFICATION
data$id = -1
trainTrainDataSize = floor(nrow(data)/300)
trainIgnoreDataSize = nrow(data) - trainTrainDataSize
testDataIndex = c(rep(FALSE, trainTrainDataSize), rep(TRUE, (nrow(targets) + trainIgnoreDataSize)))
d = rbind(data, targets)

formula = ACTION ~ RESOURCE + MGR_ID + ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_TITLE + ROLE_FAMILY_DESC + ROLE_FAMILY + ROLE_CODE

# svm = svm(formula=formula, data = d[!testDataIndex,], 
#                   importance=TRUE, proximity=TRUE, na.action=na.roughfix, prob.model=TRUE)
# kernel="rbfdot", kpar=list(sigma=0.015), C=70, cross=4, 
# pr = predict(rf, d[testDataIndex,])
# summary(pr)
# trcon = trainControl(method="cv",number=10)

# model = train(d[!testDataIndex,-1], d[!testDataIndex,]$ACTION, method="nb", 
#               form=formula,
#               trControl=trcon, na.action=na.roughfix)

# rf = randomForest(formula=formula, data = d[!testDataIndex,c(-11)],
#        importance=TRUE, proximity=TRUE, na.action=na.roughfix, prob.model=TRUE)

nb = NaiveBayes(d[!testDataIndex,c(-11)], d[!testDataIndex,c(-11)]$ACTION,
       cv.fold=20, formula=formula, proximity=TRUE, na.action=na.roughfix, prob.model=TRUE)

testDataIndex = c(rep(FALSE, trainTrainDataSize + trainIgnoreDataSize), rep(TRUE, nrow(targets)))
pr = predict(nb, d[testDataIndex,])
summary(pr)

sampleSubmission$Action = as.numeric(pr$posterior[,1])
# sampleSubmission$Action = sampleSubmission$Action - 1
# sampleSubmission$Action = pr$posterior[,2]
# sampleSubmission$Action = sampleSubmission$Action[,2]


# sampleSubmission$Action = as.numeric(sampleSubmission$Action)
# sampleSubmission$Action = sampleSubmission$Action - 1
hist(sampleSubmission$Action)
sampleSubmission[which(sampleSubmission$Action > 0.9),]$Action = 1
sampleSubmission[which(sampleSubmission$Action < 0.8),]$Action = 0

# sampleSubmission = sampleSubmission[,-3]
# hist(as.numeric(pr$posterior[,1]))

write.table(sampleSubmission, file = paste0(WD, "/output/data/nb_02.csv"), na="0", col.names=TRUE, row.names=FALSE, fileEncoding="", sep=",", quote=FALSE)