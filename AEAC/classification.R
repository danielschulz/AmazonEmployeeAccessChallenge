
# SETUP WORKSPACE

library(e1071)
library(randomForest)
library(kernlab)
library(rpart)


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
rawData$ACTION = as.numeric(rawData$ACTION)
rawData$RESOURCE = as.numeric(rawData$RESOURCE)
rawData$MGR_ID = as.numeric(rawData$MGR_ID)
rawData$ROLE_ROLLUP_1 = as.numeric(rawData$ROLE_ROLLUP_1)
rawData$ROLE_ROLLUP_2 = as.numeric(rawData$ROLE_ROLLUP_2)
rawData$ROLE_DEPTNAME = as.numeric(rawData$ROLE_DEPTNAME)
rawData$ROLE_TITLE = as.numeric(rawData$ROLE_TITLE)
rawData$ROLE_FAMILY_DESC = as.numeric(rawData$ROLE_FAMILY_DESC)
rawData$ROLE_FAMILY = as.numeric(rawData$ROLE_FAMILY)
rawData$ROLE_CODE = as.numeric(rawData$ROLE_CODE)


rawDataTargets$id = as.numeric(rawDataTargets$id)
rawDataTargets$ACTION = as.numeric(0)
rawDataTargets$RESOURCE = as.numeric(rawDataTargets$RESOURCE)
rawDataTargets$MGR_ID = as.numeric(rawDataTargets$MGR_ID)
rawDataTargets$ROLE_ROLLUP_1 = as.numeric(rawDataTargets$ROLE_ROLLUP_1)
rawDataTargets$ROLE_ROLLUP_2 = as.numeric(rawDataTargets$ROLE_ROLLUP_2)
rawDataTargets$ROLE_DEPTNAME = as.numeric(rawDataTargets$ROLE_DEPTNAME)
rawDataTargets$ROLE_TITLE = as.numeric(rawDataTargets$ROLE_TITLE)
rawDataTargets$ROLE_FAMILY_DESC = as.numeric(rawDataTargets$ROLE_FAMILY_DESC)
rawDataTargets$ROLE_FAMILY = as.numeric(rawDataTargets$ROLE_FAMILY)
rawDataTargets$ROLE_CODE = as.numeric(rawDataTargets$ROLE_CODE)


data = rawData
targets = rawDataTargets

rm(list=c("rawData", "rawDataTargets", "dataLocation", "dataLocationTargets"))

# CLASSIFICATION
data$id = -1
trainTrainDataSize = floor(nrow(data)/3)
trainIgnoreDataSize = nrow(data) - trainTrainDataSize
testDataIndex = c(rep(FALSE, trainTrainDataSize), rep(TRUE, (nrow(targets) + trainIgnoreDataSize)))
d = rbind(data, targets)

formula = ACTION ~ RESOURCE + MGR_ID + ROLE_ROLLUP_1 + ROLE_ROLLUP_2 + ROLE_DEPTNAME + ROLE_TITLE + ROLE_FAMILY_DESC + ROLE_FAMILY + ROLE_CODE

rf = randomForest(formula=formula, data = d[!testDataIndex,], 
                  importance=TRUE, proximity=TRUE, na.action=na.roughfix,
                  kernel="rbfdot", kpar=list(sigma=0.015), C=70, cross=4, prob.model=TRUE)
# pr = predict(rf, d[testDataIndex,])
# summary(pr)


testDataIndex = c(rep(FALSE, trainTrainDataSize + trainIgnoreDataSize), rep(TRUE, nrow(targets)))
pr = predict(rf, d[testDataIndex,])
summary(pr)

sampleSubmission$Action = pr
write.table(sampleSubmission, file = paste0(WD, "/output/data/mySubmission.csv"), na="-1", col.names=TRUE, row.names=FALSE, fileEncoding="", sep=",")