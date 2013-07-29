
# SETUP WORKSPACE

library(e1071)
library(randomForest)
library(kernlab)


# INIT

# clean
rm(list = ls()[!(ls() %in% PERSISTENT_CONSTANTS)])

# constants
PERSISTENT_CONSTANTS = c("PERSISTENT_CONSTANTS", "WD")
WD = getwd()

dataLocation = paste0(WD, "/input/data/train.csv")


# LOAD DATA
rawData = read.csv(dataLocation, header=TRUE, sep=",")


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


data = rawData


# CLASSIFICATION
testDataIndex = c(rep(FALSE, nrow(data)))
d = rbind(data)

formula = data$ACTION ~ .
svm = ksvm(formula, data = d[!testDataIndex,], kernel="rbfdot", kpar=list(sigma=0.015), C=70, cross=4, prob.model=TRUE, na.action=na.roughfix)
pr = predict(svm, newdata = d[testDataIndex,])

