rm(list=ls())

setwd("/Users/mimihoang/coursera/predmachlearn-005/")

library(caret)
library(randomForest)
library(kernlab)
library(corrplot)

# create a data folder if it doesn't exist
if (!file.exists("data")) { dir.create("data") }

# download the training and test data sets
trainingUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainingFile <- "./data/training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testFile <- "./data/testing.csv"

# download training and test files
download.file(trainingUrl, trainingFile, method="curl")
download.file(testUrl, testFile, method="curl")

trainingData <- read.csv("./data/training.csv", na.strings = c("", " ", "NA", "NULL"))

trainingNA <- apply(trainingData, 2, function(x) {sum(is.na(x))})
cleanTraining <- trainingData[,which(trainingNA == 0)]
cleanTraining <- cleanTraining[8:length(cleanTraining)]

inTrain <- createDataPartition(y = cleanTraining$classe, p = 0.7, list = FALSE)
training <- cleanTraining[inTrain, ]
crossVal <- cleanTraining[-inTrain, ]

model <- randomForest(classe ~ ., data = training)

predictCrossVal <- predict(model, crossVal)
confusionMatrix(crossVal$classe, predictCrossVal)

testData <- read.csv("./data/testing.csv", na.strings= c("", " ", "NA", "NULL"))
testNA <- apply(testData, 2, function(x) {sum(is.na(x))})
cleanTest <- testData[,which(testNA == 0)]
cleanTest <- testData[8:length(testData)]

predictTest <- predict(model, cleanTest)

