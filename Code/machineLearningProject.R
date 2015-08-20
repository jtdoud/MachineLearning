
# Title:    Practical Machine Learning Course Project
# Author:   Tucker Doud
# Date:     August 9, 2015

# Set up
setwd("~/GitHub/MachineLearning")
library(caret)
library(ipred)
library(plyr)
dat <- read.csv("./Data/pml-training.csv", stringsAsFactors = F)
dat$classe <- factor(dat$classe)

# Partition
inTrain <- createDataPartition(y = dat$classe, p = 0.8, list = F)
trainDat <- dat[inTrain, ]
testDat <- dat[-inTrain, ]

# Explore data
dim(trainDat)
names(trainDat)
cls <- sapply(X = trainDat, FUN = class)
table(cls) # Lots of character

# Create index to subset
iChar <- which(cls == "character")
iNum <- which(cls == "integer" | cls == "numeric")

#lapply(X = trainDat[, iChar], FUN = unique) # all contain #DIV/0 or ""

findError <- function(col){
    sum(col == "#DIV/0!")
}
sapply(X = trainDat[, iChar], FUN = findError) # DIV/0! error in most

findBlank <- function(col){
    sum(col == "")
}
sapply(X = trainDat[, iChar], FUN = findBlank) # Blanks in most

countNA <- function(col){
    sum(is.na(col))
}
sapply(X = trainDat[, iNum], FUN = countNA) # Lots of NAs in summarized variables

# Keep numeric with 0 NAs
iPred <- sapply(X = trainDat[, iNum], FUN = countNA)
iPred <- names(iPred[which(iPred == 0)])
str(trainDat[, iPred])

# Remove meta data from trainData and add outcome
iPred <- iPred[-c(1:4)] # Final predictors to use!

rm(dat, inTrain, cls, iChar, iNum, countNA, findBlank, findError)

# Model building
nzv <- nearZeroVar(x = trainDat[, iPred], saveMetrics = T)

# tmp <- trainDat[sample(1:15699, size = 1000, replace = F), ] # Test on small size

library(doParallel)
registerDoParallel(cores=4)
ctrl <- trainControl(number = 10, returnData = F, savePredictions = F, trim = T)
mdlFit <- train(x = trainDat[, iPred], y = trainDat[, "classe"], method = "treebag",
                trControl = ctrl, allowParallel = T)
save(mdlFit, file = "./Code/mdlFit.Rdata", compress = T)

# Test model on Testing data
pred <- predict(mdlFit, newdata = testDat)
confusionMatrix(data = pred, reference = testDat$classe)

# Test model on assignment sample
finalTest <- read.csv("./Data/pml-testing.csv", stringsAsFactors = F)
finalPred <- predict(mdlFit, newdata = finalTest)
finalPred <- as.character(finalPred)

# Submission Files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
setwd("~/GitHub/MachineLearning/Submission")
pml_write_files(finalPred)
