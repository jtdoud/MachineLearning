
# Title:    Practical Machine Learning Course Project
# Author:   Tucker Doud
# Date:     August 9, 2015
# Notes:    Don't forget to split data befor EDA.
#           Recode to use only numeric indicies.

# Set up
setwd("~/GitHub/MachineLearning")
library(caret)
dat <- read.csv("./Data/pml-training.csv", stringsAsFactors = F)

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
iMdl <- sapply(X = trainDat[, iNum], FUN = countNA)
iMdl <- names(iMdl[which(iMdl == 0)])
str(trainDat[, iMdl])

# Remove meta trainData and add outcome
iMdl <- iMdl[-c(1:4)]
iMdl <- append(x = iMdl, values = "classe") # Final variables to use!

# Model building
mdl <- trainDat[, iMdl]
nzv <- nearZeroVar(x = mdl[, -53], saveMetrics = T)
mdlFit <- train(classe ~ ., method = "rpart", data = mdl)
