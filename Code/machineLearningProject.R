

setwd("~/GitHub/MachineLearning")
dat <- read.csv("./Data/pml-training.csv", stringsAsFactors = F)

# Explore data
dim(dat)
names(dat)
cls <- sapply(X = dat, FUN = class)
table(cls) # Lots of character

# Create index to subset
iChar <- names(which(cls == "character"))
iNum <- names(which(cls == "integer" | cls == "numeric"))


lapply(X = dat[, iChar], FUN = unique) # all contain #DIV/0 or ""

findError <- function(col){
    sum(col == "#DIV/0!")
}
sapply(X = dat[, iChar], FUN = findError) # DIV/0! error in most

findBlank <- function(col){
    sum(col == "")
}
sapply(X = dat[, iChar], FUN = findBlank) # Blanks in most

countNA <- function(col){
    sum(is.na(col))
}
sapply(X = dat[, iNum], FUN = countNA)

# Keep numeric with 0 NAs
iMdl <- sapply(X = dat[, iNum], FUN = countNA)
iMdl <- names(iMdl[which(iMdl == 0)])
summary(dat[, iMdl])

# Remove meta data and add outcome
iMdl <- iMdl[-c(1:4)]
iMdl <- append(x = iMdl, values = "classe")
