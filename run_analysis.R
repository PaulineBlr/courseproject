#Course project


##reading the data

library(data.table)
library(dplyr)

dfFeatures <- read.table(file = "./UCI HAR Dataset/features.txt")
colnames(dfFeatures) <- c("id","name")

dfXtest <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")
colnames(dfXtest) <- dfFeatures$name
dtXtest <- data.table(dfXtest)
rm(dfXtest)

dfYtest <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
colnames(dfYtest) <- c("activityLabel")
dtYtest <- data.table(dfYtest)
rm(dfYtest)

dfSubjectTest <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")
colnames(dfSubjectTest) = c("subject")

dtTest <- data.table(dtXtest)
dtTest <- cbind(dfSubjectTest,dtTest)
dtTest <- cbind(dtYtest,dtTest)

rm(dfSubjectTest) ; rm(dtXtest) ; rm(dtYtest)

######

dfXtrain <- read.table(file = "./UCI HAR Dataset/train/X_train.txt")
colnames(dfXtrain) <- dfFeatures$name
dtXtrain <- data.table(dfXtrain)
rm(dfXtrain)

dfYtrain <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
colnames(dfYtrain) <- c("activityLabel")
dtYtrain <- data.table(dfYtrain)
rm(dfYtrain)

dfSubjectTrain <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")
colnames(dfSubjectTrain) = c("subject")

dtTrain <- data.table(dtXtrain)
dtTrain <- cbind(dfSubjectTrain,dtTrain)
dtTrain <- cbind(dtYtrain,dtTrain)

rm(dfSubjectTrain) ; rm(dtXtrain) ; rm(dtYtrain)

### MERGER

dt <- rbind(dtTest,dtTrain)
rm(dtTest); rm(dtTrain); rm(dfFeatures)

##remove unuseful feats
meanFeats <- grep("mean",colnames(dt),value=F)
stdFeats <- grep("std",colnames(dt),value=F)
toKeepFeats <- c(1,2,meanFeats,stdFeats)
toKeepFeats <- toKeepFeats[order(toKeepFeats, decreasing = FALSE)]
rm(meanFeats,stdFeats)

dt <- data.frame(dt)
dt <- dt[,toKeepFeats]

rm(toKeepFeats)

##appropriate labeling of activities
dtActivity <- read.table(file = "./UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE)

dt$activityLabel <- as.character(dt$activityLabel)

for(i in 1:dim(dtActivity)[1]){
        dt$descriptiveActivityLabel[dt$activityLabel==as.character(i)] <- dtActivity[i,2]
}

dt$activityLabel <- dt$descriptiveActivityLabel
dt <- select(dt,-descriptiveActivityLabel)

rm(dtActivity,i)

##second data set with average of features for each activity and subject

dtMeans <- dt %>% group_by(subject,activityLabel) %>% summarise_each(funs(mean))

colnames(dtMeans)[3:dim(dtMeans)[2]] <- paste(rep("mean",81-3),colnames(dtMeans)[3:dim(dtMeans)[2]],sep=".")

write.table(dtMeans,file = "dataset.txt",row.names = FALSE)
