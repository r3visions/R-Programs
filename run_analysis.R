library(data.table)

## Meta Data
FeatureName <- read.table("UCI HAR Dataset/features.txt")
ActivityLabel <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
## Test Data
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test_set <- read.table("UCI HAR Dataset/test/x_test.txt", header = FALSE)
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
## Training Data
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train_set <- read.table("UCI HAR Dataset/train/x_train.txt", header = FALSE)
train_labels <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)

## Step 1: Merge the training and the test sets to create one data set.
subject <- rbind(test_subject, train_subject)
set <- rbind(test_set, train_set)
labels <- rbind(test_labels, train_labels)
colnames(subject) <- "Subject"
colnames(labels) <- "Activity"
colnames(set) <- t(FeatureName[2])
DataSet <- cbind(set,labels,subject)
## Step 2:Extract only the measurements on the mean and standard deviation for each measurement. 
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(DataSet), ignore.case=TRUE)
# Adding labels and subject
columnsWithMeanSTD <- c(columnsWithMeanSTD, 562, 563)
columnsinstep2 <- DataSet[,columnsWithMeanSTD]
dim(columnsinstep2)
## Step 3
columnsinstep2$Activity <- as.character(columnsinstep2$Activity)
for (i in 1:6){
  columnsinstep2$Activity[columnsinstep2$Activity == i] <- as.character(ActivityLabel[i,2])
}
columnsinstep2$Activity <- as.factor(columnsinstep2$Activity)
## Part 4
names(columnsinstep2)<-gsub("Acc", "Accelerometer", names(columnsinstep2))
names(columnsinstep2)<-gsub("Gyro", "Gyroscope", names(columnsinstep2))
names(columnsinstep2)<-gsub("BodyBody", "Body", names(columnsinstep2))
names(columnsinstep2)<-gsub("Mag", "Magnitude", names(columnsinstep2))
names(columnsinstep2)<-gsub("^t", "Time", names(columnsinstep2))
names(columnsinstep2)<-gsub("^f", "Frequency", names(columnsinstep2))
names(columnsinstep2)<-gsub("tBody", "TimeBody", names(columnsinstep2))
names(columnsinstep2)<-gsub("-mean()", "Mean", names(columnsinstep2), ignore.case = TRUE)
names(columnsinstep2)<-gsub("-std()", "STD", names(columnsinstep2), ignore.case = TRUE)
names(columnsinstep2)<-gsub("-freq()", "Frequency", names(columnsinstep2), ignore.case = TRUE)
names(columnsinstep2)<-gsub("angle", "Angle", names(columnsinstep2))
names(columnsinstep2)<-gsub("gravity", "Gravity", names(columnsinstep2))
## part 5
columnsinstep2$Subject <- as.factor(columnsinstep2$Subject)
columnsinstep2 <- data.table(columnsinstep2)

tidyData <- aggregate(. ~Subject + Activity, columnsinstep2, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

