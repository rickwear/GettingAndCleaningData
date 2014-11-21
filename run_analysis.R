# Getting and Cleaning Data Project - Week 3
# This R script does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# 1. Merges the training and the test sets to create one data set.

# Need the working directory setting in RStudio

set1 <- read.table("UCI HAR Dataset/train/X_train.txt")
set2 <- read.table("UCI HAR Dataset/test/X_test.txt")
setX <- rbind(set1, set2)

set1 <- read.table("UCI HAR Dataset/train/subject_train.txt")
set2 <- read.table("UCI HAR Dataset/test/subject_test.txt")
setS <- rbind(set1, set2)

set1 <- read.table("UCI HAR Dataset/train/y_train.txt")
set2 <- read.table("UCI HAR Dataset/test/y_test.txt")
setY <- rbind(set1, set2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("UCI HAR Dataset/features.txt")
meanAndStandardFeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
setX <- setX[, meanAndStandardFeatures]
names(setX) <- features[meanAndStandardFeatures, 2]
names(setX) <- gsub("\\(|\\)", "", names(setX))
names(setX) <- tolower(names(setX))

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
setY[,1] = activities[setY[,1], 2]
names(setY) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(setS) <- "subject"
joinedSet <- cbind(setS, setY, setX)
write.table(joinedSet, "merged_clean_data.txt")

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(setS)[,1]
numberOfSubjects = length(unique(setS)[,1])
numberOfActivities = length(activities[,1])
numberOfColumns = dim(joinedSet)[2]
result = joinedSet[1:(numberOfSubjects*numberOfActivities), ]

row = 1
for (s in 1:numberOfSubjects) {
	for (a in 1:numberOfActivities) {
		result[row, 1] = uniqueSubjects[s]
		result[row, 2] = activities[a, 2]
		tmp <- joinedSet[joinedSet$subject==s & joinedSet$activity==activities[a, 2], ]
		result[row, 3:numberOfColumns] <- colMeans(tmp[, 3:numberOfColumns])
		row = row + 1
	}
}

# Write out the resulting data suppressing any row name values

write.table(result, "project_tidy_data.txt", row.names=FALSE)
