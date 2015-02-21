library(plyr)

## You should create one R script called run_analysis.R that does the following. 

## 1. Merges the training and the test sets to create one data set.

subject1 <- read.table("test/subject_test.txt")
subject2 <- read.table("train/subject_train.txt")
Subject <- rbind(subject1, subject2)

x1 <- read.table("test/X_test.txt")
x2 <- read.table("train/X_train.txt")
X <- rbind(x1, x2)

y1 <- read.table("test/y_test.txt")
y2 <- read.table("train/y_train.txt")
Y <- rbind(y1, y2)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))


## 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"



## 4. Appropriately labels the data set with descriptive variable names. 
names(Subject) <- "subject"
clean <- cbind(Subject, Y, X)
write.table(clean, "merged_clean_data.txt")



## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
##    for each activity and each subject.

uniqueSubjects = unique(Subject)[,1]
lenSubjects = length(unique(Subject)[,1])
lenActivities = length(activities[,1])
numCols = dim(clean)[2]
result = clean[1:(lenSubjects*lenActivities), ]

row = 1
for (s in 1:lenSubjects) {
  for (a in 1:lenActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    temp <- clean[clean$subject==s & clean$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(temp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_averages.txt")
