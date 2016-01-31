##########################################################################################################

## Coursera: Getting and Cleaning Data Course Project
## Author: Laxman Kasula
## Date: 2016-01-31

# runAnalysis.R does following:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

library(plyr)

# 1. Merge the training and the test sets to create one data set.

# Load Data; already unzipped files

X_train <- read.table("data/UCI HAR DATASET/train/X_train.txt")
y_train <- read.table("data/UCI HAR DATASET/train/y_train.txt")
subject_train <- read.table("data/UCI HAR DATASET/train/subject_train.txt")

X_test <- read.table("data/UCI HAR DATASET/test/X_test.txt")
y_test <- read.table("data/UCI HAR DATASET/test/y_test.txt")
subject_test <- read.table("data/UCI HAR DATASET/test/subject_test.txt")

# Creating 'x','y', and 'subject_data' sets
X_data <- rbind(X_train, X_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)


# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("data/UCI HAR DATASET/features.txt")

# Select only columns with mean() or std() in their names from features data.frame
mean_n_std_features <- grep("-(mean|std)\\(\\)", features[, 2])
length(mean_n_std_features)  #[1] 66

# Subset the desired columns in X_data from 561 column to 66 column
X_data <- X_data[, mean_n_std_features]

# Label the corrected column name in X_data from features 
names(X_data) <- features[mean_n_std_features, 2]


# 3. Use descriptive activity names to name the activities in the data set

activity_labels <- read.table("data/UCI HAR DATASET/activity_labels.txt")

# Update values in y_data set by description from activity_labels
y_data[, 1] <- activity_labels[y_data[, 1], 2]

# Label the column name "activity" and "subject" in subject_data
names(y_data) <- "activity"
names(subject_data) <- "subject"

# 4. Appropriately label the data set with descriptive activity names. 

# Assign labels with descriptive information

names(X_data)<-gsub("^t", "time", names(X_data))
names(X_data)<-gsub("^f", "frequency", names(X_data))
names(X_data)<-gsub("Acc", "Accelerometer", names(X_data))
names(X_data)<-gsub("Gyro", "Gyroscope", names(X_data))
names(X_data)<-gsub("Mag", "Magnitude", names(X_data))
names(X_data)<-gsub("BodyBody", "Body", names(X_data))


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Create a single data set from all 3 data set

single_data <- cbind(X_data, y_data, subject_data)
avg_tidy_data <- ddply(single_data, .(subject, activity), function(x) colMeans(x[, 1:66]))
write.table(avg_tidy_data, "./avg_tidy_data.txt", row.name=FALSE)