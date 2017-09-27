# title: "Getting and Cleaning Data Course Project"
# author: "Jason McKenna"
# date: "September 26, 2017"
# 
# This R script (run_analysis.R) does the following:
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
#    variable for each activity and each subject.
#
#
## Begin Program   
#
## Step 0: Load the necessary library(ies) and download the zip file containing the data for analysis 
# (but first check to see if the data has been downloaded). Then read in the data
#
#
install.packages(dplyr)
library(dplyr)
#
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#
dataFile <- "getdata_projectfiles_UCI HAR Dataset.zip"
#
if (!file.exists(dataFile)) { download.file(dataUrl, dataFile, mode = "wb")}
#
# If data directory doesn't already exist, unzip file and create directory structure:
#
dirName <- "UCI HAR Dataset"
#
if (!file.exists(dirName)) {unzip(dataFile)}
#
# Read the Training data:
#
trainingSubjects <- read.table(file.path(dirName, "train", "subject_train.txt"))
#
trainingValues <- read.table(file.path(dirName, "train", "X_train.txt"))
#
trainingActivity <- read.table(file.path(dirName, "train", "y_train.txt"))
#
# Read the Test data:
#
testSubjects <- read.table(file.path(dirName, "test", "subject_test.txt"))
#
testValues <- read.table(file.path(dirName, "test", "X_test.txt"))
#
testActivity <- read.table(file.path(dirName, "test", "y_test.txt"))
#
# Read the Features data:
#
features <- read.table(file.path(dirName, "features.txt"), as.is = TRUE)
#
#Read the Activity Labels data:
#
activities <- read.table(file.path(dirName, "activity_labels.txt"))
#
# Provide new column labels:
colnames(activities) <- c("activityId", "activityLabel")
#
#
##Step 1: Merge the training and the test sets to create one data set
#
# Merge individual data tables to create a single data table (HAR) comprised of training and testing data:
#
HAR <- rbind(cbind(trainingSubjects, trainingValues, trainingActivity), cbind(testSubjects, testValues, testActivity))
#
# Assign column names
#
colnames(HAR) <- c("subject", features[, 2], "activity")
#
#
## Step 2: Extract the mean and standard deviation for each measurement
#
# Determine the variables to keep based on column name & update working data table:
#
columnsKept <- grepl("subject|activity|mean|std", colnames(HAR))
#
HAR <- HAR[, columnsKept]
#
#
## Step 3: Re-label activities using "descriptive" names
#
# Replace activity values with named factor levels:
#
HAR$activity <- factor(HAR$activity, levels = activities[, 1], labels = activities[, 2])
#
#
## Step 4: Appropriately label the data set with descriptive variable names
#
# Obtain column names:
#
HARCols <- colnames(HAR)
#
# Remove all special characters in variable names
#
HARCols <- gsub("[\\(\\)-]", "", HARCols)
#
# Remove anyd vaviable abbreviations:
#
HARCols <- gsub("^f", "frequencyDomain", HARCols)
#
HARCols <- gsub("^t", "timeDomain", HARCols)
#
HARCols <- gsub("Acc", "Accelerometer", HARCols)
#
HARCols <- gsub("Gyro", "Gyroscope", HARCols)
#
HARCols <- gsub("Mag", "Magnitude", HARCols)
#
HARCols <- gsub("Freq", "Frequency", HARCols)
#
HARCols <- gsub("mean", "Mean", HARCols)
#
HARCols <- gsub("std", "standardDeviation", HARCols)
#
# Correct observed typo:
#
HARCols <- gsub("BodyBody", "Body", HARCols)
#
# Use new labels as column names:
#
colnames(HAR) <- HARCols
#
#
## Step 5: Create a second, independent tidy set with the average of each variable for each activity and each subject
#
# Group by subject/activity & summarize the data using the mean of each variable: 
#
HARMeans <- HAR %>% 
            group_by(subject, activity) %>%
            summarise_all(funs(mean))
#
# Output the tidy data set:
#
write.table(HARMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)
#
## End of program
