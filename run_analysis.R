# Name: run_analysis.R
# Purpose: Collects and cleans data from the Samsung data set as part of the final
#          project in the "Getting and Cleaning Data" class in John Hopkins
#          University. The script does the following:
#          1. Merges the training and the test sets to create one data set.
#          2. Extracts only the measurements on the mean and standard deviation for 
#             each measurement. 
#          3. Uses descriptive activity names to name the activities in the data set
#          4. Appropriately labels the data set with descriptive variable names. 
#          5. From the data set in step 4, creates a second, independent tidy data set
#              with the average of each variable for each activity and each subject.
#          Notes: 1. Script assumes dataset is in the current working directory.
#                 2. Dataset: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#          
# Arguments: None
# Output: A second, independent tidy data set with the average of
# each variable for each activity and each subject.

## 1. Merge the training and the test sets to create one data set

    # Read train dataset
    xTrain <- read.table("train/X_train.txt", header = FALSE, sep = "", colClasses = rep("numeric", 561))
    subjectTrain <- read.table("train/subject_train.txt", header = FALSE, sep = "")
    yTrain <- read.table("train/y_train.txt", header = FALSE, sep = "")
    # Read test dataset
    xTest <- read.table("test/X_test.txt", header = FALSE, sep = "", colClasses = rep("numeric", 561))
    subjectTest <- read.table("test/subject_test.txt", header = FALSE, sep = "")
    yTest <- read.table("test/y_test.txt", header = FALSE, sep = "")
    # Merge two datasets and store result in variable "data"
    dataTrain <- cbind(subjectTrain, xTrain, yTrain)
    dataTest <- cbind(subjectTest, xTest, yTest)
    data <- rbind(dataTrain, dataTest)
    
## 2. Extract only the measurements on the mean and standard deviation for 
## each measurement. 

    features <- read.table("features.txt", header=FALSE, colClasses = c("integer", "character"), sep = "", col.names = c("index", "featureName"))
    relevantFeaturesIndices = union(grep("mean()", features$featureName, fixed = TRUE), grep("std()", features$featureName, fixed = TRUE))
    relevantFeatures <- features[relevantFeaturesIndices, ]
    relevantData <- data[, c(1, relevantFeaturesIndices + 1, 563)]
    
## 3. Use descriptive activity names to name the activities in the data set

    numCols <- length(relevantFeaturesIndices) + 2
    activityLabels <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
    relevantData[, numCols] <- factor(relevantData[, numCols], labels = activityLabels)

## 4. Appropriately label the data set with descriptive variable names

    colnames(relevantData) <- c("subject", relevantFeatures[, 2], "activity")
    
## 5. From the data set in step 4, creates a second, independent tidy data
## set with the average of each variable for each activity and each subject.
    
    # We use the split, apply and combine strategy
    splitData <- split(relevantData, list(factor(relevantData[, 1], labels = 1:30), relevantData[, numCols]))
    newData <- as.data.frame(t(sapply(splitData, function(x) sapply(x, function(x) mean(unclass(x))))))
    newData$subject <- as.integer(newData$subject)
    newData$activity <- factor(newData$activity, labels = activityLabels)
    # Sort dataset by subject followed by activity
    newData <- newData[order(newData$subject, newData$activity), ]
    print(newData)