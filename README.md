---
title: "README"
author: "Guy Cohen"
date: "June 18, 2015"
output: html_document
---

## Project Description

This project is the final project in the "Getting and Cleaning Data" course at John Hopkins University. The "Getting and Cleaning Data" course is part of the Data Science Specialization (series of 9 courses + final project) at John Hopkins University.

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

 You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Good luck!

## Project Solution


To achieve the project goals, I wrote a single script file called run_analysis.R . This script file implements the 5 stages stated in the project description.

1. The script merges the training and test sets to one data set. It does so by reading the files "train/X_train.txt", "train/subject_train.txt" and "train/y_train.txt" files for the train set, which respectively contain a data frame with 561 features for each example, a vector the subject numbers for each example and the activity number for each example (1 - WALKING, 2 - WALKING_UPSTAIRS, 3 - WALKING_DOWNSTAIRS, 4 - SITTING,
5 - STANDING, 6 - LAYING). Similarly, the script reads the test set files named "test/X_test.txt", "test/subject_test.txt" and "test/y_test.txt" . It then merges the train data and test data to one data frame called "data," in which the first column contains the subject number, the following columns contain the features, and the last column contains the activity type.
2. The script then extracts only the measurements on the mean and standard deviation for each measurement. It does so by reading the features.txt file, which lists all 561 features, and keeping feature names that either contain either "mean()" or "std()". Once the script has the list of relevant features, it subsets the "data" data frame to extract only the relevant features to a new data frame called "relevantData."
3. The script then uses descriptive activity names to name the activities in the data set. It replaces the numbers 1,2,...6 in the "activity" column with WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, and LAYING, respectively.
4. The script then appropriately labels the data set with descriptive variable names. The script writes the names of relevant features from features.txt to the column names of the "relevantFeatures" data frame.
5. The script then creates a second, independent tidy data set with the average of each variable for each activity and each subject. It does so by applying the split, apply and combine method using the split() and sapply() functions.

