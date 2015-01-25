
# Autor: Rene Rangel
# Date : 20-ENE-2015
# Project: Getting and Cleaning Data
# URL Project: https://class.coursera.org/getdata-010/human_grading/view/courses/973497/assessments/3/submissions
# David's Project FAQ
# https://class.coursera.org/getdata-010/forum/thread?thread_id=49#post-185
# Note: before install package: dplyr 
# install.packages("dplyr")
# This code assumes that the working directory is the unzipped file: zip getdata-projectfiles-UCI HAR Dataset
# the dity and summarized data is dataSummarized object
# For cleannig objects from working directory: rm(list = ls())



library(dplyr)



# 1. Merges the training and the test sets to create one data set.
# Read files
# read observations for test
dataTest = read.table("./test/X_test.txt")
# read observations for train
dataTrain = read.table("./train/X_train.txt")
# read subjects for test
dataSubjectTest = read.table("./test/subject_test.txt")
# read subjects for train
dataSubjectTrain = read.table("./train/subject_train.txt")
# read activities for test
dataYTest = read.table("./test/y_test.txt")
# read activities for train
dataYTrain = read.table("./train/y_train.txt")
# merge test data and train data
dataComplete =  rbind(dataTest, dataTrain)



# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# read features
feat = read.table("./feat.txt")
# create vector positions with mean and std measures
colSelMean = feat[grep("mean", feat$V2),1]
colSelStd = feat[grep("std", feat$V2),1]
colSel = c(colSelMean, colSelStd)
datSel = dataComplete[,colSel]
# removes characters specials in the labels
# replace "-" with empty value
feat$V2 = gsub("-", "", feat$V2)
# replace "(" with empty value
feat$V2 = gsub("\\(", "", feat$V2)
# replace ")" with empty value
feat$V2 = gsub(")", "", feat$V2)
# replace "," with empty value
feat$V2 = gsub("\\,", "", feat$V2)



# 4. Appropriately labels the data set with descriptive variable names.
j = 1
for(i in colSel) {
  colnames(datSel)[j] = feat[feat$V1 == i,2]
  j = j + 1
} 
# Merge data selected with other columns -> subjects and activities
datSel = cbind(datSel,rbind(dataSubjectTest,dataSubjectTrain))
datSel = cbind(datSel,rbind(dataYTest,dataYTrain))
# last position rename columns
columnsdatSel = length(colnames(datSel))
colnames(datSel)[columnsdatSel - 1] = "Subject"
colnames(datSel)[columnsdatSel] = "Activity"



# 3.Uses descriptive activity names to name the activities in the data set
datSel[datSel$Activity == 1, columnsdatSel] = "WALKING"
datSel[datSel$Activity == 2, columnsdatSel] = "WALKING_UPSTAIRS"
datSel[datSel$Activity == 3, columnsdatSel] = "WALKING_DOWNSTAIRS"
datSel[datSel$Activity == 4, columnsdatSel] = "SITTING"
datSel[datSel$Activity == 5, columnsdatSel] = "STANDING"
datSel[datSel$Activity == 6, columnsdatSel] = "LAYING"



# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Using library dplyr
dataSummarized <-
datSel %>%
group_by(Subject,Activity) %>%
summarise_each(funs(mean))



# for submmit
write.table(dataSummarized, file="dataSummarized.csv", sep = ",", row.name=FALSE)
    