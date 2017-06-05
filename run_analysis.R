##Getting and Cleaning Data Course Project

#Set donwload directory
  setwd("C:\\Users\\arunr\\OneDrive\\OnlineDoc\\Data Science\\R\\Course 3\\Assignment 4\\Assignment")

##Downloading and Unzip the file
  fileurl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  download.file(fileurl,"FUCI_DataSet.zip")
  unzip("FUCI_DataSet.zip")

##Activate all the libraries for this session
  library(dplyr)
  library(data.table)
  library(tidyr)

##Set working directory
  setwd("C:\\Users\\arunr\\OneDrive\\OnlineDoc\\Data Science\\R\\Course 3\\Assignment 4\\Assignment\\UCI HAR Dataset")


# 1. Merges the training and the test sets to create one data set.

#Reading subject files
  dataSubjectTrain <- tbl_df(read.table(file.path("./","train","subject_train.txt")))
  dataSubjectTest <- tbl_df(read.table(file.path("./","test","subject_test.txt")))

#Reading activity files
  dataActivityTrain <- tbl_df(read.table(file.path("./","train","y_train.txt")))
  dataActivityTest <- tbl_df(read.table(file.path("./","test","y_test.txt")))

#Reading data files
  dataTrain <-  tbl_df(read.table(file.path("./","train","X_train.txt")))
  dataTest <-  tbl_df(read.table(file.path("./","test","X_test.txt")))

#Combining Subject test and train dataSets
  DataSubjectAll <- rbind(dataSubjectTrain,dataSubjectTest)
  setnames(DataSubjectAll, "V1", "subject")

#Combining Activity test and train dataSets
  DataActivityAll <- rbind(dataActivityTrain,dataActivityTest)
  setnames(DataActivityAll,"V1","ActivityNum")

#Data sets from X and naming them properly
  dataSets <- rbind(dataTrain,dataTest)
  dataFeature <- tbl_df(read.table(file.path("./","features.txt")))
  setnames(dataFeature,names(dataFeature),c("FeatureNum","FeatureName"))
  colnames(dataSets) <- dataFeature$FeatureName

#merging datasets together 
  DataSubAct <- cbind(DataSubjectAll,DataActivityAll)
  DataAll <- cbind(dataSets,DataSubAct)



## 2 . Extracts only the measurements on the mean and standard deviation for each measurement. 
  ColNames <- colnames(DataAll)
  MeanSD <- (grepl("subject", ColNames) |
              grepl("ActivityNum", ColNames) |
              grepl("mean..", ColNames) |
              grepl("std..", ColNames))
  ExtractMeanSD <- DataAll[, MeanSD == TRUE]

##3.Uses descriptive activity names to name the activities in the data set

#COlumn names for activtiy labes
  ActivityLabel <- tbl_df(read.table(file.path("./","activity_labels.txt")))
  setnames(ActivityLabel,names(ActivityLabel),c("ActivityNum","ActivityName"))
  
  ExtractMeanSD <- merge(ActivityLabel, ExtractMeanSD, by = "ActivityNum", all.x = TRUE)
  ExtractMeanSD$ActivityName <- as.character(ExtractMeanSD $ActivityName)

##4.Appropriately labels the data set with descriptive variable names. 
  
  names(ExtractMeanSD)<-gsub("std()", "SD", names(ExtractMeanSD))
  names(ExtractMeanSD)<-gsub("mean()", "MEAN", names(ExtractMeanSD))
  names(ExtractMeanSD)<-gsub("^t", "time", names(ExtractMeanSD))
  names(ExtractMeanSD)<-gsub("^f", "frequency", names(ExtractMeanSD))
  names(ExtractMeanSD)<-gsub("Acc", "Accelerometer", names(ExtractMeanSD))
  names(ExtractMeanSD)<-gsub("Gyro", "Gyroscope", names(ExtractMeanSD))
  names(ExtractMeanSD)<-gsub("Mag", "Magnitude", names(ExtractMeanSD))
  names(ExtractMeanSD)<-gsub("BodyBody", "Body", names(ExtractMeanSD))

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

  ExtractMeanSD$ActivityName <- as.character(ExtractMeanSD$ActivityName)
  dataAggr<- aggregate(. ~ subject - ActivityName, data = ExtractMeanSD, mean) 
  ExtractMeanSD<- tbl_df(arrange(dataAggr,subject,ActivityName))
  write.table(ExtractMeanSD, "TidyDataSet.txt", row.names = FALSE)

