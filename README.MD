### Getting and Cleaning Data (Course Project)
____________________________________________________________________________________________________________________

This is the course assignment on Coursera for Getting and Cleaning Data project. 

`run_analysis.R` does the following steps in order to creates a second, independent tidy data set with the average of each variable for each activity and each subject.

1)	At first the script set the download directory for the file to download. One the download directory is set it download the zip file and unzip it.
2)	Afterwards the scrip loading all the necessary libraries to perform the subsequent tasks.
3)	Then it set the current working directory again.
4)	Then the script loads (train and test) subject, activity and data files.
5)	Once all the file is loaded, then it combines them in one data set.
6)	Afterwards it extracts the necessary columns from the data set (mean and SD for each variables).
7)	Then using the activity names to name the activity numbers.
8)	Once it is done, then the data is labelled with descriptive variable names properly.
9)	Then at the final step mean is calculated for each variable by subject and by activity name.
10)	Finally, the second independent tidy data set is written in flat file called `TidyDataSet.txt`.

