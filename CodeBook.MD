#Getting and Cleaning Data Project - Part 3 of the Data Scientist Series

##Description

This code book contains details about the data and transformations used in the Getting and Cleaning Data course project.

Data Source (taken from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

##Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

##Attribute Information:

For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

##R Program Operation

The program operation is as follows:

###Step 1 - Merge the training and the test sets to create one data set.

The working directory is set at the level containing the 'UCI HAR Dataset' folder.

Access the following files:

UCI HAR Dataset/train/X_train.txt
UCI HAR Dataset/test/X_test.txt
UCI HAR Dataset/train/subject_train.txt
UCI HAR Dataset/test/subject_test.txt
UCI HAR Dataset/train/y_train.txt
UCI HAR Dataset/test/y_test.txt

and combine into relevant data sets.

Read in the _UCI HAR Dataset/features.txt_ file to extract the list of standard deviation and mean data attributes.

Read in the _UCI HAR Dataset/activity_labels.txt_ file to extract the list of activity labels.

Assign column names and merge to create a single data set.

###Step 2 - Extract only the measurements on the mean and standard deviation for each measurement. 

Make use of a logcal vector to contain TRUE for the ID, mean and stdev attributes and FALSE for all others. Perform a subset to retain only the required attributes from the original data set.

###Step 3 - Use descriptive activity names to name the activities in the data set

Merge the data subset from step 3 with the activityType table to include the descriptive activity names for the data.

###Step 4 - Appropriately label the data set with descriptive variable names.

Use the gsub function tidy the data labels.

###Step 5 - From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

As requested the final data set should only contain data set with the average of each veriable for each activity and subject so utilise a loop within a loop to process through the full set of available Subjects / Activities to calculate these averages.
