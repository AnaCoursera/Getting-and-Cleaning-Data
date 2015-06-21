Code Book
=====

Data Information: 
--------

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 


The dataset includes the following files:

1. 'features.txt': List of all features.
2. 'activity_labels.txt': Links the class labels with their activity name.
3. 'train/X_train.txt': Training set.
4. 'train/y_train.txt': Training labels.
5. 'test/X_test.txt': Test set.
6. 'test/y_test.txt': Test labels. 
7. 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.  
8. 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

Variables Description: 
--------

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

- mean(): Mean value
- std(): Standard deviation

1. Merges the training and the test sets to create one data set.
--------

After reading all files into tables and assign column names. 

-  Read the Activity files
```{r}
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
```

-  Read the subjec files
```{r}
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
```

- Read the subjec files
```{r}
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
```

- create 'x' data set and set names to the variables
```{r}
x_data <- rbind(x_train, x_test)
features <- read.table("UCI HAR Dataset/features.txt")
names(x_data)<-features$V2
```

- create 'y' data set and set names to the variables
```{r}
y_data <- rbind(y_train, y_test)
names(y_data)<-c("activity")
```

- create 'subject' data set and set names to the variables
```{r}
subject_data <- rbind(subject_train, subject_test)
names(subject_data)<-c("subject")
```

Merge to create one data set.
```{r}
RawData <- cbind(subject_data,  y_data, x_data)
```
2. Extract only the measurements on the mean and standard deviation for each measurement.
--------
Make a subset with all the variables that contain mean and std in the name.
```{r}
subfeatures<- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
selected<-c(as.character(subfeatures), "subject", "activity" )
subData<-subset(RawData, select=selected)
```

3. Uses descriptive activity names to name the activities in the data set.
--------
Merge the data subset with the activity_labels table to cinlude the descriptive activity names.
```{r}
activities <- read.table("UCI HAR Dataset/activity_labels.txt",header=FALSE,colClasses="character")
activities <- apply(activities, 1, function(x) unlist(strsplit(x, split=" ")))
subData[,68] <- factor(as.factor(subData[,68]), labels=activities[2,])
```

4. Appropriately labels the data set with descriptive variable names.
--------
Make understandable the variables using the gsun function
```{r}
names(subData) <- gsub('\\(|\\)',"",names(subData), perl = TRUE)
names(subData)<-gsub("std", "SD", names(subData))
names(subData)<-gsub("mean", "MEAN", names(subData))
names(subData)<-gsub("^t", "Time", names(subData))
names(subData)<-gsub("Acc", "Accelerometer", names(subData))
names(subData)<-gsub("Gyro", "Gyroscope", names(subData))
names(subData)<-gsub("Mag", "Magnitude", names(subData))
names(subData)<-gsub("^f", "Frequency", names(subData))
names(subData)<-gsub("BodyBody", "Body", names(subData))
View(subData)
```

5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
--------

Create a second data set with the average of each variable for each activity and each subject.
```{r}
library(plyr)
TidyData<-aggregate(. ~subject + activity, subData, mean)
TidyData<-TidyData[order(TidyData$subject,TidyData$activity),]
write.table(TidyData, file = "AverageData.txt",row.name=FALSE)
```