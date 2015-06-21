library(plyr)

# Step 1
# Merges the training and the test sets to create one data set.
########################################################################

# Read the Activity files
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")

# Read the subjec files
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Read the subjec files
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")

# create 'x' data set and set names to the variables
x_data <- rbind(x_train, x_test)
features <- read.table("UCI HAR Dataset/features.txt")
names(x_data)<-features$V2

# create 'y' data set and set names to the variables
y_data <- rbind(y_train, y_test)
names(y_data)<-c("activity")

# create 'subject' data set and set names to the variables
subject_data <- rbind(subject_train, subject_test)
names(subject_data)<-c("subject")

# Raw Data
RawData <- cbind(subject_data,  y_data, x_data)

# housecleaning
rm(x_test,y_test,x_train,y_train,subject_train,subject_test,x_data,y_data,subject_data)

# Step 2
# Extract only the measurements on the mean and standard deviation for
# each measurement
######################################################################

subfeatures<- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]
selected<-c(as.character(subfeatures), "subject", "activity" )
subData<-subset(RawData, select=selected)

# Step 3
# Uses descriptive activity names to name the activities in the data
# set
######################################################################

activities <- read.table("UCI HAR Dataset/activity_labels.txt",header=FALSE,colClasses="character")
activities <- apply(activities, 1, function(x) unlist(strsplit(x, split=" ")))
subData[,68] <- factor(as.factor(subData[,68]), labels=activities[2,])

# Step 4
# Appropriately labels the data set with descriptive variable names.
######################################################################

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

# Step 5
# From the data set in step 4, creates a second, independent tidy data
# set with the average of each variable for each activity and each subject.
######################################################################

library(plyr)
TidyData<-aggregate(. ~subject + activity, subData, mean)
TidyData<-TidyData[order(TidyData$subject,TidyData$activity),]
write.table(TidyData, file = "AverageData.txt",row.name=FALSE)

