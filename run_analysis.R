################################################################################
# Coursera Getting and Cleaning Data Course Project
# Trish Putnam
# 2014-08-22
#
# file: run_analysis.R
#
# This script is intended to fulfill the following requirements: 
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
#       measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each 
#       variable for each activity and each subject. 
################################################################################

# First, read in the features.txt to get the starting point for column names

features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)

# Next, adjust the column names to something more friendly and consistent

features[,2] = gsub('[-()]', '', features[,2])
features[,2] = gsub("-std$","StdDev",features[,2])
features[,2] = gsub("-mean","Mean",features[,2])
features[,2] = gsub("^(t)","time",features[,2])
features[,2] = gsub("^(f)","freq",features[,2])
features[,2] = gsub("([Gg]ravity)","Gravity",features[,2])
features[,2] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",features[,2])
features[,2] = gsub("[Gg]yro","Gyro",features[,2])


# Read in the data and assign column names for the train dataset

train <- read.table("UCI HAR Dataset/train/X_train.txt")
colnames(train) = features[,2]

train[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep = "", 
        header=FALSE)
colnames(train)[562] = "activity"

train[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep = "", 
        header=FALSE)
colnames(train)[563] = "subject"

# Read in the data and assign column names for the test dataset

test <- read.table("UCI HAR Dataset/test/X_test.txt")
colnames(test) = features[,2]
test[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", 
        header=FALSE)
colnames(test)[562] = "activity"
test[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", 
        header=FALSE)
colnames(test)[563] = "subject"
# Now merge train and test datasets into a single dataset
data = rbind(train, test)


# Reduce dataset to mean and standard deviation only

# Find the columns of interest
cols <- grep(".*mean.*|.*stddev.*", features[,2])

# Use this information to reduce the features data appropriately
features <- features[cols,]

# Add subject and activity columns
cols <- c(cols, 562, 563)

# Now, use this information to reduce the dataset
data <- data[,cols]

#convert the remaining columns to have all lower case headings
colnames(data) <- tolower(colnames(data))

activity.labels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", 
        header=FALSE)

# Replace the numeric activity id with a friendly version
i = 1
for (label in activity.labels$V2) {
        data$activity <- gsub(i, label, data$activity)                               
        i <- i + 1
}

#Create final tidy dataset
data$activity <- as.factor(data$activity)
data$subject <- as.factor(data$subject)

tidyData <- aggregate(data[,names(data) != c('activity','subject')],by=list(activity=data$activity, subject=data$subject),mean)

# Write out the tidy data table without row names

write.table(tidyData, "tidy.txt", sep="\t", row.name=FALSE)