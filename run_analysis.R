# STEP 0 - We need to download the data and read it.
# Download data

DL.data <- function (){
  if (!file.exists("data")) {
    message("Creating Data folder in working directory")
    dir.create("data")
  }
  if(!file.exists("data/UCI HAR Dataset")) {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                  , destfile = "./data/UCI_HAR_data.zip")
    unzip("./data/UCI_HAR_data.zip", exdir = "data")
    unlink("./data/UCI_HAR_data.zip")
  }
  else message("data already exists")
}

DL.data()

# Read the data

# Features
features <- read.table("./data/UCI HAR Dataset/features.txt")
# Test data
tst_x <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features[,2])
tst_y <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = c("Activity"))
subject_tst <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = c("Subject"))
# Training data
trn_x <- read.table("./data/UCI HAR Dataset/train/X_train.txt", col.names = features[,2])
trn_y <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = c("Activity"))
subject_trn <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = c("Subject"))

# STEP 1 - Merges the training and the test sets to create one data set.

tst <- cbind(tst_x, tst_y, subject_tst)
trn <- cbind(trn_x, trn_y, subject_trn)
data <- rbind(tst, trn)

# STEP 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
# Using matchcols we can select only the columns with "mean" or"std" in their names

cols <- matchcols(data, with=c("mean", "std"), method = "or")
data <- data[,c("Activity","Subject", cols$mean, cols$std)]


# STEP 3 -  Uses descriptive activity names to name the activities in the data set.
# We add one column with the label of the activity

activity <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
names(activity) <- c("Activity","Label")
data <- merge( data, activity, by = "Activity")

# STEP 4 -  Appropriately labels the data set with descriptive activity names. 

names <- names(data)
namess <- c("Activity","Subject","Label")

for( i in names ){
  if( ! i %in% namess ) {
    x <- gsub("^t", "TimeDomain", i)
    x <- gsub("^f", "FrequencyDomain", x)
    x <- gsub(".std", "StandardDeviation", x)
    x <- gsub(".mean", "Mean", x)
    x <- gsub("-", "", x)
    x <- gsub("Acc", "LinearAcceleration", x)
    x <- gsub("Gyro", "AngularVelocity", x)
    names(data)[names(data)==i] <- x
  }
}

## STEP 5 - Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidym <- setdiff(colnames(data), namess)
tidy <- melt(data, id = namess, measure.vars = tidym)
tidy <- dcast(tidy, Activity + Subject ~ variable, mean)
write.table(tidy, file="tidy.txt", sep = "|")

