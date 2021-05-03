## run_analysis.R
## Author: tf
## Date: 04/20/2021
## 
## This script does transformations based on the
## Human Activity Recognition Using Smartphones Dataset
## and provides two data sets: 
## harus_means_sd: Measurements on the mean and standard deviation for each measurement
## harus_averages: Average of each variable for each activity and each subject

## Libraries used
library(dplyr)
library(tibble)

## Read the data

# Read the labels
labels_features_ds <- read.csv("./UCI HAR Dataset/features.txt", sep = " ", header = FALSE)
# Extract 2nd column as vector
labels_features <- labels_features_ds[[2]]
# Fix duplicate labels
labels_features[303:316] <- sub("fBodyAcc-bandsEnergy","fBodyAcc-bandsEnergy1",labels_features[303:316])
labels_features[317:330] <- sub("fBodyAcc-bandsEnergy","fBodyAcc-bandsEnergy2",labels_features[317:330])
labels_features[331:344] <- sub("fBodyAcc-bandsEnergy","fBodyAcc-bandsEnergy3",labels_features[331:344])
labels_features[382:395] <- sub("fBodyAccJerk-bandsEnergy","fBodyAccJerk-bandsEnergy1",labels_features[382:395])
labels_features[396:409] <- sub("fBodyAccJerk-bandsEnergy","fBodyAccJerk-bandsEnergy2",labels_features[396:409])
labels_features[410:423] <- sub("fBodyAccJerk-bandsEnergy","fBodyAccJerk-bandsEnergy3",labels_features[410:423])
labels_features[461:474] <- sub("fBodyGyro-bandsEnergy","fBodyGyro-bandsEnergy1",labels_features[461:474])
labels_features[475:488] <- sub("fBodyGyro-bandsEnergy","fBodyGyro-bandsEnergy2",labels_features[475:488])
labels_features[489:502] <- sub("fBodyGyro-bandsEnergy","fBodyGyro-bandsEnergy3",labels_features[489:502])

# Lengths of fixed fields for data
width_datafields <- rep(c(-1,15),times=561)

## Test data
# Read test data
xtest <- read.fwf("./UCI HAR Dataset/test/X_test.txt", widths = width_datafields, header = FALSE, skip = 0)

# Assign the labels to xtest column names
colnames(xtest) <- labels_features

# Read subjects for test data
xtest_subjects <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# Read activities for test data
xtest_activities <- read.csv("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

# Data frames to tibbles
xtest_tbl <- as_tibble(xtest)
xtest_subjects_tbl <- as_tibble(xtest_subjects)
xtest_activities_tbl <- as_tibble(xtest_activities)

# Add test subjects column
xtest_tbl <- xtest_tbl %>% add_column(xtest_subjects_tbl)
# Rename column
colnames(xtest_tbl)[562] <- c("subject")

# Add activities column
xtest_tbl <- xtest_tbl %>% add_column(xtest_activities_tbl)
# Rename column
colnames(xtest_tbl)[563] <- c("activity")


## Training data
# Read training data
xtrain <- read.fwf("./UCI HAR Dataset/train/X_train.txt", widths = width_datafields, header = FALSE, skip = 0)

# Assign the labels to xtrain column names
colnames(xtrain) <- labels_features

# Read subjects for train data
xtrain_subjects <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# Read activities for train data
xtrain_activities <- read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Data frames to tibbles
xtrain_tbl <- as_tibble(xtrain)
xtrain_subjects_tbl <- as_tibble(xtrain_subjects)
xtrain_activities_tbl <- as_tibble(xtrain_activities)

# Add train subjects column
xtrain_tbl <- xtrain_tbl %>% add_column(xtrain_subjects_tbl)
# Rename column
colnames(xtrain_tbl)[562] <- c("subject")

# Add activities column
xtrain_tbl <- xtrain_tbl %>% add_column(xtrain_activities_tbl)
# Rename column
colnames(xtrain_tbl)[563] <- c("activity")

## Join test and train tibbles
x_tbl <- bind_rows(xtest_tbl,xtrain_tbl)

## Extract only the measurements on the mean and standard deviation for each measurement
# Check for mean and std
select_mean <- grep("-mean()", x_names)
select_std <- grep("-std()", x_names)
# Combine in one vector and add activity and subject
select_mean_std <- c(select_mean, select_std, c(562,563))
# Sort vector
select_mean_std <- sort(select_mean_std)
# Subset x_tbl
x_tbl_mean_std <- x_tbl %>% select(select_mean_std)

## Uses descriptive activity names to name the activities in the data set
x_tbl_mean_std_rc <- x_tbl_mean_std %>% mutate(activity=recode(activity, `1`="WALKING", `2`="WALKING_UPSTAIRS", `3`="WALKING_DOWNSTAIRS", `4`="SITTING", `5`="STANDING", `6`="LAYING"))

## Appropriately labels the data set with descriptive variable names
# Replace brackets
names(x_tbl_mean_std_rc) <- gsub("[()]","",names(x_tbl_mean_std_rc))
# Replace strings
names(x_tbl_mean_std_rc) <- sub("^t","time",names(x_tbl_mean_std_rc))
names(x_tbl_mean_std_rc) <- sub("^f","frequncy",names(x_tbl_mean_std_rc))
names(x_tbl_mean_std_rc) <- sub("Mag","magnitude",names(x_tbl_mean_std_rc))
names(x_tbl_mean_std_rc) <- sub("Acc","acceleration",names(x_tbl_mean_std_rc))
names(x_tbl_mean_std_rc) <- sub("Freq","frequency",names(x_tbl_mean_std_rc))
# Replace dash
names(x_tbl_mean_std_rc) <- gsub("-","",names(x_tbl_mean_std_rc))
# Lower case
names(x_tbl_mean_std_rc) <- tolower(names(x_tbl_mean_std_rc))

# Resulting data set: x_tbl_mean_std_rc

## Data set with the average of each variable for each activity and each subject.
# Group and calculate mean
x_grouped_means <- x_tbl_mean_std_rc %>% group_by(subject, activity) %>% summarise(across(everything(), list(mean)))
# Adapt labels
names(x_grouped_means) <- sub("_1","",names(x_grouped_means))
names(x_grouped_means) <- paste(c("mean"),names(x_grouped_means), sep = "")
names(x_grouped_means)[1] <- "subject"
names(x_grouped_means)[2] <- "activity"

# Write table
write.table(x_grouped_means,file="x_grouped_means.txt", row.names = FALSE)

# Resulting data set: x_grouped_means
