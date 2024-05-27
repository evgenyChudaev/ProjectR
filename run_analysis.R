library(readr)
library(dplyr)


#getwd()


## unzip the folder with the data
#unzip("projectfile.zip")

#setwd("./UCI HAR Dataset")

## check list of files and folders
##list.files()

## import X train
x_train <- read.table("./train/X_train.txt",  header=FALSE)
##names(x_train)
##dim(x_train)

## import X test
x_test <- read.table("./test/X_test.txt", header=FALSE)
##names(x_test)
##dim(x_test)

## Requirement 1: concatenate X train and X test to merge training and test sets in 1 dataset
x_combined <- rbind(x_train, x_test)
##dim(x_combined)

## Assign labels from the file
feature_names <- read.table("./features.txt", header=FALSE)
##feature_names$V2

## Requirement 4: Appropriately label the data set with descriptive variable names. 
## Assign feature names to columns
names(x_combined) <- feature_names$V2

## import Y train
y_train <- read.table("./train/y_train.txt",  header=FALSE)
##names(y_train)
##dim(y_train)

## import Y test
y_test <- read.table("./test/y_test.txt", header=FALSE)
##names(y_test)
##dim(y_test)

## concatenate labels
y_combined <- rbind(y_train, y_test)
##dim(y_combined)
names(y_combined) <- "y_label"


## Add subject info from subject test/train datasets
subject_train <- read.table("./train/subject_train.txt",  header=FALSE, col.names=c("subject"))
subject_test  <- read.table("./test/subject_test.txt", header=FALSE, col.names=c("subject"))
## combine subject train/test
subjects_combined <- rbind(subject_train, subject_test)
##dim(subjects_combined)


## Requirement 3: Use descriptive activity names to name the activities in the data set
## by adding activity label definitions from the activity_labels.txt 
activity_labels <- read.table("./activity_labels.txt", header=FALSE, col.names=c("id", "activity"))


## bind label columns to the feature columns
xy_combined <- cbind(x_combined, y_combined)
##dim(xy_combined)

# bind subjects dataset to the feature and label dataset
xy_combined <- cbind(xy_combined, subjects_combined)
##dim(xy_combined)

## merge features and labels dataset with the activity name definitions
activities_all <- merge(xy_combined, activity_labels, by.x = "y_label", by.y = "id", all.x = TRUE)

##Requirement 2: Extract only the measurements on the mean and standard deviation for each measurement.
mean_sd_measures <- grep("-(mean|std)[()]",feature_names$V2, value=TRUE)
mean_sd_measures <- c(mean_sd_measures, "activity", "subject")
activities_mean_std <- select(activities_all, all_of(mean_sd_measures))


## Requirement 5: Creates a second, independent tidy data set with 
##the average of each variable for each activity and each subject.
##str(activities_mean_std)

grouped_df <- activities_mean_std %>%
  group_by(activity, subject) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE ))

grouped_df

## export data from step 5
##write.table(grouped_df, file = "summary.txt",row.name=FALSE)

## reimport and check the summary file
##summary <- read.table("./summary.txt", header = TRUE)