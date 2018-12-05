library(dplyr)
library(data.table)


# Read training and test data files, activity and subject identifiers,
# activity and feature labels

data_train <- fread("./UCI HAR Dataset/train/X_train.txt")
data_test <- fread("./UCI HAR Dataset/test/X_test.txt")
activity_test <- fread("./UCI HAR Dataset/test/y_test.txt")
activity_train <- fread("./UCI HAR Dataset/train/y_train.txt")
subject_test <- fread("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- fread("./UCI HAR Dataset/train/subject_train.txt")
activity_labels <- fread("./UCI HAR Dataset/activity_labels.txt")
feature_labels <- fread("./UCI HAR Dataset/features.txt")

# Merge training and test files and attach activity labels

data <- rbind(data_train,data_test)
subject <- rbind(subject_train,subject_test)
activity <- rbind(activity_train,activity_test) %>%
        merge(y = activity_labels,all.x = TRUE)


# Attach subject and activity identifiers and labels to data set and set
# column names

data <- cbind(subject,activity,data)
colnames(data) <- c("SubjectId","ActivityId","ActivityLabel",feature_labels$V2)


# Reduce data set to only mean and standard deviation features

columns <- grep("Subject|Activity|mean|std",colnames(data))
data <- data[,..columns]


# Create aggregated data set with average measurements for each subject
# and activity

data_summary <- data[,lapply(.SD,mean),
                     keyby = .(SubjectId,ActivityId,ActivityLabel)]


# Write tidy data set and summary to files

write.csv(data,"./UCI HAR Dataset/tidydata.csv")
write.csv(data_summary,"./UCI HAR Dataset/tidydata_summary.csv")