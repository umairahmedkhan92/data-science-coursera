library(plyr)

# 1 - Merging train and test data

# Reading training data
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")

# Reading test data
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

# Creating data
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject_data <- rbind(subject_train, subject_test)

# 2 - Extracting info on only mean and std of measurements

features <- read.table("features.txt")

# Getting columns which have mean or std in their names
mean_std_features <- grep("-(mean|std)\\(\\)", features[ ,2])

# subsetting the desired data
x_data <- x_data[, mean_std_features]

# Correcting column names
names(x_data) <- features[mean_std_features,2]

# 3 - Descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")

# update names with correct activities
y_data[, 1] <- activities[y_data[, 1], 2]

# Correcting column names
names(y_data) <- "activity"

# 4 - Labeling the data set with descriptive variable names

# Correcting column names
names(subject_data) <- "subject"

# Merging all data in one single dataset
all_data <- cbind(x_data, y_data, subject_data)

# 5 - Independent tidy data set with the average of each variable for each activity and each subject
averages_data <- ddply(all_data, .(subject, activity), function(x) colMeans(x[, 1:66]))

write.table(averages_data, "tidy_data.txt", row.name=FALSE)

