# read feature and activity labels
features <- read.table("UCI HAR Dataset/features.txt")
names(features) <- c("id", "name")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
names(activity_labels) <- c("id", "name")

# read test data 

test_data = read.table("UCI HAR Dataset/test//X_test.txt")
test_subject = read.table("UCI HAR Dataset/test//subject_test.txt")
test_data = cbind(test_data, subject = test_subject)
test_activity = read.table("UCI HAR Dataset/test//y_test.txt")
test_data = cbind(test_data, activity = test_activity)


# read training data
train_data = read.table("UCI HAR Dataset/train//X_train.txt")
train_subject = read.table("UCI HAR Dataset/train//subject_train.txt")
train_data = cbind(train_data, train_subject)
train_activity = read.table("UCI HAR Dataset/train//y_train.txt")
train_data = cbind(train_data, train_activity)

# merge test and training data
total_data = rbind(test_data, train_data)

# We extract the names of features which correspond to mean and standard deviation
features_tidy = features[grep("mean\\(\\)|std\\(\\)", features$name),]

# l\Let us filter the total_data by the required features. From the above data we get the ids
# our tidy features. We also add our activity and subject columns that we have appended to
# our data set.
pre_tidy_data = total_data[, c(features_tidy$id, 562, 563)]

# set the column name of the data frame
names(pre_tidy_data) = c(as.character(features_tidy$name), "subject", "activity")

# Now we will tidy the data set. We see that each row of the dataset contains multiple 
# observations. We can use the melt function to tidy the data set/
library(reshape2)

# We will add a variable which will keep the observation id to help as decast later.
tidy_data$id <- row.names(tidy_data)
tidy_data <- melt(pre_tidy_data, id.vars=c("activity", "subject", "id"), variable.name="measurement")

# The above data set can be considered tidy for most purposes although some would argue that
# mean and standard deviation are variables of a single observation set and hence should be 
# in a single row. We can achieve this via regex kungfu. 

tidy_data$observation = gsub("-(mean|std)\\(\\)", "", tidy_data$measurement)
tidy_data$type = gsub(".*-(mean|std)\\(\\).*", "\\1", tidy_data$measurement)

# Now we need to do some casting get this data into its final form before the 
# analysis can be done
tidy_data = dcast(tidy_data, activity + subject + observation + id ~ type)

# remove columns no longer necessary
tidy_data$measurement = NULL
tidy_data$id = NULL

# factor the activity column so that it is readable
tidy_data$activity = factor(pre_tidy_data$activity, labels=activity_labels$name)

# We will store this into a text file.
write.table(tidy_data, file="tidy_data.txt", col.names=TRUE, row.names=FALSE)

# We can also calculate the average of every variable per subject and per activity
tidy_data_aggregate = aggregate(cbind(mean, std) ~ subject + activity, data=tidy_data, FUN=mean)

write.table(tidy_data_aggregate, file="tidy_data_agg.txt", col.names=TRUE, row.names=FALSE)

