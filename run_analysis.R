## load plyr, dplyr, and data.table packages
library(plyr)
library(dplyr)
library(data.table)

## read the test data from text files located in subdirectories of the working directory
s.test <- read.table("./UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = FALSE)
y.test <- read.table("./UCI HAR Dataset/test/y_test.txt", stringsAsFactors = FALSE)
x.test <- read.table("./UCI HAR Dataset/test/X_test.txt", stringsAsFactors = FALSE)

s.train <- read.table("./UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = FALSE)
y.train <- read.table("./UCI HAR Dataset/train/y_train.txt", stringsAsFactors = FALSE)
x.train <- read.table("./UCI HAR Dataset/train/X_train.txt", stringsAsFactors = FALSE)

## add subject (s.test/train) and activity (y.test/train) columns to the x. data frames
yx.test <- cbind(y.test, x.test)
syx.test <- cbind(s.test, yx.test)

yx.train <-cbind(y.train, x.train)
syx.train <-cbind(s.train, yx.train)

## appropriately rename the first 2 columns of the resulting test and train data frames
colnames(syx.test)[1:2] <- c("subject", "activity")
colnames(syx.train)[1:2] <- c("subject", "activity")

## combine the test and train data frames
c.data <- rbind(syx.train, syx.test)

## convert c.data to a dplyr data frame table and extract the mean and standard
## deviation columns for each main measurement category along with
## the subject and activity columns
dft.data <- tbl_df(c.data)
ms.data <- select(dft.data, subject, activity, V1, V2, V3, V4, V5, V6, V41, V42, V43,
                  V44, V45, V46, V81, V82, V83, V84, V85, V86, V121, V122, V123, V124,
                  V125, V126, V161, V162, V163, V164, V165, V166, V201, V202, V214,
                  V215, V227, V228, V240, V241, V253, V254, V266, V267, V268, V269,
                  V270, V271, V345, V346, V347, V348, V349, V350, V424, V425, V426,
                  V427, V428, V429, V503, V504, V516, V517, V529, V530, V542, V543)


## convert activity column numbers to characters and replace with activity description
ms.data <- mutate(ms.data, activity = as.character(activity))
ms.data$activity <- revalue(ms.data$activity, c("1" = "walking", "2" = "walking_up_stairs",
                            "3" = "walking_down_stairs", "4" = "sitting", "5" = "standing",
                            "6" = "laying"))                                
colnames(ms.data) <- c("subject", "activity", "tBodyAcc_mean_X", "tBodyAcc_mean_Y",
                       "tBodyAcc_mean_Z", "tBodyAcc_std_X", "tBodyAcc_std_Y",
                       "tBodyAcc_std_Z", "tGravityAcc_mean_X", "tGravityAcc_mean_Y",
                       "tGravityAcc_mean_Z", "tGravityAcc_std_X",
                       "tGravityAcc_std_Y", "tGravityAcc_std_Z",
                       "tBodyAccJerk_mean_X", "tBodyAccJerk_mean_Y",
                       "tBodyAccJerk_mean_Z", "tBodyAccJerk_std_X",
                       "tBodyAccJerk_std_Y", "tBodyAccJerk_std_Z",
                       "tBodyGyro_mean_X", "tBodyGyro_mean_Y",
                       "tBodyGyro_mean_Z", "tBodyGyro_std_X",
                       "tBodyGyro_std_Y", "tBodyGyro_std_Z", "tBodyGyroJerk_mean_X",
                       "tBodyGyroJerk_mean_Y", "tBodyGyroJerk_mean_Z", 
                       "tBodyGyroJerk_std_X", "tBodyGyroJerk_std_Y",
                       "tBodyGyroJerk_std_Z", "tBodyAccMag_mean", "tBodyAccMag_std",
                       "tGravityAccMag_mean", "tGravityAccMag_std",
                       "tBodyAccJerkMag_mean", "tBodyAccJerkMag_std", 
                       "tBodyGyroMag_mean", "tBodyGyroMag_std",
                       "tBodyGyroJerkMag_mean", "tBodyGyroJerkMag_std",
                       "fBodyAcc_mean_X", "fBodyAcc_mean_Y", "fBodyAcc_mean_Z",
                       "fBodyAcc_std_X", "fBodyAcc_std_Y", "fBodyAcc_std_Z",
                       "fBodyAccJerk_mean_X", "fBodyAccJerk_mean_Y",
                       "fBodyAccJerk_mean_Z", "fBodyAccJerk_std_X",
                       "fBodyAccJerk_std_Y", "fBodyAccJerk_std_Z",
                       "fBodyGyro_mean_X", "fBodyGyro_mean_Y", "fBodyGyro_mean_Z",
                       "fBodyGyro_std_X", "fBodyGyro_std_Y", "fBodyGyro_std_Z",
                       "fBodyAccMag_mean", "fBodyAccMag_std",
                       "fBodyBodyAccJerkMag_mean", "fBodyBodyAccJerkMag_std",
                       "fBodyBodyGyroMag_mean", "fBodyBodyGyroMag_std",
                       "fBodyBodyGyroJerkMag_mean", "fBodyBodyGyroJerkMag_std")


## loop through 30 subjects creating rows for each subject/activity combination
## and appending each row to the t.data data frame
t.data <- data.frame(stringsAsFactors = FALSE)
for (i in 1:30) {
     
     ## selects rows with desired subject and activity
     l.data <- filter(ms.data, subject==i, activity=="laying")
     ## creates numeric vector of the average of each column removing subject/activity columns
     l.avgs <- colMeans(select(l.data, -(subject:activity)))
     ## converts numeric vector to data frame, t() switches names to columns instead of rows
     l.avgs <- as.data.frame(t(l.avgs))
     ## adds "Avg" to the front of each column name
     colnames(l.avgs) <- paste("Avg", colnames(l.avgs), sep = "_")
     ## returns the subject/activity columns back to the data frame/row
     l.avgs <- cbind(subject=i, activity="laying", l.avgs)
     ## combines this row with previous rows in a single data frame
     t.data <- rbind(t.data, l.avgs)
     
     sit.data <- filter(ms.data, subject==i, activity=="sitting")
     sit.avgs <- colMeans(select(sit.data, -(subject:activity)))
     sit.avgs <- as.data.frame(t(sit.avgs))
     colnames(sit.avgs) <- paste("Avg", colnames(sit.avgs), sep = "_")
     sit.avgs <- cbind(subject=i, activity="sitting", sit.avgs)
     t.data <- rbind(t.data, sit.avgs)
     
     stand.data <- filter(ms.data, subject==i, activity=="standing")
     stand.avgs <- colMeans(select(stand.data, -(subject:activity)))
     stand.avgs <- as.data.frame(t(stand.avgs))
     colnames(stand.avgs) <- paste("Avg", colnames(stand.avgs), sep = "_")
     stand.avgs <- cbind(subject=i, activity="standing", stand.avgs)
     t.data <- rbind(t.data, stand.avgs)
     
     w.data <- filter(ms.data, subject==i, activity=="walking")
     w.avgs <- colMeans(select(w.data, -(subject:activity)))
     w.avgs <- as.data.frame(t(w.avgs))
     colnames(w.avgs) <- paste("Avg", colnames(w.avgs), sep = "_")
     w.avgs <- cbind(subject=i, activity="walking", w.avgs)
     t.data <- rbind(t.data, w.avgs)
     
     wd.data <- filter(ms.data, subject==i, activity=="walking_down_stairs")
     wd.avgs <- colMeans(select(wd.data, -(subject:activity)))
     wd.avgs <- as.data.frame(t(wd.avgs))
     colnames(wd.avgs) <- paste("Avg", colnames(wd.avgs), sep = "_")
     wd.avgs <- cbind(subject=i, activity="walking_down_stairs", wd.avgs)
     t.data <- rbind(t.data, wd.avgs)
     
     wu.data <- filter(ms.data, subject==i, activity=="walking_up_stairs")
     wu.avgs <- colMeans(select(wu.data, -(subject:activity)))
     wu.avgs <- as.data.frame(t(wu.avgs))
     colnames(wu.avgs) <- paste("Avg", colnames(wu.avgs), sep = "_")
     wu.avgs <- cbind(subject=i, activity="walking_up_stairs", wu.avgs)
     t.data <- rbind(t.data, wu.avgs)
}

write.table(t.data, "tdata.txt", row.names=FALSE, sep=",")