##------------------------------------------------------------------------------
##Setting the Stage
##------------------------------------------------------------------------------
## Need this package to unzip the zipped data file
install.packages("downloader")

## Need this package to use the awesome dplyr functions like select, groupby etc
install.packages("dplyr")

## Need this package to use the tbl_dt function to convert a data frame into 
## a data table
install.packages("data.table")

## Load the necessary libraries

library(downloader)
library(dplyr)
library(data.table)

##------------------------------------------------------------------------------
## Extracting and reading the files
##------------------------------------------------------------------------------

## Unzip the downloaded zipped Data file

unzip("getdata-projectfiles-UCI HAR Dataset.zip")

## Read the 'Test' & 'Train' Data into data frames
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", quote="\"")

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", quote="\"")

## class(X_test)

## Read the 'Subject Data' into data frames

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                           quote="\"")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", 
                            quote="\"")

## Read the 'Activity' data into data frames

Y_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", quote="\"")

Y_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", quote="\"")

## Read the Column Lables data into a data frame

Features <- read.table("./UCI HAR Dataset/features.txt")

activities <- read.table("./UCI HAR Dataset/activity_labels.txt")


##------------------------------------------------------------------------------
## Transforming the dataset
##------------------------------------------------------------------------------

## Attach the 'Activity' columns to the 'Test' and 'Train' main data frames

X_test1 <- cbind(Y_test, X_test)

X_train1 <- cbind(Y_train, X_train)

## Attach the 'Subject' column to the 'Test' and 'Train main data frames

X_test2 <- cbind(subject_test, X_test1)

X_train2 <- cbind(subject_train, X_train1)

##------------------------------------------------------------------------------
## Merge the Testing and Training Data per Step 1 needed for the project
##------------------------------------------------------------------------------


X_data <- rbind(X_test2, X_train2)

## Adding on Column Names to this resultant data frame to make it readable

colnames(X_data) <- c("Subject", "Activity", as.character(Features$V2))

## Look at the data to see how it looks

## head(X_data)
## tail(X_data)

## Converting the data frame to a table for ease of operations

X <- tbl_df(X_data)

## Noticed that there are duplicate columns and removing them

colname1 <- colnames(X)
colname1 <- tbl_dt(colname1)

X <- X[, !duplicated(colname1)]


##------------------------------------------------------------------------------
## Find all columns that are Means or STD only as per Step 2 needed for the 
## project
## -----------------------------------------------------------------------------


X_skinny <- select (X, Subject, Activity, contains("mean"), contains("Mean"), 
                    contains("std"), contains("Std"))



##------------------------------------------------------------------------------
## Use descriptive Activity names to describe the activities in the Dataset per
## Step 3 needed for the project
## -----------------------------------------------------------------------------

Act_lbls <- tbl_df(activities)
colnames(Act_lbls) <- c("Activity", "ActivityDesc")

## Doing an inner join to get Activity descriptions

X_skinny1 <- merge(x = X_skinny, y = Act_lbls, by = "Activity", all.x=TRUE)

## Reorder columns

X_skinny2 <- select(X_skinny1, Subject, ActivityDesc, 3:88)

##------------------------------------------------------------------------------
## Renaming Columns to make them more descriptive per Step 4 for the Project
## -----------------------------------------------------------------------------

names(X_skinny2) <- gsub("tBody", "TimeBody", names(X_skinny2))

names(X_skinny2) <- gsub("fBody", "FreqBody", names(X_skinny2))

names(X_skinny2) <- gsub("tGravity", "TimeGravity", names(X_skinny2))

names(X_skinny2) <- gsub("BodyBody", "Body", names(X_skinny2))


##------------------------------------------------------------------------------
## Group by Activity and subject and find the mean of each column
## Needed for Step 5 of the project
## -----------------------------------------------------------------------------

X_skinny3 <- select(X_skinny2, ActivityDesc, Subject, 3:88)
X_Avgs <- X_skinny3 %>%
        group_by(ActivityDesc, Subject) %>%
        summarise_each(funs(mean), 3:88)

##------------------------------------------------------------------------------
## Finally write the output file
## -----------------------------------------------------------------------------

write.table(X_Avgs, file = "./X_Avgs.txt", row.names = FALSE) 
