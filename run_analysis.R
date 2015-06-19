#   Loading dplyr package
library("dplyr")

#   Small utility function to load the test or training data set. 
#   !!! Important: It is supposed that the working directory is already pointing at "UCI HAR Dataset" folder. !!!
#       xPath: relative file path to the features data set
#       yPath: relative file path to the activities data set
#       sPath: relative file path to the subject list data set
#       featureNames: a list containing feature column names in a user-friendly format
loadDataSet <- function(xPath, yPath, sPath, featureNames){
    setX <- read.table(xPath, header = F)
    setY <- read.table(yPath, header = F)
    subjectId <- read.table(sPath, header = F)
    
    frame <- data.frame(subject = subjectId, X = setX, y = setY)
    names(frame) <- c("subject", featureNames, "activity")  #   Assignment #4: labels the data set with descriptive variable names. 
    
    rm(setX, setY, subjectId)
    
    frame
}

#   Loads the feature names from "features.txt" and applies some regex to clean it and make it more readable.
featureNames <- read.table("features.txt", header = F)
featureNames <- sapply(featureNames$V2, function(s){
    #   Keeps alpha characters and numbers only. Replace other characters by "_".
    tmp <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_",s, ignore.case = T), ignore.case = T)
    #   Replace abbreviations by full words.
    tmp <- gsub("^t", "time_domain_", tmp)
    tmp <- gsub("^f", "frequency_domain_", tmp)
    tmp <- gsub("Acc", "_acceleration", tmp)
    tmp <- gsub("_std", "_standard_deviation", tmp)
    tmp <- gsub("Jerk", "_jerk", tmp)
    tmp <- gsub("Gyro", "_gyroscope", tmp)
    tmp <- gsub("Mag", "_magnitude", tmp)
    tmp <- gsub("(X|Y|Z)$", "\\1_axis", tmp)
    tolower(tmp)
})

#   Loads the activity labels from "activity_labels.txt"
activityLbl <- read.table("activity_labels.txt", header = F)

#   Loads the test and training data set from original files.
trainFrame <- loadDataSet("train/X_train.txt", "train/y_train.txt", "train/subject_train.txt", featureNames)
testFrame <- loadDataSet("test/X_test.txt", "test/y_test.txt", "test/subject_test.txt", featureNames)

mainDataSet <- bind_rows(testFrame, trainFrame) %>%     #   Assignment #1: merges the two data sets
    select(subject, matches("_mean_|_mean$|_standard_deviation_|_standard_deviation$"), activity) %>%   #   Assignment #2: selects only mean and standard deviation columns from feature measurements.
    mutate(activity = factor(activity, labels=activityLbl$V2))      #   Assignment #3: Replaces numerical activity codes by readable labels.

#   Assignment #5: Generates an independent tidy data set with the average of each variable for each activity and each subject.
summarizedDataSet <- mainDataSet %>% group_by(subject, activity) %>% summarise_each(funs(mean))

#   Cleans up the temporary variables.
rm(testFrame, trainFrame, featureNames, activityLbl)

#   Saves data sets in raw text files.
write.table(mainDataSet, "mainDataSet.txt", row.names = F)
write.table(summarizedDataSet, "summarizedDataSet.txt", row.names = F)
