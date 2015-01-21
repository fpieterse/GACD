# tidyData
#
# This function will create a tidy data set from the UCI HAR Dataset
# This function should be called from a working directory that contains the UCI
# HAR Dataset in a folder named "UCI HAR Dataset"
# The function will create a csv file with labeled columns.
# Extracted data include the mean and standard deviation of each measurement as
# well as the test subject's number and descriptive text for the activity
# performed
# Test- and Training data is combined into one file

tidyData <- function(filename="tidyData.csv")
{
    # TODO: check folders exist


    selectedFeatureNames <- .readSelectedFeatureNames()
    activities <- .readActivities()

    sep="\t"

    featureData <- .readFeatureFrame("test",selectedFeatureNames,activities)
    write.table(featureData,file=filename,sep=sep,append=FALSE,row.names=FALSE,col.names=TRUE)

    featureData <- .readFeatureFrame("train",selectedFeatureNames,activities)
    write.table(featureData,file=filename,sep=sep,append=TRUE,row.names=FALSE,col.names=FALSE)

}


# .readSelectedFeatureNames <<<
# Returns dataframe of selected features (mean and std) names and column numbers
# part of tidyData() function
.readSelectedFeatureNames <- function()
{
    # featureNames is table of feature names and column numbers in raw dataset
    featureNames <- read.table("UCI HAR Dataset/features.txt")
    names(featureNames) <- c("Column","FeatureName")

    # Select columns that contain the words "mean()" or "std()"
    selectedColumns <- NULL

    for (i in seq_len(nrow(featureNames)))
    {
        if ( grepl("^.*mean\\(\\).*",featureNames[i,"FeatureName"]) )
        {
            selectedColumns <- append(selectedColumns,featureNames[i,"Column"])
        }
        else if ( grepl("^.*std\\(\\).*",featureNames[i,"FeatureName"]) )
        {
            selectedColumns <- append(selectedColumns,featureNames[i,"Column"])
        }
    }

    featureNames[selectedColumns,]
}
# >>>

# .readActivites <<<
# Returns dataframe of descriptive text for activities and associated numbers
# Part of tidyData() function
.readActivities <- function()
{
    activities <- read.table("UCI HAR Dataset/activity_labels.txt")
    names(activities) <- c("Index","Activity")
    activities
}
# >>>

# .readFeatureFrame <<<
# This reads feature data into a data frame. This function is part of the
# tidyData() function but is exctracted into a function because it has to be
# done twice (for training and testing data).
.readFeatureFrame <- function(folder, selectedFeatureNames, activities)
{
    # Read test subject number
    filename = paste("UCI HAR Dataset/",folder,"/subject_",folder,".txt",sep="")
    testSubjects <- read.table(filename)
    names(testSubjects) <- c("Test Subject")

    # Read activity
    filename = paste("UCI HAR Dataset/",folder,"/y_",folder,".txt",sep="")
    actDF <- read.table(filename)
    names(actDF) <- "Activity"
    for (i in seq_len(nrow(activities)))
    {
        actNumber = activities[i,'Index']

        actDF[actDF$Activity == actNumber,] <- as.character(
            activities[i,'Activity'])
    }


    # Read data
    filename = paste("UCI HAR Dataset/",folder,"/X_",folder,".txt",sep="")
    df <- read.table(filename)
    df <- df[,selectedFeatureNames$Column]
    names(df) <- selectedFeatureNames$FeatureName

    df <- cbind(testSubjects,actDF,df)

    df
}
# >>>

# vim: foldmethod=marker: foldmarker=<<<,>>>
