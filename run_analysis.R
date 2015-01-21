# When sourced, this will create a tidy dataset using the functions in
# tidyData.R

source('tidyData.R')

data <- tidyData()


subjects <- unique(data$TestSubject)
activities <- unique(data$Activity)

# New dataset will have same column names
columnNames <- names(data)

# Create a blank column to hold an average value for each subject for each
# activity
blankColumn <- rep(NA,length(subjects)*length(activities))

df <- data.frame(blankColumn)
names(df) <- c("TestSubject")
for (i in seq(2,length(columnNames)))
{
    df[,columnNames[i]] <- blackColumn
}

i = 1
for (subject in subjects)
{
    correctSubject <- data$TestSubject == subject
    for (activity in activities)
    {
        correctActivity <- data$Activity == activity
        temp <- data[correctSubject & correctActivity,]

        df$TestSubject[i] <- subject
        df$Activity[i] <- activity

        for (m in columnNames[3:length(columnNames)])
        {
            df[i,m] <- mean(temp[,m])
        }

        i = i+1

    }
}


write.table(df,file="tidyData.txt",sep="\t",row.names=F)
