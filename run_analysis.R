read_data <- function() {
    # Download file
    destfile <- "data.zip"
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = destfile)
    unzip(destfile)
    
    features <- read.table("UCI HAR Dataset/features.txt")
    
    ## Read test data:
    
    
    ## Subjects who performed the test
    test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
    colnames(test_subjects) <- c("subject_id")
    
    test_set <- read.table("UCI HAR Dataset/test/X_test.txt")
    colnames(test_set) <- features[,2]
    ## labels for activities
    test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")
    colnames(test_labels) <- c("activity")

    
    ## combine test data into one dataframe
    test_data <- cbind(test_subjects, test_set, test_labels)
    
    ## Read training data
    train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
    colnames(train_subjects) <- c("subject_id")
    
    train_set <- read.table("UCI HAR Dataset/train/X_train.txt")
    colnames(train_set) <- features[,2]
    
    ## labels for activities
    train_labels <- read.table("UCI HAR Dataset/train/y_train.txt")
    colnames(train_labels) <- c("activity")
    
    train_data <- cbind(train_subjects, train_set, train_labels)
    
    ## Combine test and train data into one dataframe
    rbind(test_data, train_data)
    
}

extract_mean_and_std_columns <- function(dataframe) {
    column_names <- colnames(dataframe)
    
    mean_cols <- sapply(column_names, function(x) grepl("mean()", x, fixed=TRUE))
    std_cols <- sapply(column_names, function(x) grepl("std()", x, fixed=TRUE))
    
    other <- sapply(column_names, function(x) x %in% c("subject_id", "activity")) 
    
    extracted <- dataframe[, (mean_cols | std_cols | other)]
    extracted
}

run_analysis <- function() {
    data <- read_data()
    extracted <- extract_mean_and_std_columns(data)
    
    activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
    colnames(activity_labels) <- c("activity_id", "activity_label")
    df <- merge(x = extracted, y = activity_labels, by.x = "activity", by.y = "activity_id")
    
    ## get the columns names for calculating means
    column_names <- colnames(df)
    
    mean_cols <- sapply(column_names, function(x) grepl("mean()", x, fixed=TRUE))
    std_cols <- sapply(column_names, function(x) grepl("std()", x, fixed=TRUE))
    tidy <- aggregate(df[, (mean_cols | std_cols)], by = list(df$subject_id, df$activity_label), FUN = mean)
    colnames(tidy)[1] <- "subject_id"
    colnames(tidy)[2] <- "activity"
    
    write.table(tidy, file = "tidy_dataset.txt", row.name = FALSE)
}