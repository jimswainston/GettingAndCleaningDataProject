run_analysis <- function() {
  
  library(dplyr)

  ## read test data for merging
  
  x_test <- read.table("X_test.txt")
  features <- read.table("features.txt")
  activity_test <- read.table("y_test.txt")
  subject_test <- read.table("subject_test.txt")
  
  ## read train data for merging
  x_train <- read.table("X_train.txt")
  activity_train <- read.table("y_train.txt")
  subject_train <- read.table("subject_train.txt")
  
  ## A function to replace the activity code with the label
  replaceCodeForActivityLabels <- function(activityCode) {
    
    
    ## 'activityCode' is a number representing a type of acticity
    ## 1 WALKING
    ## 2 WALKING_UPSTAIRS
    ## 3 WALKING_DOWNSTAIRS
    ## 4 SITTING
    ## 5 STANDING
    ## 6 LAYING
    
    ## Return activity label
    
    switch(activityCode,
           invisible(print('WALKING')),
           invisible(print('WALKING_UPSTAIRS')),
           invisible(print('WALKING_DOWNSTAIRS')),
           invisible(print('SITTING')),
           invisible(print('STANDING')),
           invisible(print('LAYING')))
    
    
  }
  
  
  ## clean test data
  
  colnames(x_test) <- features[,2]  ## replace column names of x_test set with feature names
  activity_label <- invisible(sapply(activity_test[,1],replaceCodeForActivityLabels)) ## replace activity codes with labels
  dftest <- cbind(x_test,activity_label) ## bind activity labels to test data
  dftest <- cbind(dftest,subject_test) ## bind test data with subject data
  names(dftest)[563] = "subject_id"
  
  
  ## clean train data
  colnames(x_train) <- features[,2] ## replace column names of x_train set with feature names
  activity_label <- invisible(sapply(activity_train[,1],replaceCodeForActivityLabels)) ## replace activity codes with labels
  dftrain <- cbind(x_train,activity_label) ## bind activity labels to train data
  dftrain <- cbind(dftrain,subject_train) ## bind train data with subject data
  names(dftrain)[563] = "subject_id"
  
  ## stack the test and training data
  
  full_table <- rbind(dftest,dftrain)
  
  ## get indexes of columns that contain mean or standard deviation observations. Contains mean() or std() in the name.

  labels <- colnames(full_table)
  mean_cols <- grep("^.*mean\\(\\).*$",labels)
  std_cols <- grep("^.*std\\(\\).*$",labels)
  subj_act_cols <- c(562,563)
  select_columns <- sort(c(mean_cols,std_cols,subj_act_cols))
  
  ## reduce table so it only contains columns that contain mean() or std() in the name.
  select_table <- full_table[,select_columns]

  ## summarise each variable using mean. Group by each subject and activity. 
  grpdf <- group_by(select_table,subject_id,activity_label)
  x <-  summarise_each(grpdf,funs(mean))
  new_col_names <- paste(colnames(x[3:length(x)]),"-average", sep="") ## rename column names to reflect the data is now an average
  new_col_names <- c("subject_id","activity_label",new_col_names)
  colnames(x) <- new_col_names
  
  toreplace <- grep("^t",colnames(data))
  names(x)[toreplace] <- gsub("^t","time",names(x)[toreplace])
  
  toreplace <- grep("^f",colnames(data))
  names(x)[toreplace] <- gsub("^f","frequency",names(x)[toreplace])
  
  toreplace <- grep("Acc",colnames(data))
  names(x)[toreplace] <- gsub("Acc","Accelerometer",names(x)[toreplace])
  
  toreplace <- grep("Mag",colnames(data))
  names(x)[toreplace] <- gsub("Mag","Magnitude",names(x)[toreplace])
  
  toreplace <- grep("Gyro",colnames(data))
  names(x)[toreplace] <- gsub("Gyro","Gyroscope",names(x)[toreplace])
  
  tidy_data <- x
  write.table(tidy_data, file = "tidy_data.txt", row.name=FALSE)
  
}  
 



