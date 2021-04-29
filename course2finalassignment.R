library(dplyr)

tst <- function() {
  
  ##This first section is loading all of the data from the directory. I have 
  ##renamed the features to columnheaders as this is easier for me to keep 
  ##track of.
  columnheaders <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
  activitylabels <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
  testx <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
  testy <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
  testsubject <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
  trainx <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
  trainy <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
  trainsubject <- read.table("./data/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
  
  
  ##This second block is renaming the columns of the data. For the test and 
  ##training data, it is renaming the columns based on the list of features 
  ##(which I have renamed to columnheaders$column). Most of this is 
  ##unnecessary, but makes it easier for to read later on.
  colnames(columnheaders) <- c("index", "column")
  colnames(activitylabels) <- c("activityid", "activitydescr")
  colnames(testsubject) <- c("subject")
  colnames(trainsubject) <- c("subject")
  colnames(testy) <- c("activityid")
  colnames(trainy) <- c("activityid")
  
  ##This final piece renames the columns of the "x" data sets with the 
  ##descriptive variable names, satisfying requirment 4
  colnames(testx) <- columnheaders$column
  colnames(trainx) <- columnheaders$column
  
  ##activity laebls to each y data set so that the description can be added
  ##to the combined data set later on
  testydescr <- left_join(testy, activitylabels, by = "activityid")
  trainydescr <- left_join(trainy, activitylabels, by = "activityid")
  
  ##selecting only the measurements on mean and standard deviation for each
  ##measurement. This satisfies requirement 2 of the assignment
  test <- select(testx, contains("mean()")|contains("std()"))
  train <- select(trainx, contains("mean()")|contains("std()"))
  
  ##Adding activity description to both test and training data to fulfill
  ##requirement 3 of the assignment. Also adding subject to test and training
  ##data sets which will be used later to fulfill requirement 5
  test <- cbind(test, activitydescr = testydescr$activitydescr)
  test <- cbind(test, subject = testsubject$subject)
  train <- cbind(train, activitydescr = trainydescr$activitydescr)
  train <- cbind(train, subject = trainsubject$subject)
  
  ##Combining the two data sets to satisfy requirement 1
  returnall <- union_all(test, train)
  
  ##Returning average of each variable grouped by subject and activity 
  ##satisfying requirement 5
  returnavg <- returnall %>% group_by(subject, activitydescr) %>% summarise(across(everything(), mean, .names = "mean_{.col}"))
  
  ##Wasn't sure what to do with the two tidy data sets, so I just moved them to
  ##the global environment.
  assign("returnall", returnall, envir = .GlobalEnv)
  assign("returnavg", returnavg, envir = .GlobalEnv)
  
  ##This outputs the data from step 5 into a text file in the directory
  write.table(returnavg, file = "./AssignmentStep5output.txt", row.names = FALSE)

}

